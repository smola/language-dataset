import github
import json
import multiprocessing
import os
import os.path
import random
import shutil
import subprocess
import sys
import typing as T

import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger("harvest")

from .common import (
    Meta,
    Dataset,
    clone_tmp_repo,
    MAX_SAMPLE_SIZE,
    MIN_SAMPLE_SIZE,
    SAMPLES_PER_LANGUAGE,
)

GITHUB_TOKEN = os.environ.get("GITHUB_TOKEN")

if not GITHUB_TOKEN:
    gh = github.Github()
else:
    gh = github.Github(GITHUB_TOKEN)


def search_repos(
    lang: str,
    n: int,
    query_extra: T.Optional[str] = None,
    seen_repos: T.Optional[T.Collection[str]] = None,
) -> T.List[str]:
    query = f'language:"{lang}"'
    seen_repos = set(seen_repos) if seen_repos else set()
    if query_extra:
        query += f" {query_extra}"
    query += f" NOT nothack{random.randint(0, 1000)}"
    logger.info(f"search {n} repos for {lang} with query: {query}")
    results = gh.search_code(query=query)
    found_repos: T.List[str] = []
    for result in results:
        if len(found_repos) >= n:
            break
        repo = result.repository.full_name
        if repo in seen_repos:
            continue
        # Filter out some frequent repos with bad samples
        if "linguist" in repo or "PL_recognizer" in repo:
            continue
        seen_repos.add(repo)
        found_repos.append(repo)
    return found_repos


def get_commit_hash(repo_path: str) -> str:
    result = subprocess.run(
        "git rev-parse HEAD", cwd=repo_path, shell=True, capture_output=True, check=True
    )
    return result.stdout.decode("utf-8").strip()


def scan_path_for_lang(path: str, linguist_langs: T.Sequence[str]) -> T.List[str]:
    # XXX: hack to avoid gem not found error in repositories using .bundle/config.
    bundle_config_path = os.path.join(path, ".bundle", "config")
    if os.path.exists(bundle_config_path):
        os.remove(bundle_config_path)

    result = subprocess.run(
        "github-linguist --json", cwd=path, shell=True, check=True, capture_output=True
    )
    result = result.stdout.decode("utf-8").strip()
    result = json.loads(result)
    files = []
    for lang in linguist_langs:
        files += result.get(lang, [])
    return files


def scan_repo_for_lang(repo: str, linguist_langs: T.Sequence[str]) -> T.List[str]:
    logger.info("scanning %s for %s" % (repo, linguist_langs))
    tmp_dir = clone_tmp_repo(repo)
    commit_hash = get_commit_hash(tmp_dir)
    paths = scan_path_for_lang(tmp_dir, linguist_langs)
    paths_with_sizes = {p: os.path.getsize(os.path.join(tmp_dir, p)) for p in paths}
    paths = [p for p in paths if paths_with_sizes[p] >= MIN_SAMPLE_SIZE]
    paths = [p for p in paths if paths_with_sizes[p] <= MAX_SAMPLE_SIZE]
    random.shuffle(paths)
    added_files = []
    for path in paths[:1]:
        src_path = os.path.join(tmp_dir, path)
        bare_dst_path = os.path.join("github.com", repo, commit_hash, path)
        dst_path = os.path.join("data", bare_dst_path)
        dir_path = os.path.dirname(dst_path)
        logger.info("copy %s to %s" % (src_path, dst_path))
        os.makedirs(dir_path, exist_ok=True)
        shutil.copy(src_path, dst_path)
        added_files.append(bare_dst_path)
    shutil.rmtree(tmp_dir)
    if not added_files:
        logger.info("no suitable files for %s in %s" % (linguist_langs, repo))
    return added_files


def try_process_repo(
    meta: Meta, dataset: Dataset, lock: multiprocessing.Lock, repo: str, lang: str
) -> bool:
    linguist_langs = meta.to_dataset_languages(dataset="linguist", norm_lang=lang)
    added_files = scan_repo_for_lang(repo, linguist_langs)
    if not added_files:
        return False
    lock.acquire()
    dataset.load()
    for file in added_files:
        if file not in dataset.data["files"]:
            dataset.data["files"][file] = {"annotations": {}}
        dataset.data["files"][file]["annotations"]["linguist"] = lang
    dataset.save()
    lock.release()
    return True


def _worker(
    meta: Meta,
    dataset: Dataset,
    lock: multiprocessing.Lock,
    q_req: multiprocessing.Queue,
    q_resp: multiprocessing.Queue,
) -> None:
    while True:
        req = q_req.get()
        if not req:
            return
        repo, lang = req
        logger.info(f"processing {repo} for {lang}")
        if try_process_repo(meta, dataset, lock, repo, lang):
            q_resp.put((repo, lang, True))
        else:
            q_resp.put((repo, lang, False))


def count_missing_samples(
    meta: Meta, dataset: Dataset, max_samples: int
) -> T.Dict[str, int]:
    missing_samples: T.Dict[str, int] = {}
    for norm_lang in meta.get_normalized_languages():
        if not meta.to_dataset_languages("linguist", norm_lang):
            continue
        missing_samples[norm_lang] = max_samples

    for path, file_data in dataset.data["files"].items():
        ann = file_data["annotations"]
        if "linguist" not in ann:
            continue
        lang = ann.get("vote", ann["linguist"])
        if lang not in missing_samples:
            continue
        missing_samples[lang] = missing_samples[lang] - 1

    return missing_samples


def get_seen_repos(dataset: T.Optional[Dataset] = None):
    if not dataset:
        dataset = Dataset()
    seen = set([])
    for path in dataset.data["files"].keys():
        if not path.startswith("github.com/"):
            continue
        repo = "/".join(path.split("/")[1:3])
        seen.add(repo)
    return seen


def fill_samples(
    meta: Meta,
    dataset: Dataset,
    max_samples=SAMPLES_PER_LANGUAGE,
    n_jobs=-1,
    filter_lang: T.Optional[str] = None,
    query_extra: T.Optional[str] = None,
):
    if n_jobs == -1:
        n_jobs = multiprocessing.cpu_count()

    q_req = multiprocessing.Queue(maxsize=20 * 400)
    q_resp = multiprocessing.Queue(maxsize=20 * 400)
    lock = multiprocessing.Lock()
    workers = []
    for n in range(1, n_jobs + 1):
        p = multiprocessing.Process(
            name="harvest-worker-%d" % n,
            target=_worker,
            args=(meta, dataset, lock, q_req, q_resp),
        )
        p.start()
        workers.append(p)

    # Search repos
    needed_samples = count_missing_samples(meta, dataset, max_samples)
    if filter_lang:
        needed_samples = {filter_lang: needed_samples[filter_lang]}
    seen_repos = get_seen_repos()
    all_repos = {}
    for lang, n_need in needed_samples.items():
        if not n_need:
            continue
        all_repos[lang] = search_repos(
            lang=lang, n=n_need * 5, query_extra=query_extra, seen_repos=seen_repos
        )
        random.shuffle(all_repos[lang])

    while True:
        n_added = 0
        for lang, n_need in needed_samples.items():
            if not n_need:
                continue
            repos = [r for r in all_repos[lang] if r not in seen_repos]
            repos_next, repos_remaining = repos[:n_need], repos[n_need:]
            logger.info(f"trying to get {len(repos_next)} repos for {lang}")
            for repo in repos_next:
                seen_repos.add(repo)
                q_req.put((repo, lang))
                n_added += 1
        if n_added == 0:
            logger.info("no more jobs to add")
            break
        for _ in range(n_added):
            repo, lang, ok = q_resp.get()
            if ok:
                needed_samples[lang] -= 1
    q_req.close()
    q_resp.close()
    for p in workers:
        p.join()


def main():
    meta = Meta()
    dataset = Dataset()

    filter_lang: T.Optional[str] = None
    query_extra: T.Optional[str] = None
    if len(sys.argv) == 1:
        logger.info("running for all languages")
    else:
        if len(sys.argv) >= 2:
            filter_lang = sys.argv[1]
            all_langs = meta.get_normalized_languages()
            if filter_lang not in all_langs:
                logger.error(
                    f"invalid language '{filter_lang}', valid languages: {all_langs}"
                )
                sys.exit(1)
            logger.info(f"running for language {filter_lang}")
        if len(sys.argv) >= 3:
            query_extra = sys.argv[2]
            logger.info(f"running with extra query filter: {query_extra}")

    fill_samples(
        meta=meta,
        dataset=dataset,
        max_samples=SAMPLES_PER_LANGUAGE,
        n_jobs=-1,
        filter_lang=filter_lang,
        query_extra=query_extra,
    )


if __name__ == "__main__":
    multiprocessing.log_to_stderr(logging.DEBUG)
    if not GITHUB_TOKEN:
        logger.error("GITHUB_TOKEN is not set")
        sys.exit(1)
    main()
