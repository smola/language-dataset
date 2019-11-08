import json
import multiprocessing
import os
import os.path
import random
import requests
import shutil
import subprocess
import yaml

import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger("harvest")

from .common import *

if "GITHUB_TOKEN" in os.environ:
    github_headers = {"Authorization": "bearer " + os.environ["GITHUB_TOKEN"]}
else:
    github_headers = {}


def github_query(query):
    response = requests.post(
        "https://api.github.com/graphql", json={"query": query}, headers=github_headers
    )
    response.raise_for_status()
    return response.json()


query_repos_per_lang = """
{
  search(query: "language:%s sort:stars is:public archived:false fork:false", type: REPOSITORY, first: %d) {
    edges {
      node {
        ... on Repository {
          nameWithOwner
        }
      }
    }
  }
}
"""


def search_repos(lang, n):
    result = github_query(query_repos_per_lang % (lang, n))
    edges = result["data"]["search"]["edges"]
    repos = [r["node"]["nameWithOwner"] for r in edges]
    return repos[:n]


def get_commit_hash(repo_path):
    result = subprocess.run(
        "git rev-parse HEAD", cwd=repo_path, shell=True, capture_output=True, check=True
    )
    return result.stdout.decode("utf-8").strip()


def scan_path_for_lang(path, linguist_langs):
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


def scan_repo_for_lang(repo, linguist_langs):
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


def try_process_repo(meta, dataset, lock, repo, lang):
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


def _worker(meta, dataset, lock, q):
    while True:
        job = q.get()
        if not job:
            return
        (lang, n) = job
        logger.info("processing %s" % lang)
        repos = search_repos(lang, 100)
        seen_repos = get_seen_repos()
        repos = [r for r in repos if r not in seen_repos]
        for repo in repos:
            if not n:
                break
            if try_process_repo(meta, dataset, lock, repo, lang):
                n -= 1
        if n:
            logger.info("cannot find enough repositories for %s" % lang)
        else:
            logger.info("got enough samples for %s" % lang)


def count_missing_samples(meta: Meta, dataset: Dataset, max_samples: int):
    missing_samples = {}
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


def get_seen_repos(dataset: Dataset = None):
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
    meta: Meta, dataset: Dataset, max_samples=SAMPLES_PER_LANGUAGE, n_jobs=-1
):
    needed_samples = count_missing_samples(meta, dataset, max_samples)
    seen_repos = get_seen_repos()

    if n_jobs == -1:
        n_jobs = multiprocessing.cpu_count()

    q = multiprocessing.Queue()
    lock = multiprocessing.Lock()
    workers = []
    for n in range(1, n_jobs + 1):
        p = multiprocessing.Process(
            name="harvest-worker-%d" % n, target=_worker, args=(meta, dataset, lock, q)
        )
        p.start()
        workers.append(p)
    jobs = list(needed_samples.items())
    random.shuffle(jobs)
    for lang, n in jobs:
        if needed_samples == 0:
            logger.info("got enough samples for %s" % lang)
            continue
        q.put((lang, n))

    q.close()
    for p in workers:
        p.join()


def main():
    meta = Meta()
    dataset = Dataset()
    fill_samples(
        meta=meta, dataset=dataset, max_samples=SAMPLES_PER_LANGUAGE, n_jobs=-1
    )


if __name__ == "__main__":
    multiprocessing.log_to_stderr(logging.DEBUG)
    main()
