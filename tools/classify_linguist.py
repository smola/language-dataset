import subprocess
import multiprocessing
import logging
import os.path
import re
import shutil


logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger("classify_linguist")

from .common import *


def classify_linguist(path):
    if path.startswith("data/"):
        path = path[len("data/") :]

    repo = "/".join(path.split("/")[1:3])
    commit = path.split("/")[3]
    path = "/".join(path.split("/")[4:])

    tmp_repo = clone_tmp_repo(repo, commit)
    try:
        # XXX: hack to avoid gem not found error in repositories using .bundle/config.
        bundle_config_path = os.path.join(tmp_repo, ".bundle", "config")
        if os.path.exists(bundle_config_path):
            os.remove(bundle_config_path)

        result = subprocess.run(
            "github-linguist %s" % path,
            cwd=tmp_repo,
            shell=True,
            check=True,
            capture_output=True,
        )

    finally:
        shutil.rmtree(tmp_repo)

    result = result.stdout.decode("utf-8").strip()
    language = re.findall(r"language:\s+(.*)", result)
    return language


def main():
    meta = Meta()
    dataset = Dataset()

    files = list(dataset.data["files"].keys())

    with multiprocessing.Pool() as p:
        languages = p.map(classify_linguist, [os.path.join("data", f) for f in files])

    for f, l in zip(files, languages):
        norm_lang = meta.to_normalized_language(dataset="linguist", lang=l)
        dataset.data["files"][f]["annotations"]["linguist"] = norm_lang

    dataset.save()


if __name__ == "__main__":
    multiprocessing.log_to_stderr(logging.DEBUG)
    main()
