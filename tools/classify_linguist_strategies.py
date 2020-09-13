import json
import logging
import multiprocessing
import os
import subprocess

from .common import *

logger = multiprocessing.log_to_stderr()
logger.setLevel(logging.DEBUG)


def classify_linguist_strategies(path: str, ann):
    meta = Meta()
    full_path = os.path.join("data", path)
    result = subprocess.run(
        ["ruby", "tools/linguist_wrapper.rb", full_path],
        shell=False,
        check=True,
        capture_output=True,
    )
    result = result.stdout.decode("utf-8").strip()
    result = json.loads(result)
    for strategy, languages in result.items():
        if len(languages) != 1:
            continue
        language: str = languages[0]
        language = meta.to_normalized_language(dataset="linguist", lang=language)
        ann[f"linguist-{strategy}"] = language
    return ann


def _job(args):
    return (args[0], classify_linguist_strategies(args[0], args[1]))


def classify_linguist_strategies_all(dataset):
    inputs = [(p, d["annotations"]) for p, d in dataset.data["files"].items()]
    with multiprocessing.Pool() as p:
        updates = p.map(_job, inputs)
    for path, ann in updates:
        dataset.data["files"][path]["annotations"] = ann


def main():
    dataset = Dataset()
    classify_linguist_strategies_all(dataset)
    dataset.save()


if __name__ == "__main__":
    main()
