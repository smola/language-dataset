import json
import logging
import multiprocessing
import os
import subprocess

from .common import *


logger = multiprocessing.log_to_stderr()
logger.setLevel(logging.DEBUG)


def chunks(lst, n):
    for i in range(0, len(lst), n):
        yield lst[i : i + n]


def classify_linguist(group):
    meta = Meta()
    full_path_map = {os.path.join("data", p): p for p in group.keys()}

    linguist_results = subprocess.run(
        ["ruby", "tools/linguist_wrapper.rb"] + list(full_path_map.keys()),
        shell=False,
        check=True,
        capture_output=True,
    )
    linguist_results = linguist_results.stdout.decode("utf-8").strip()
    linguist_results = json.loads(linguist_results)

    results = {}
    for full_path, linguist_result in linguist_results.items():
        path = full_path_map[full_path]
        ann = group[path]
        results[path] = ann
        for strategy, languages in linguist_result.items():
            key = f"linguist-{strategy}" if strategy != "all" else "linguist"
            if len(languages) != 1:
                if key in ann:
                    del ann[key]
                continue
            language: str = languages[0]
            language = meta.to_normalized_language(dataset="linguist", lang=language)
            ann[key] = language
    return results


def classify_linguist_all(dataset):
    chunk_size = 20
    inputs = []
    for keys_group in chunks(list(dataset.data["files"].keys()), chunk_size):
        d = {k: dataset.data["files"][k]["annotations"] for k in keys_group}
        inputs.append(d)
    with multiprocessing.Pool() as p:
        update_groups = p.map(classify_linguist, inputs)
    for group in update_groups:
        for path, ann in group.items():
            dataset.data["files"][path]["annotations"] = ann


def main():
    dataset = Dataset()
    classify_linguist_all(dataset)
    dataset.save()


if __name__ == "__main__":
    main()
