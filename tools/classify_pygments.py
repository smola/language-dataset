import subprocess
import multiprocessing
import logging
import os.path

import pygments.util
import pygments.lexers

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger("classify_linguist")

from .common import *


def classify_pygments(path):
    with open(path, "rb") as f:
        data = f.read()

    try:
        return pygments.lexers.guess_lexer_for_filename(path, data).name
    except pygments.util.ClassNotFound:
        return None


def main():
    meta = Meta()
    dataset = Dataset()

    files = list(dataset.data["files"].keys())

    with multiprocessing.Pool() as p:
        languages = p.map(classify_pygments, [os.path.join("data", f) for f in files])

    for f, l in zip(files, languages):
        l1 = l
        if l1:
            norm_lang = meta.to_normalized_language(dataset="pygments", lang=l1)
            dataset.data["files"][f]["annotations"]["pygments-filename"] = norm_lang
        else:
            if "pygments-filename" in dataset.data["files"][f]["annotations"]:
                del dataset.data["files"][f]["annotations"]["pygments-filename"]

    dataset.save()


if __name__ == "__main__":
    multiprocessing.log_to_stderr(logging.DEBUG)
    main()
