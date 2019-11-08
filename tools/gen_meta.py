#!/usr/bin/env python3

import copy
import requests
import shutil
from typing import Sequence
import yaml

import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger("gen_meta")

from .common import *

LINGUIST_COMMIT = "10c20c7286a4b56c17253e8aab044debfe9f0dbe"
ROSETTA_CODE_DATA_COMMIT = "aac6731f2c1e30321fcfc58ac95d8203c041ee04"


def add_linguist_languages(commit: str, meta: Meta):
    meta.add_dataset(name="linguist", data={"version": commit,})

    norm_langs = {
        "PLSQL": "PL/SQL",
        "PLpgSQL": "PL/pgSQL",
    }

    langs = get_linguist_languages(commit=commit)
    for lang in langs:
        norm_lang = norm_langs.get(lang, lang)
        meta.add_language(dataset="linguist", norm_lang=norm_lang, lang=lang)


def get_linguist_languages(commit: str) -> Sequence[str]:
    logger.info("loading linguist languages.yml for commit %s" % commit)
    url = (
        "https://raw.githubusercontent.com/github/linguist/%s/lib/linguist/languages.yml"
        % commit
    )
    response = requests.get(url)
    response.raise_for_status()
    data = load_yaml_from_steam(response.content.decode("utf-8"))
    return [l for l in data.keys()]


def add_rosetta_code_languages(commit: str, meta: Meta):
    dataset_name = "rosetta_code"
    meta.add_dataset(name=dataset_name, data={"version": commit,})

    norm_langs = {
        "AWK": "Awk",
        "Batchfile": "Batchfile",
        "Brainf***": "Brainfuck",
        "C sharp": "C#",
        "EC": "eC",
        "F Sharp": "F#",
        "Fish": "fish",
        "lilypond": "LilyPond",
        "Make": "Makefile",
        "MoonScript": "moonscript",
        "NewLISP": "NewLisp",
        "OOC": "ooc",
        "Openscad": "OpenSCAD",
        "POV-Ray": "POV-Ray SDL",
        "Powerbuilder": "PowerBuilder",
        "Q": "q",
        "REBOL": "Rebol",
        "Sed": "sed",
        "Vim Script": "Vim script",
        "XSLT 1.0": "XSLT",
        "XSLT 2.0": "XSLT",
        "Object Pascal": "Pascal",
        "Delphi": "Pascal",
        "Free Pascal": "Pascal",
        "Visual Basic .NET": "Visual Basic",
        "VBA": "Visual Basic",
        "VBScript": "Visual Basic",
    }

    langs = get_rosetta_code_languages(commit=commit)
    for lang in langs:
        norm_lang = norm_langs.get(lang, lang)
        meta.add_language(dataset=dataset_name, norm_lang=norm_lang, lang=lang)


def get_rosetta_code_languages(commit: str) -> Sequence[str]:
    logger.info("loading rosetta_code languages for commit %s" % commit)
    tmp_dir = clone_tmp_repo("acmeism/RosettaCodeData", commit=commit)

    langs = load_yaml(os.path.join(tmp_dir, "Meta", "Lang.yaml"))
    langs = {k: v["path"] for k, v in langs.items()}

    def _has_rosetta_code_samples(tmp_dir, lang):
        return len(os.listdir(os.path.join(tmp_dir, "Lang", lang))) > 2

    langs = [l for l, p in langs.items() if _has_rosetta_code_samples(tmp_dir, p)]
    shutil.rmtree(tmp_dir)

    return langs


def main():
    meta = Meta()
    add_linguist_languages(LINGUIST_COMMIT, meta)
    add_rosetta_code_languages(ROSETTA_CODE_DATA_COMMIT, meta)
    meta.save()


if __name__ == "__main__":
    main()
