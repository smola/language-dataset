#!/usr/bin/env python3

import copy
import requests
import shutil
from typing import Optional, Sequence, Tuple
import yaml

import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger("gen_meta")

from .common import *

LINGUIST_COMMIT = "3ef386bbb7d10afe2ef81d85c9911871a9f2c221"
ROSETTA_CODE_DATA_COMMIT = "aac6731f2c1e30321fcfc58ac95d8203c041ee04"


def add_linguist_languages(commit: str, meta: Meta):
    meta.add_dataset(
        name="linguist",
        data={
            "version": commit,
        },
    )

    norm_langs = {
        "Cuda": "CUDA",
        "Fortran Free Form": "Fortran",  # TODO: We might want to split this.
        "JSONLD": "JSON-LD",
        "PLSQL": "PL/SQL",
        "PLpgSQL": "PL/pgSQL",
        "Mathematica": "Wolfram Language",
        "Moocode": "MOO",
        "mupad": "MuPAD",
        "prolog": "Prolog",
    }

    langs = get_linguist_languages(commit=commit)
    for group, lang in langs:
        norm_lang = norm_langs.get(lang, lang)
        norm_group = norm_langs.get(group, group)
        meta.add_language(
            dataset="linguist", norm_lang=norm_lang, lang=lang, group=norm_group
        )


def get_linguist_languages(commit: str) -> Sequence[Tuple[Optional[str], str]]:
    logger.info("loading linguist languages.yml for commit %s" % commit)
    url = (
        "https://raw.githubusercontent.com/github/linguist/%s/lib/linguist/languages.yml"
        % commit
    )
    response = requests.get(url)
    response.raise_for_status()
    data = load_yaml_from_steam(response.content.decode("utf-8"))
    return [(data[l].get("group"), l) for l in data.keys()]


def add_rosetta_code_languages(commit: str, meta: Meta):
    dataset_name = "rosetta_code"
    meta.add_dataset(
        name=dataset_name,
        data={
            "version": commit,
        },
    )

    norm_langs = {
        "386 Assembly": "Assembly",
        "6502 Assembly": "Assembly",
        "6800 Assembly": "Assembly",
        "8051 Assembly": "Assembly",
        "8080 Assembly": "Assembly",
        "8086 Assembly": "Assembly",
        "ARM Assembly": "Assembly",
        "AWK": "Awk",
        "Batch File": "Batchfile",
        "Brainf***": "Brainfuck",
        "Bc": "bc",
        "C sharp": "C#",
        "Computer/zero Assembly": "Assembly",
        "EC": "eC",
        "F Sharp": "F#",
        "Fish": "fish",
        "FRISC Assembly": "Assembly",
        "Friendly interactive shell": "fish",
        "GML": "Game Maker Language",
        "LC3 Assembly": "Assembly",
        "lilypond": "LilyPond",
        "Lilypond": "LilyPond",
        "Make": "Makefile",
        "Mathematica": "Wolfram Language",
        "MIPS Assembly": "Assembly",
        "MIRC Scripting Language": "mIRC Script",
        "Moonscript": "MoonScript",
        "MySQL": "SQL",
        "68000 Assembly": "Motorola 68K Assembly",
        "NewLISP": "NewLisp",
        "OOC": "ooc",
        "Openscad": "OpenSCAD",
        "PDP-11 Assembly": "Assembly",
        "Perl 6": "Raku",
        "POV-Ray": "POV-Ray SDL",
        "Powerbuilder": "PowerBuilder",
        "Q": "q",
        "REBOL": "Rebol",
        "Sed": "sed",
        "SoneKing Assembly": "Assembly",
        "SPARC Assembly": "Assembly",
        "TI-83 Hex Assembly": "Assembly",
        "Transact-SQL": "TSQL",
        "Vim Script": "Vim script",
        "XSLT 1.0": "XSLT",
        "XSLT 2.0": "XSLT",
        "Object Pascal": "Pascal",
        "Delphi": "Pascal",
        "Free Pascal": "Pascal",
        "VAX Assembly": "Assembly",
        "X86 Assembly": "Assembly",
        "Z80 Assembly": "Assembly",
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


def add_pygments_languages(meta: Meta):
    import pygments
    import pygments.lexers

    dataset_name = "pygments"
    meta.add_dataset(
        name=dataset_name,
        data={
            "version": pygments.__version__,
        },
    )

    norm_langs = {
        "ActionScript 3": "ActionScript",
        "Ampl": "AMPL",
        "autohotkey": "AutoHotkey",
        "aspx-cs": "ASP.NET",
        "aspx-vb": "ASP.NET",
        "Base Makefile": "Makefile",
        "Bash": "Shell",
        "Bash Session": "ShellSession",
        "BBC Basic": "BBC BASIC",
        "BC": "bc",
        "c-objdump": "C-ObjDump",
        "cpp-objdump": "Cpp-ObjDump",
        "Coldfusion CFC": "ColdFusion CFC",
        "d-objdump": "D-ObjDump",
        "DASM16": "Assembly",
        "Delphi": "Pascal",
        "Docker": "Dockerfile",
        "EmacsLisp": "Emacs Lisp",
        "Fish": "fish",
        "FortranFixed": "Fortran",  # TODO
        "FoxPro": "xBase",
        "GAS": "Unix Assembly",
        "Hxml": "HXML",
        "LessCss": "Less",
        "liquid": "Liquid",
        "markdown": "Markdown",
        "Mathematica": "Wolfram Language",
        "Matlab": "MATLAB",
        "MySQL": "SQL",
        "Nginx configuration file": "Nginx",
        "MOOCode": "MOO",
        "objdump": "ObjDump",
        "ODIN": "Odin",
        "Ooc": "ooc",
        "Perl6": "Raku",
        "Pig": "PigLatin",
        "PostgreSQL SQL dialect": "SQL",
        "POVRay": "POV-Ray SDL",
        "Properties": "Java Properties",
        "Python 2.x": "Python",
        "Python console session": "Python console",
        "Python Traceback": "Python traceback",
        "Python 2.x Traceback": "Python traceback",
        "ReasonML": "Reason",
        "REBOL": "Rebol",
        "Rexx": "REXX",
        "RHTML": "HTML+ERB",
        "RPMSpec": "RPM Spec",
        "systemverilog": "SystemVerilog",
        "Transact-SQL": "TSQL",
        "verilog": "Verilog",
        "VB.net": "Visual Basic .NET",
        "vhdl": "VHDL",
        "Web IDL": "WebIDL",
        "Text only": "Text",
    }

    lexers = pygments.lexers.get_all_lexers()
    for lex in lexers:
        lang = lex[0]
        norm_lang = norm_langs.get(lang, lang)
        if norm_lang.startswith("ANTLR With"):
            norm_lang = "ANTLR"
        if norm_lang.startswith("Ragel in"):
            norm_lang = "Ragel"
        meta.add_language(dataset=dataset_name, norm_lang=norm_lang, lang=lang)


def add_custom_languages(meta: Meta):
    dataset_name = "custom"
    meta.add_dataset(
        name=dataset_name,
        data={},
    )

    langs = [
        "KoLMafia ASH",
    ]

    for lang in langs:
        meta.add_language(dataset=dataset_name, norm_lang=lang, lang=lang)

    meta.add_language(
        dataset="linguist",
        norm_lang="Literate Idris",
        lang="Literate Idris",
        group="Idris",
    )


def main():
    meta = Meta(load=False)
    add_linguist_languages(LINGUIST_COMMIT, meta)
    add_rosetta_code_languages(ROSETTA_CODE_DATA_COMMIT, meta)
    add_pygments_languages(meta)
    add_custom_languages(meta)
    meta.save()


if __name__ == "__main__":
    main()
