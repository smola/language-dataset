from .common import *
from .gen_meta import *


def test_add_linguist_languages():
    meta = Meta()
    add_linguist_languages(LINGUIST_COMMIT, meta)
    assert "linguist" in meta.data["datasets"]
    assert len(meta.data["languages"]) > 100


def test_add_rosetta_code_languages():
    meta = Meta()
    add_rosetta_code_languages(ROSETTA_CODE_DATA_COMMIT, meta)
    assert "rosetta_code" in meta.data["datasets"]
    assert len(meta.data["languages"]) > 100
