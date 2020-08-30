import os
import os.path
import pytest

from .harvest import *


def test_count_missing_samples():
    meta = Meta(path="non-existent")
    meta.add_language("linguist", "Lang1", "XXX1")
    meta.add_language("linguist", "Lang2", "XXX2")
    dataset = Dataset(path="non-existent")
    dataset.data["files"]["file1"] = {
        "annotations": {
            "linguist": "Lang1",
        }
    }
    missing_samples = count_missing_samples(meta, dataset, 10)
    assert missing_samples == {
        "Lang1": 9,
        "Lang2": 10,
    }


def test_scan_repo_for_lang(mocker):
    mocker.patch(target="os.makedirs")
    copy_stub = mocker.stub(name="shutil.copy")
    mocker.patch(target="shutil.copy", new=copy_stub)
    files = scan_repo_for_lang("git-fixtures/basic", ["PHP"])
    assert len(files) == 1
    assert files[0].endswith("/crappy.php")
    assert copy_stub.call_count == 1
    assert not os.path.exists("tmp/git-fixtures/basic")


def test_search_repos():
    if "GITHUB_TOKEN" not in os.environ:
        pytest.skip("missing GITHUB_TOKEN")
    repos = search_repos("JavaScript", 100)
    assert len(repos) == 100
    assert all([isinstance(r, str) for r in repos])


def test_seen_repos():
    dataset = Dataset(path="non-existent")
    dataset.data["files"]["github.com/foo/bar/more"] = {}
    dataset.data["files"]["github.com/foo/bar2/more/more"] = {}
    dataset.data["files"]["gitlab/foo/bar3/more/more"] = {}
    dataset.data["files"]["other"] = {}
    assert set(get_seen_repos(dataset)) == set(["foo/bar", "foo/bar2"])


def test_try_process_repo(tmp_path):
    dataset_path = tmp_path / "dataset.yml"
    meta = Meta()
    dataset = Dataset(path=dataset_path)
    import multiprocessing

    lock = multiprocessing.Lock()
    res = try_process_repo(meta, dataset, lock, "git-fixtures/basic", "PHP")
    assert res
    assert len(dataset.data["files"]) == 1
