import os
import os.path
import subprocess
from typing import Sequence, Union
import yaml

try:
    from yaml import CLoader as Loader, CDumper as Dumper
except ImportError:
    from yaml import Loader, Dumper

DATA_DIR = "data"
META_PATH = os.path.join(DATA_DIR, "meta.yml")
DATASET_PATH = os.path.join(DATA_DIR, "dataset.yml")

MIN_SAMPLE_SIZE = 500
MAX_SAMPLE_SIZE = 1024 * 100
SAMPLES_PER_LANGUAGE = 20


class ConfObject:
    def __init__(self, path):
        self.path = path
        self.data = None
        self.load()

    def save(self):
        save_yaml(self.data, self.path)

    def load(self):
        if os.path.exists(self.path):
            self.data = load_yaml(self.path)
        else:
            self.data = self._default_value()

    def _default_value(self):
        return {}


class Meta(ConfObject):
    def __init__(self, path=META_PATH):
        super().__init__(path=path)

    def _default_value(self):
        return {"datasets": {}, "languages": {}}

    def add_dataset(self, name: str, data):
        self.data["datasets"][name] = data

    def add_language(self, dataset: str, norm_lang: str, lang: str):
        if "languages" not in self.data:
            self.data["languages"] = {}
        ld = self.data["languages"]
        if norm_lang not in ld:
            ld[norm_lang] = {"maps_to": {}}
        if dataset not in ld[norm_lang]["maps_to"]:
            ld[norm_lang]["maps_to"][dataset] = []
        if lang not in ld[norm_lang]["maps_to"][dataset]:
            ld[norm_lang]["maps_to"][dataset].append(lang)

    def to_normalized_language(self, dataset: str, lang) -> str:
        if lang == "Unknown":
            return lang
        for norm_lang, ld in self.data["languages"].items():
            maps_to = ld["maps_to"]
            if dataset not in maps_to:
                continue
            for specific_lang in maps_to[dataset]:
                if lang == specific_lang:
                    return norm_lang
        raise Exception("unknown language in dataset %s: %s" % (dataset, lang))

    def to_dataset_languages(self, dataset: str, norm_lang: str) -> Sequence[str]:
        return list(self.data["languages"][norm_lang]["maps_to"].get(dataset, []))

    def get_normalized_languages(self) -> Sequence[str]:
        return list(self.data["languages"].keys())


class Dataset(ConfObject):
    def __init__(self, path=DATASET_PATH):
        super().__init__(path=path)

    def _default_value(self):
        return {"files": {}}


def load_yaml(path: str):
    with open(path, "r") as f:
        return load_yaml_from_steam(f)


def load_yaml_from_steam(f):
    return yaml.load(f, Loader=Loader)


def save_yaml(data, path):
    with open(path, "w") as f:
        yaml.dump(data, f, Dumper=Dumper)


def clone_tmp_repo(repo, commit=None):
    tmp_dir = os.path.join("tmp", repo)
    if os.path.exists(os.path.join(tmp_dir, ".git")):
        return tmp_dir
    os.makedirs(tmp_dir, exist_ok=True)
    url = "https://github.com/%s.git" % repo
    if commit:
        subprocess.run(
            "git clone --no-progress --single-branch %s %s" % (url, tmp_dir),
            shell=True,
            check=True,
        )
        subprocess.run("git checkout %s" % commit, cwd=tmp_dir, shell=True, check=True)
    else:
        subprocess.run(
            "git clone --no-progress --depth 1 %s %s" % (url, tmp_dir),
            shell=True,
            check=True,
        )
    return tmp_dir
