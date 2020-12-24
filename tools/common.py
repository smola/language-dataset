import os
import os.path
import subprocess
from typing import Optional, Sequence, Union

# XXX: long keys (>128 characters) will be prefixed with '?'
#     This workaround prevents this behavior with Dumper.
#     Note that CDumper has a similar (but not fully consistent)
#     behavior, so we strictly rely on the slower pure Python implementation.
import yaml

yaml.emitter.Emitter.check_simple_key = lambda x: True

from yaml import Dumper

try:
    from yaml import CLoader as Loader
except ImportError:
    from yaml import Loader

DATA_DIR = "data"
META_PATH = os.path.join(DATA_DIR, "meta.yml")
DATASET_PATH = os.path.join(DATA_DIR, "dataset.yml")

MIN_SAMPLE_SIZE = 500
MAX_SAMPLE_SIZE = 1024 * 100
SAMPLES_PER_LANGUAGE = 20


class ConfObject:
    def __init__(self, path, load=True):
        self.path = path
        self.data = None
        if load:
            self.load()
        else:
            self.data = self._default_value()

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
    def __init__(self, path=META_PATH, load=True):
        super().__init__(path=path, load=load)

    def _default_value(self):
        return {"datasets": {}, "languages": {}}

    def add_dataset(self, name: str, data):
        self.data["datasets"][name] = data

    def add_language(
        self, dataset: str, norm_lang: str, lang: str, group: Optional[str] = None
    ):
        if "languages" not in self.data:
            self.data["languages"] = {}
        ld = self.data["languages"]
        if norm_lang not in ld:
            ld[norm_lang] = {"maps_to": {}}
        if dataset not in ld[norm_lang]["maps_to"]:
            ld[norm_lang]["maps_to"][dataset] = []
        if lang not in ld[norm_lang]["maps_to"][dataset]:
            ld[norm_lang]["maps_to"][dataset].append(lang)
        if group:
            ld[norm_lang]["group"] = group

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
    def __init__(self, path=DATASET_PATH, load=True):
        super().__init__(path=path, load=load)

    def _default_value(self):
        return {"files": {}}


def load_yaml(path: str):
    with open(path, "r") as f:
        return load_yaml_from_steam(f)


def load_yaml_from_steam(f):
    return yaml.load(f, Loader=Loader)


def save_yaml(data, path):
    with open(path, "w") as f:
        yaml.dump(
            data,
            f,
            Dumper=Dumper,
            width=10000,
            line_break="\n",
            canonical=False,
            sort_keys=True,
            default_flow_style=False,
        )


def clone_tmp_repo(repo, commit=None):
    tmp_dir = os.path.join("tmp", repo)
    if os.path.exists(os.path.join(tmp_dir, ".git")):
        return tmp_dir
    os.makedirs(tmp_dir, exist_ok=True)
    url = "git://github.com/%s.git" % repo
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
