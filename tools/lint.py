import os
import os.path
import sys

from .common import *


def max_samples_per_repositories(dataset, max_samples):
    samples_per_repo = {}
    for path in dataset.data["files"].keys():
        if not path.startswith("github.com/"):
            continue
        repo = "/".join(path.split("/")[1:3])
        samples_per_repo[repo] = samples_per_repo.get(repo, []) + [path]

    errors = []
    for repo, samples in samples_per_repo.items():
        if len(samples) <= max_samples:
            continue
        errors.append(
            "more than %s samples for github.com/%s: %s" % (max_samples, repo, samples)
        )

    return errors


def declared_samples_are_present(dataset):
    errors = []
    for path in dataset.data["files"].keys():
        if not os.path.exists(os.path.join(DATA_DIR, path)):
            errors.append("declared sample does not exist: %s" % path)
    return errors


def present_samples_are_declared(dataset):
    errors = []
    for base, ds, fs in os.walk(os.path.join(DATA_DIR, "github.com")):
        if not fs:
            continue
        for f in fs:
            f = os.path.join(base, f)
            f = os.path.relpath(f, DATA_DIR)
            if f not in dataset.data["files"]:
                errors.append("present sample not declared in dataset.yml: %s" % f)
    return errors


def samples_are_within_size_limits(dataset):
    errors = []
    for path in dataset.data["files"].keys():
        actual_path = os.path.join(DATA_DIR, path)
        if not os.path.exists(actual_path):
            continue
        size = os.path.getsize(actual_path)
        if size < MIN_SAMPLE_SIZE:
            errors.append("sample under %s bytes: %s" % (MIN_SAMPLE_SIZE, path))
        elif size > MAX_SAMPLE_SIZE:
            errors.append("sample over %s bytes: %s" % (MAX_SAMPLE_SIZE, path))
    return errors


def main():
    dataset = Dataset()
    errors = []
    errors += max_samples_per_repositories(dataset, max_samples=1)
    errors += declared_samples_are_present(dataset)
    errors += present_samples_are_declared(dataset)
    errors += samples_are_within_size_limits(dataset)
    for error in errors:
        print("ERROR: %s" % error)
    return 1 if errors else 0


if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)
