# language-dataset

A dataset for programming language identification.

## Methodology

* Available languages are fetched from [github/linguist](https://github.com/github/linguist/)'s [languages.yml](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml) and [acmeism/RosettaCodeData](https://github.com/acmeism/RosettaCodeData)'s [Lang.yaml](https://github.com/acmeism/RosettaCodeData/blob/master/Meta/Lang.yaml).
* For each language, initial samples are fetched from GitHub as follows:
  * [GitHub Search API](https://developer.github.com/v4/query/#search) is used to get a list of repositories.
  * Each repository is cloned and languages are detected with [github/linguist](https://github.com/github/linguist/).
  * One sample is added from each repository.
* Samples are later reviewed by humans.

Rules for sample inclusion are:

* No more than one sample from each repository.
* Sample is at least 500b and at most 100kb.

## Dataset

The dataset is stored in the `data` directory. It contains:

* `meta.yml`: metadata about the dataset and available languages.
* `dataset.yml`: collection of all samples, with pointers sample paths relative to `data`.

Check a summary of the dataset at [REPORT.md](REPORT.md).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## Tooling

The `tools` directory contains various Python utilities to maintain the dataset:
* `tools/gen_meta.py`: Generates `data/meta.yml`. This is only needed when upgrading to a new github/linguist or acmeism/RosettaCodeData version.
* `tools/harvest.py`: Fetches samples from GitHub.
* `tools/vote.py`: Updates the `vote` annotation.
* `tools/lint.py`: Checks the dataset for potential problems.
* `tools/prepare_commit.py`: Updates generated files, required before any commit.
* `tools/classify.py`: Basic classifier used to detect potentially incorrect labels.

To run tools first create the virtual environment:

```
pip install pipenv
pipenv install
```

Then run the tool with `python -m`:

```
pipenv run python -m tools.gen_meta
```

## License

Each sample in `data` has its own license. Check the origin repository for details.

Everything else is licensed under the [MIT License](LICENSE).
