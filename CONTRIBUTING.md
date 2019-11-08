
# Contributing to language-dataset

* [Fixing a sample label](#fixing-a-sample-label)
* [Adding a new language](#adding-a-new-language)
* [Improving the tooling](#Improving-the-tooling)

## Fixing a sample label

If you find an incorrect label, you can submit an annotation:

1. Find the sample at `data/dataset.yml`.
2. In the `annotations` section, add your annotation as follows:
   `human-<github_username>: <language>` where `<github_username>` is your GitHub username and `<language>` is one of the possible keys listed at `data/meta.yml` or `Unknown` if it is an unlisted language.
3. Run `pipenv run python -m tools.lint`.
4. Submit a Pull Request with all changes.

## Adding a new language

At the moment we do not accept new languages. However, if the language is popular enough, you can add support for it to [github/linguist](https://github.com/github/linguist/) and we will eventually import it to the dataset.

## Improving the tooling

If you submit changes to the tooling, keep in mind that:

1. We format Python code with [black](https://github.com/psf/black).
