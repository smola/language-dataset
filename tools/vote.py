from .common import *


def vote(ann):
    all_votes = {}
    human_votes = {}
    for key, lang in ann.items():
        if key.startswith("vote"):
            continue
        if key.startswith("human"):
            human_votes[lang] = human_votes.get(lang, 0) + 1
        all_votes[lang] = all_votes.get(lang, 0) + 1
    if len(human_votes) > 0:
        max_votes = max(human_votes.values())
        langs = [l for l, v in human_votes.items() if v == max_votes]
        if len(langs) == 1:
            return langs[0]
    max_votes = max(all_votes.values())
    langs = [l for l, v in all_votes.items() if v == max_votes]
    if len(langs) == 1:
        return langs[0]
    return "Unknown"


def vote_all(dataset):
    for path, file_data in dataset.data["files"].items():
        file_data["annotations"]["vote"] = vote(file_data["annotations"])


def main():
    dataset = Dataset()
    vote_all(dataset)
    dataset.save()


if __name__ == "__main__":
    main()
