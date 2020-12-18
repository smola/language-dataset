from .common import *


def vote(ann, weights):
    all_votes = {}
    human_votes = {}
    for key, lang in ann.items():
        if key.startswith("vote"):
            continue
        if key.startswith("human"):
            human_votes[lang] = human_votes.get(lang, 0) + 1
            all_votes[lang] = all_votes.get(lang, 0.0) + 1.0
        else:
            all_votes[lang] = all_votes.get(lang, 0.0) + weights.get(key, 1.0)
    if len(all_votes) == 0:
        return "Unknown"
    if len(human_votes) > 0:
        max_votes = max(human_votes.values())
        langs = [l for l, v in human_votes.items() if v == max_votes]
        if len(langs) == 1:
            return langs[0]
    max_votes = max(all_votes.values())
    langs = [l for l, v in all_votes.items() if v == max_votes]
    if len(langs) != 1:
        return "Unknown"
    lang = langs[0]
    if all_votes[lang] <= sum(all_votes.values()) - all_votes[lang]:
        return "Unknown"
    return lang


def vote_all(dataset, weights):
    for path, file_data in dataset.data["files"].items():
        file_data["annotations"]["vote"] = vote(file_data["annotations"], weights)


def get_weights(dataset):
    good = {}
    bad = {}
    for path, file_data in dataset.data["files"].items():
        ann = file_data["annotations"]
        if not any([k.startswith("human-") for k in ann.keys()]):
            continue
        class_ = file_data["annotations"]["vote"]
        if class_ == "Unknown":
            continue
        for k, v in ann.items():
            if k.startswith("human-"):
                continue
            if k == "vote":
                continue
            if v == class_:
                good[k] = good.get(k, 0) + 1
            else:
                bad[k] = bad.get(k, 0) + 1
    weights = {}
    for k in set(good.keys()) | set(bad.keys()):
        weights[k] = good.get(k, 0) / (good.get(k, 0) + bad.get(k, 0))
    return weights


def main():
    dataset = Dataset()
    weights = get_weights(dataset)
    for k, v in sorted(weights.items()):
        print(f"Weight: {k} -> {v}")
    vote_all(dataset, weights)
    dataset.save()


if __name__ == "__main__":
    main()
