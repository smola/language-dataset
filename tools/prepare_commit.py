from .common import *
from .vote import main as vote


def compute_report():
    meta = Meta()
    dataset = Dataset()
    report = {}
    report = {"languages": {}}
    for lang, lang_data in meta.data["languages"].items():
        if "linguist" not in lang_data["maps_to"]:
            continue
        n_samples = 0
        n_human = 0
        for file_data in dataset.data["files"].values():
            ann = file_data["annotations"]
            if "vote" not in ann:
                continue
            if ann["vote"] != lang:
                continue
            n_samples += 1
            if len([k for k in ann.keys() if k.startswith("human")]) > 0:
                n_human += 1
        if n_samples == 0:
            continue
        report["languages"][lang] = {"n_samples": n_samples, "n_human": n_human}
    report["summary"] = {
        "n_languages": len(report["languages"].keys()),
        "n_samples": sum([d["n_samples"] for d in report["languages"].values()]),
        "n_human": sum([d["n_human"] for d in report["languages"].values()]),
    }
    return report


def write_report():
    REPORT_PATH = "REPORT.md"
    report = compute_report()
    with open(REPORT_PATH, "w") as f:
        print("# Dataset Report", file=f)
        print("## Summary", file=f)
        print("", file=f)
        print("| Metric | Value |", file=f)
        print("| ------ | ----- |", file=f)
        print("| **Languages** | %d |" % report["summary"]["n_languages"], file=f)
        print("| **Total samples** | %d |" % report["summary"]["n_samples"], file=f)
        print(
            "| **Samples labeled by humans** | %d |" % report["summary"]["n_human"],
            file=f,
        )
        print("", file=f)
        print("## Per Language", file=f)
        print(
            "For each language, the table reports total number of samples (_Samples_) and how many of them have been labeled by a human (_Human_).",
            file=f,
        )
        print("", file=f)
        print("| Language | Samples | Human |", file=f)
        print("| -------- | ------- | ------|", file=f)
        for lang, d in report["languages"].items():
            print("| %s | %d | %d |" % (lang, d["n_samples"], d["n_human"]), file=f)


def main():
    vote()
    write_report()


if __name__ == "__main__":
    main()
