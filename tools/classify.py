import numpy as np
import pandas as pd

from .common import *

CV_SPLITS = 10


def load_dataset(dataset: Dataset):
    X = []
    Y = []
    files = []

    for path, file_data in dataset.data["files"].items():
        if "vote" not in file_data["annotations"]:
            continue
        lang = file_data["annotations"]["vote"]
        if lang == "Unknown":
            continue
        with open(os.path.join(DATA_DIR, path), "rb") as f:
            code = f.read()
        code = code[:10000]
        code = code.decode(encoding="utf-8", errors="replace")
        files.append(path)
        filename = os.path.basename(path)
        X.append(filename + " " + code)
        Y.append(lang)

    df = pd.DataFrame(Y, columns=["class_name"])
    df["class_name"] = df["class_name"].astype(np.str)
    df["code"] = pd.Series(X)
    df["file"] = pd.Series(files)

    classes = df.groupby(["class_name"]).count().reset_index()
    classes = classes.loc[classes["file"] >= CV_SPLITS]
    classes = classes.reset_index(drop=False).reset_index()
    classes = classes.rename(columns={"index": "class_index"})
    classes["class_name"] = classes["class_name"].astype(np.str)
    classes = classes[["class_name", "class_index"]]

    df = df.merge(classes, on=["class_name"], how="inner")
    return (df, classes)


def create_pipeline():
    from sklearn.feature_extraction.text import CountVectorizer

    vectorizer = CountVectorizer(
        input="content",
        encoding="utf-8",
        decode_error="replace",
        analyzer="char",
        lowercase=False,
        ngram_range=(1, 5),
        min_df=4,
        max_df=0.98,
        dtype=np.uint32,
    )

    from sklearn.feature_extraction.text import TfidfTransformer

    tfidf = TfidfTransformer(
        sublinear_tf=True,
    )

    from sklearn.ensemble import ExtraTreesClassifier

    clf = ExtraTreesClassifier(
        n_estimators=100,
        min_samples_leaf=2,
        max_features="sqrt",
        n_jobs=1,
    )

    from sklearn.pipeline import Pipeline

    pipeline = Pipeline(
        [
            ("vectorize", vectorizer),
            ("tfidf", tfidf),
            ("clf", clf),
        ],
        verbose=True,
    )

    return pipeline


def cross_eval(pipeline, df, classes):
    from sklearn.model_selection import cross_val_predict

    pred_Y = cross_val_predict(
        pipeline, df["code"], df["class_index"], cv=CV_SPLITS, n_jobs=5, verbose=100
    )
    df = df.copy()
    df["pred_index"] = pd.Series(pred_Y)
    df["hit"] = df["class_index"] == df["pred_index"]
    df["pred_name"] = df["pred_index"].apply(
        lambda x: classes.loc[classes["class_index"] == x]["class_name"].iloc[0]
    )
    return df


def get_errors(df):
    return df.loc[~df["hit"]]["file"]


def main():
    dataset = Dataset()
    df, classes = load_dataset(dataset)
    pipeline = create_pipeline()
    df = cross_eval(pipeline, df, classes)
    for file in get_errors(df):
        print("| %s |" % file)


if __name__ == "__main__":
    main()
