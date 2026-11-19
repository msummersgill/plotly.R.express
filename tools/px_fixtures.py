"""Reference output from plotly.express for tests/test-express.R.

Run from the package root with plotly.py (plus pandas, numpy, statsmodels):
    python3 tools/px_fixtures.py
Reads tests/fixtures/px-cases.json and writes tests/fixtures/plotly-py-express.json.
"""
import json

import pandas as pd
import plotly
import plotly.express as px

INLINE = {
    "inline_funnel": lambda: pd.DataFrame({
        "number": [39, 27.4, 20.6, 11, 52, 36, 18, 14],
        "stage": ["Visit", "Download", "Sign up", "Purchase"] * 2,
        "office": ["Montreal"] * 4 + ["Toronto"] * 4}),
    "inline_timeline": lambda: pd.DataFrame({
        "task": ["Job A", "Job B", "Job C"],
        "start": ["2009-01-01", "2009-03-05", "2009-02-20"],
        "finish": ["2009-02-28", "2009-04-15", "2009-05-30"],
        "resource": ["Alex", "Alex", "Max"]}),
}


def dataset(name, filters, data_args):
    if name is None:
        return None
    df = INLINE[name]() if name in INLINE else getattr(px.data, name)(**(data_args or {}))
    for col, values in (filters or {}).items():
        df = df[df[col].isin(values)].reset_index(drop=True)
    return df


def plain_json(obj):
    """Encode a figure dict with plain JSON arrays (to_json() writes numpy
    arrays as base64 typed arrays) and NaN as null."""
    import datetime
    import math

    import numpy as np

    def conv(o):
        if isinstance(o, dict) and "bdata" in o and "dtype" in o:
            import base64
            arr = np.frombuffer(base64.b64decode(o["bdata"]), dtype=np.dtype(o["dtype"]))
            if "shape" in o:
                shape = o["shape"]
                arr = arr.reshape([int(n) for n in shape.split(",")] if isinstance(shape, str) else shape)
            return conv(arr)
        if isinstance(o, dict):
            return {k: conv(v) for k, v in o.items()}
        if isinstance(o, (list, tuple)):
            return [conv(v) for v in o]
        if isinstance(o, np.ndarray):
            if o.dtype.kind == "M":
                return [None if np.isnat(v) else str(v) for v in o]
            return conv(o.tolist())
        if isinstance(o, pd.Series):
            return conv(o.tolist())
        if isinstance(o, pd.DataFrame):
            return conv(o.values.tolist())
        if isinstance(o, (np.generic,)):
            return conv(o.item())
        if isinstance(o, float) and (math.isnan(o) or math.isinf(o)):
            return None
        if isinstance(o, (pd.Timestamp, datetime.datetime, datetime.date)):
            return o.isoformat()
        if o is pd.NaT:
            return None
        return o
    return json.loads(json.dumps(conv(obj)))


def strip(fig):
    d = plain_json(fig.to_dict())
    d["layout"].pop("template", None)
    for tr in d["data"] + [t for f in d.get("frames", []) for t in f["data"]]:
        tr.pop("geojson", None)
    return d


cases = json.load(open("tests/fixtures/px-cases.json"))
out = {"plotly_version": plotly.__version__, "figures": {}}
for case in cases:
    fig = getattr(px, case["fn"])(dataset(case["data"], case.get("filter"), case.get("data_args")), **case["kwargs"])
    out["figures"][case["name"]] = strip(fig)

# px.imshow cases: images are given as nested lists plus a numpy dtype
import numpy as np

imshow_cases = json.load(open("tests/fixtures/px-imshow-cases.json"))
for case in imshow_cases:
    img = np.array(case["img"], dtype=case["dtype"])
    if case.get("dataframe"):
        img = pd.DataFrame(img, columns=case["columns"])
    fig = px.imshow(img, **case["kwargs"])
    out["figures"][case["name"]] = strip(fig)

with open("tests/fixtures/plotly-py-express.json", "w", encoding="utf-8") as f:
    json.dump(out, f, separators=(",", ":"), ensure_ascii=False)
