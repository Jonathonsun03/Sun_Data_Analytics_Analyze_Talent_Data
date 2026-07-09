from __future__ import annotations


def squish(value: object) -> str:
    return " ".join(str(value or "").strip().split())


def is_missing(value: object) -> bool:
    text = str(value or "").strip()
    return text == "" or text.upper() == "NA"

