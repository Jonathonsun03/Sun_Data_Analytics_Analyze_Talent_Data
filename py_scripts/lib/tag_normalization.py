from __future__ import annotations

import re
import unicodedata
from collections import defaultdict

import pandas as pd
from rapidfuzz import fuzz, process


COMPACT_TOKEN_ALIASES = {
    "masterduel": "master duel",
    "subgoal": "sub goal",
    "yugioh": "yu gi oh",
}


def normalize_tag(value: object) -> str:
    """Normalize transparent surface differences without changing meaning."""
    text = unicodedata.normalize("NFKC", str(value)).casefold().strip()
    text = text.replace("&", " and ")
    text = re.sub(r"[_/\-]+", " ", text)
    text = re.sub(r"(?<=\d)(?=[a-z])|(?<=[a-z])(?=\d)", " ", text)
    text = re.sub(r"[^\w\s]", " ", text, flags=re.UNICODE)
    text = re.sub(r"\s+", " ", text).strip()

    expanded: list[str] = []
    for token in text.split():
        expanded.extend(COMPACT_TOKEN_ALIASES.get(token, token).split())
    return " ".join("hour" if token == "hours" else token for token in expanded)


def numeric_signature(value: object) -> tuple[str, ...]:
    return tuple(re.findall(r"\b\d+(?:\.\d+)?\s*k?\b", normalize_tag(value)))


class _DisjointSet:
    def __init__(self, values: list[str]) -> None:
        self.parent = {value: value for value in values}

    def find(self, value: str) -> str:
        parent = self.parent[value]
        if parent != value:
            self.parent[value] = self.find(parent)
        return self.parent[value]

    def union(self, left: str, right: str) -> None:
        left_root = self.find(left)
        right_root = self.find(right)
        if left_root != right_root:
            self.parent[right_root] = left_root


def build_tag_mapping(
    raw_tags: pd.Series,
    *,
    fuzzy_score_cutoff: float = 86,
    fuzzy_high_confidence_cutoff: float = 96,
) -> pd.DataFrame:
    """Build deterministic and guarded-RapidFuzz mappings for observed tags."""
    inventory = (
        pd.DataFrame({"raw_label": raw_tags.dropna().astype(str).str.strip()})
        .loc[lambda frame: frame["raw_label"].ne("")]
        .groupby("raw_label", as_index=False)
        .size()
        .rename(columns={"size": "observed_uses"})
    )
    if inventory.empty:
        return pd.DataFrame(
            columns=[
                "raw_label",
                "normalized_label",
                "canonical_label",
                "broader_group",
                "normalization_method",
                "observed_uses",
                "rapidfuzz_component",
                "component_size",
            ]
        )

    inventory["normalized_label"] = inventory["raw_label"].map(normalize_tag)
    normalized = (
        inventory.groupby("normalized_label", as_index=False)
        .agg(observed_uses=("observed_uses", "sum"))
        .sort_values(["observed_uses", "normalized_label"], ascending=[False, True])
        .reset_index(drop=True)
    )
    labels = normalized["normalized_label"].tolist()
    uses = normalized.set_index("normalized_label")["observed_uses"].to_dict()
    high_cutoff = max(fuzzy_score_cutoff, fuzzy_high_confidence_cutoff)
    clusters = _DisjointSet(labels)

    for left_index, left_label in enumerate(labels):
        matches = process.extract(
            left_label,
            labels,
            scorer=fuzz.WRatio,
            score_cutoff=fuzzy_score_cutoff,
            limit=20,
        )
        for right_label, weighted_score, right_index in matches:
            if right_index <= left_index or weighted_score < high_cutoff:
                continue
            if numeric_signature(left_label) != numeric_signature(right_label):
                continue
            left_tokens = set(left_label.split())
            right_tokens = set(right_label.split())
            if left_tokens != right_tokens and (
                left_tokens.issubset(right_tokens)
                or right_tokens.issubset(left_tokens)
            ):
                continue
            if (
                fuzz.ratio(left_label, right_label) >= high_cutoff
                and fuzz.token_ratio(left_label, right_label) >= high_cutoff
            ):
                clusters.union(left_label, right_label)

    members_by_component: dict[str, list[str]] = defaultdict(list)
    for label in labels:
        members_by_component[clusters.find(label)].append(label)

    canonical_by_label: dict[str, str] = {}
    component_size: dict[str, int] = {}
    for members in members_by_component.values():
        canonical = sorted(
            members,
            key=lambda label: (-int(uses[label]), len(label), label),
        )[0]
        for label in members:
            canonical_by_label[label] = canonical
            component_size[label] = len(members)

    mapping = inventory.copy()
    mapping["canonical_label"] = mapping["normalized_label"].map(canonical_by_label)
    mapping["broader_group"] = pd.NA
    mapping["normalization_method"] = "unchanged"
    deterministic = mapping["raw_label"].ne(mapping["normalized_label"])
    rapidfuzz = mapping["normalized_label"].ne(mapping["canonical_label"])
    mapping.loc[deterministic, "normalization_method"] = "deterministic"
    mapping.loc[rapidfuzz, "normalization_method"] = "rapidfuzz"
    mapping.loc[
        deterministic & rapidfuzz,
        "normalization_method",
    ] = "deterministic_and_rapidfuzz"
    mapping["rapidfuzz_component"] = mapping["canonical_label"]
    mapping["component_size"] = mapping["normalized_label"].map(component_size)
    return mapping.sort_values("raw_label").reset_index(drop=True)
