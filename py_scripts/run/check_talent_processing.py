"""Admission check for prompt-driven jobs that do not select work in Python/R."""

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from py_scripts.lib.talent_lifecycle.policy import records, require_allowed


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--talent", default="")
    args = parser.parse_args()
    if args.talent and args.talent.casefold() not in {"all", "*"}:
        require_allowed(args.talent)
    elif any(not record["processing_enabled"] for record in records()):
        raise SystemExit("A talent is paused. Run this prompt job with --talent for an enabled talent.")


if __name__ == "__main__":
    main()
