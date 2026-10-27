"""CLI used by the internal admin dashboard to change one talent switch."""

import argparse
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from py_scripts.lib.talent_lifecycle.store import ACTIONS, update_control
from py_scripts.lib.utils.env import load_repo_env


def main() -> None:
    load_repo_env()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=ACTIONS)
    parser.add_argument("--talent-code", required=True)
    parser.add_argument("--actor", required=True)
    parser.add_argument("--expected-revision", required=True, type=int)
    parser.add_argument("--channel-id", action="append", default=[])
    parser.add_argument("--alias", action="append", default=[])
    args = parser.parse_args()
    print(json.dumps(update_control(
        args.talent_code, args.action, actor=args.actor,
        channel_ids=args.channel_id, aliases=args.alias,
        expected_revision=args.expected_revision,
    )))


if __name__ == "__main__":
    main()
