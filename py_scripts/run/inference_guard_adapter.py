#!/usr/bin/env python3
"""Call CT 106's loopback-only inference drain coordinator from Proxmox."""

from __future__ import annotations

import re
import subprocess
import sys


def main(argv: list[str] | None = None) -> int:
    args = sys.argv[1:] if argv is None else argv
    if len(args) != 2 or args[0] not in {"prepare", "release"}:
        return 2
    action, token = args
    if not re.fullmatch(r"[0-9a-f]{64}", token):
        return 2
    result = subprocess.run(
        [
            "/usr/sbin/pct",
            "exec",
            "106",
            "--",
            "/usr/bin/curl",
            "-fsS",
            "-X",
            "POST",
            f"http://127.0.0.1:8000/internal/shutdown/{action}/{token}",
        ],
        check=False,
    )
    return result.returncode


if __name__ == "__main__":
    raise SystemExit(main())
