"""Serve lifecycle administration behind the authenticated Worker origin route."""

import os
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))

from py_scripts.lib.talent_control_bridge import make_server
from py_scripts.lib.talent_control_catalog import read_catalog
from py_scripts.lib.talent_lifecycle.policy import control_root


def main():
    # Service EnvironmentFile supplies configuration; do not load a writer's repo .env.
    if control_root() is None:
        raise RuntimeError("TALENT_CONTROL_ROOT is required")
    database = Path(os.environ["UNIFIED_CATALOG_DB_PATH"]).resolve(strict=True)
    with make_server((os.environ.get("TALENT_BRIDGE_HOST", "127.0.0.1"),
                      int(os.environ.get("TALENT_BRIDGE_PORT", "8765"))),
                     secret=os.environ.get("TALENT_BRIDGE_SECRET", ""),
                     catalog_reader=lambda: read_catalog(database)) as server:
        server.serve_forever()


if __name__ == "__main__":
    main()
