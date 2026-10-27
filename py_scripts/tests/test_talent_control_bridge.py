"""Exercise the real HTTP adapter with disposable catalog and control records."""

import json
import os
import tempfile
import threading
import unittest
import stat
import urllib.error
import urllib.request
from pathlib import Path
from unittest.mock import patch

from py_scripts.lib.talent_control_bridge import make_server
from py_scripts.lib.talent_control_catalog import read_catalog
from py_scripts.lib.talent_lifecycle.policy import allowed


class BridgeTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.controls = self.root / "controls"
        self.controls.mkdir()
        self.env = patch.dict(os.environ, {"TALENT_CONTROL_ROOT": str(self.controls),
                                          "TALENT_CONTROL_MOUNT": ""})
        self.env.start()
        self.addCleanup(self.env.stop)
        import duckdb
        self.db = self.root / "fixture_lakehouse.duckdb"
        with duckdb.connect(str(self.db)) as con:
            con.execute("CREATE SCHEMA catalog")
            con.execute("CREATE TABLE catalog.talents(talent_code VARCHAR, talent_name VARCHAR)")
            con.execute("CREATE TABLE catalog.channels(talent_code VARCHAR, channel_id VARCHAR, channel_name VARCHAR)")
            con.execute("CREATE TABLE catalog.talent_aliases(talent_code VARCHAR, alias VARCHAR)")
            con.execute("INSERT INTO catalog.talents VALUES ('TES1', 'Test Talent')")
            con.execute("INSERT INTO catalog.channels VALUES ('TES1', 'UC-test', 'Test:Channel')")
            con.execute("INSERT INTO catalog.talent_aliases VALUES ('TES1', 'Other alias')")
        self.server = make_server(("127.0.0.1", 0), secret="fixture-secret-" * 3,
                                  catalog_reader=lambda: read_catalog(self.db))
        thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        thread.start()
        self.addCleanup(self.server.server_close)
        self.addCleanup(thread.join, 5)
        self.addCleanup(self.server.shutdown)

    def request(self, path="/v1/talents", body=None, authenticated=True, method=None):
        headers = {"Content-Type": "application/json", "X-SDA-Lifecycle-Actor": "admin@example.com"}
        if authenticated:
            headers["Authorization"] = "Bearer " + "fixture-secret-" * 3
        req = urllib.request.Request(f"http://127.0.0.1:{self.server.server_port}{path}",
                                     data=json.dumps(body).encode() if body is not None else None,
                                     headers=headers, method=method)
        try:
            response = urllib.request.urlopen(req, timeout=10)
        except urllib.error.HTTPError as error:
            response = error
        with response:
            return response.status, json.load(response)

    def change(self, action, revision):
        return self.request("/v1/talents/TES1", {"action": action, "expected_revision": revision})

    def test_independent_switches_and_preservation(self):
        results = self.root / "results.bin"
        checkpoint = self.root / "checkpoint.bin"
        for path in (results, checkpoint):
            path.write_bytes(b"preserve exactly")
        self.assertEqual(self.change("archive", 0)[0], 200)
        self.assertFalse(allowed("UC-test", activity="retrieval"))
        self.assertTrue(allowed("TES1"))
        self.assertEqual(self.change("pause", 1)[0], 200)
        self.assertFalse(allowed("Test_Channel"))
        self.assertFalse(allowed("Other alias"))
        self.assertEqual(self.change("resume", 2)[0], 200)
        self.assertFalse(allowed("TES1", activity="retrieval"))
        self.assertTrue(allowed("TES1"))
        self.assertEqual(self.change("pause", 3)[0], 200)
        self.assertEqual(self.change("restore", 4)[0], 200)
        self.assertTrue(allowed("TES1", activity="retrieval"))
        self.assertFalse(allowed("TES1"))
        self.assertEqual(self.change("resume", 5)[0], 200)
        self.assertEqual(self.change("archive", 6)[0], 200)
        self.assertTrue(allowed("TES1"))
        for path in (results, checkpoint):
            self.assertEqual(path.read_bytes(), b"preserve exactly")
        state = self.request()[1]["talents"][0]
        self.assertEqual(state["revision"], 7)
        self.assertEqual(state["updated_by"], "admin@example.com")
        self.assertEqual(stat.S_IMODE((self.controls / "TES1.json").stat().st_mode), 0o640)
        self.assertEqual(stat.S_IMODE((self.controls / ".write.lock").stat().st_mode), 0o600)

    def test_auth_identity_validation_and_revision_conflicts(self):
        self.assertEqual(self.request(authenticated=False)[0], 401)
        self.assertEqual(self.request("/v1/talents/TES1", {"action": "pause", "expected_revision": 0}, False)[0], 401)
        self.assertEqual(self.change("pause", 0)[0], 200)
        self.assertEqual(self.change("resume", 0)[0], 409)
        self.assertEqual(self.change("delete", 1)[0], 400)
        self.assertEqual(self.change("resume", True)[0], 400)
        self.assertEqual(self.request("/v1/talents/NOPE", {"action": "pause", "expected_revision": 0})[0], 404)
        self.assertEqual(self.request("/v1/talents/TES1", {"action": "resume", "expected_revision": 1,
                                                         "actor": "spoof@example.com"})[0], 400)
        self.assertEqual(self.request("/v1/talents/TES1", method="DELETE")[0], 405)
        self.assertEqual(len(list(self.controls.glob("*.json"))), 1)

    def test_missing_configured_mount_and_directory_fail_closed(self):
        with patch.dict(os.environ, {"TALENT_CONTROL_MOUNT": str(self.root)}):
            with self.assertRaises(RuntimeError):
                allowed("TES1")
            self.assertEqual(self.request()[0], 503)
        with patch.dict(os.environ, {"TALENT_CONTROL_ROOT": str(self.root / "missing")}):
            self.assertEqual(self.request()[0], 503)


if __name__ == "__main__":
    unittest.main()
