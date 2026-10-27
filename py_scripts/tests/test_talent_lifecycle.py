"""Focused lifecycle transitions and cross-language policy checks; no live data."""

import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from py_scripts.lib.talent_lifecycle.policy import allowed, records
from py_scripts.lib.talent_lifecycle.store import update_control
from py_scripts.lib.talent_lifecycle.batches import require_manifest_allowed


class TalentLifecycleTest(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        self.environment = patch.dict(os.environ, {"TALENT_CONTROL_ROOT": str(self.root)})
        self.environment.start()
        self.addCleanup(self.environment.stop)

    def change(self, action, revision):
        return update_control("TES1", action, actor="test-operator",
                              channel_ids=["UC-test"], aliases=["Test Talent"],
                              expected_revision=revision)

    def test_archive_and_pause_are_independent_and_reversible(self):
        self.change("archive", 0)
        self.assertFalse(allowed("uc-TEST", activity="retrieval"))
        self.assertTrue(allowed("TES1"))
        self.change("pause", 1)
        self.assertFalse(allowed(" Test Talent "))
        self.change("restore", 2)
        self.assertTrue(allowed("TES1", activity="retrieval"))
        self.assertFalse(allowed("TES1"))
        self.change("resume", 3)
        self.assertTrue(allowed("TES1"))

    def test_stale_save_and_malformed_record_fail_closed(self):
        self.change("pause", 0)
        with self.assertRaisesRegex(ValueError, "refresh"):
            self.change("resume", 0)
        record = records()[0]
        record["processing_enabled"] = "false"
        (self.root / "TES1.json").write_text(json.dumps(record))
        with self.assertRaises(ValueError):
            allowed("TES1")

    def test_unavailable_mount_blocks_but_unconfigured_deployment_is_compatible(self):
        with patch.dict(os.environ, {"TALENT_CONTROL_ROOT": str(self.root / "missing")}):
            with self.assertRaises(RuntimeError):
                allowed("TES1")
        with patch.dict(os.environ, {"TALENT_CONTROL_ROOT": ""}):
            self.assertTrue(allowed("TES1"))

    def test_prebuilt_mixed_batch_is_held_when_a_member_is_paused(self):
        self.change("pause", 0)
        with self.assertRaises(RuntimeError):
            require_manifest_allowed({"selected_transcripts": ["/lake/Test Talent/coded.csv"]})

    def test_r_reads_the_same_archive_and_pause_state(self):
        self.change("archive", 0)
        self.change("pause", 1)
        result = subprocess.run([
            "Rscript", "--vanilla", "-e",
            'source("r_scripts/lib/talent_lifecycle/policy.R"); '
            'stopifnot(!talent_work_allowed("UC-test", "retrieval"), '
            '!talent_work_allowed("TES1"), talent_work_allowed("Other Talent"))',
        ], env={**os.environ, "RENV_CONFIG_AUTOLOADER_ENABLED": "FALSE"},
            capture_output=True, text=True, timeout=60)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main()
