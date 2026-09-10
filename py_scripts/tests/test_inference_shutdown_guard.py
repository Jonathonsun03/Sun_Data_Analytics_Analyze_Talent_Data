"""Guard safety tests; all external commands are recorded, never executed."""

import json
from pathlib import Path
import subprocess
import sys
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from lib.inference_shutdown_guard import BUSY, confirms_idle, guarded_shutdown, main


class GuardTests(unittest.TestCase):
    def setUp(self):
        self.calls = []
        self.config = {"prepare_command": ["/adapter", "prepare"],
                       "release_command": ["/adapter", "release"]}
        self.change = {}
        self.prepare_error = None
        self.shutdown_error = None
        self.release_status = 0

    def run_command(self, argv):
        self.calls.append(argv)
        if argv[0] == "/usr/sbin/shutdown":
            if self.shutdown_error:
                raise self.shutdown_error
            return subprocess.CompletedProcess(argv, 0, "", "")
        if argv[1] == "release":
            return subprocess.CompletedProcess(argv, self.release_status, "", "")
        if self.prepare_error:
            raise self.prepare_error
        data = dict(protocol=1, request_id=argv[-1], ctid=106,
                    coverage="all_inference_work", admission_closed=True,
                    drain_persistent=True, active_requests=0, queued_jobs=0,
                    active_batches=0)
        data.update(self.change)
        return subprocess.CompletedProcess(argv, 0, json.dumps(data), "")

    def guard(self, execute=True):
        return guarded_shutdown(self.config, execute=execute,
                                run=self.run_command, report=lambda _: None)

    def assert_no_shutdown(self):
        self.assertFalse(any(c[0] == "/usr/sbin/shutdown" for c in self.calls))

    def test_idle_executes_exact_command_and_retains_drain(self):
        self.assertEqual(self.guard(), 0)
        self.assertEqual(self.calls[-1], ["/usr/sbin/shutdown", "-h", "now"])
        self.assertEqual(len(self.calls), 2)

    def test_dry_run_releases_without_shutdown(self):
        self.assertEqual(self.guard(execute=False), 0)
        self.assert_no_shutdown()
        self.assertEqual(self.calls[-1][1], "release")
        self.assertEqual(self.calls[0][-1], self.calls[-1][-1])

    def test_active_queued_and_batch_work_each_block(self):
        for field in ("active_requests", "queued_jobs", "active_batches"):
            with self.subTest(field=field):
                self.calls = []
                self.change = {field: 1}
                self.assertEqual(self.guard(), BUSY)
                self.assert_no_shutdown()

    def test_inconclusive_and_stale_evidence_block(self):
        for field, value in [("active_requests", False), ("queued_jobs", -1),
                             ("active_batches", "0"), ("request_id", "old"),
                             ("admission_closed", False), ("drain_persistent", False),
                             ("coverage", "punctuation_only"), ("ctid", 107),
                             ("protocol", True)]:
            with self.subTest(field=field):
                self.calls = []
                self.change = {field: value}
                self.assertEqual(self.guard(), BUSY)
                self.assert_no_shutdown()

    def test_missing_fields_block(self):
        for payload in (None, [], {}, {"status": "ok"}):
            self.assertFalse(confirms_idle(payload, "token"))

    def test_adapter_failure_releases_without_shutdown(self):
        for error in (OSError("unreachable"), ValueError("bad JSON"),
                      subprocess.TimeoutExpired("prepare", 5)):
            self.calls = []
            self.prepare_error = error
            self.assertEqual(self.guard(), BUSY)
            self.assert_no_shutdown()
            self.assertEqual(self.calls[-1][1], "release")

    def test_release_failure_is_not_success(self):
        self.release_status = 1
        self.assertEqual(self.guard(execute=False), BUSY)
        self.assert_no_shutdown()

    def test_shutdown_timeout_never_reopens_admission(self):
        self.shutdown_error = subprocess.TimeoutExpired("shutdown", 5)
        self.assertNotEqual(self.guard(), 0)
        self.assertEqual(len(self.calls), 2)

    def test_wrong_host_refuses_before_commands(self):
        with patch("lib.inference_shutdown_guard.socket.gethostname", return_value="llm-inference-new"):
            self.assertEqual(main(["--execute"]), BUSY)

    def test_missing_configuration_fails_closed(self):
        with patch("lib.inference_shutdown_guard.socket.gethostname", return_value="pve-nlp"), \
             patch("lib.inference_shutdown_guard.os.geteuid", return_value=0):
            self.assertEqual(main(["--config", "/nonexistent/guard.json", "--execute"]), BUSY)

    def test_concurrent_guard_lock_blocks(self):
        with patch("lib.inference_shutdown_guard.socket.gethostname", return_value="pve-nlp"), \
             patch("lib.inference_shutdown_guard.os.geteuid", return_value=0), \
             patch("lib.inference_shutdown_guard.load_config", return_value=self.config), \
             patch("lib.inference_shutdown_guard.os.open", side_effect=BlockingIOError):
            self.assertEqual(main(["--execute"]), BUSY)


if __name__ == "__main__":
    unittest.main()
