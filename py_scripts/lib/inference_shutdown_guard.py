"""Fail-closed Proxmox shutdown orchestration.

The service adapter must atomically stop admission and inspect ALL inference
producers, including queued jobs and batch leases. It is intentionally not
replaced by a process list, CPU utilization, or /health request.
"""

from __future__ import annotations

import fcntl
import json
import os
from pathlib import Path
import secrets
import socket
import stat
import subprocess
from typing import Callable


BUSY = 75


def load_config(path: Path) -> dict:
    """Only accept an administrator-owned config in protected directories."""
    path = path.absolute()
    for item in [path, *path.parents]:
        info = item.lstat()
        if stat.S_ISLNK(info.st_mode) or info.st_uid != 0 or info.st_mode & 0o022:
            raise ValueError(f"Untrusted configuration path: {item}")
    config = json.loads(path.read_text())
    if config.get("host") != "pve-nlp" or config.get("ctid") != 106:
        raise ValueError("Guard is restricted to pve-nlp / CT 106")
    if config.get("coverage_verified") is not True:
        raise ValueError("Service adapter coverage has not been verified")
    for key in ("prepare_command", "release_command"):
        command = config.get(key)
        if (not isinstance(command, list) or not command
                or any(not isinstance(arg, str) or not arg or "\0" in arg for arg in command)
                or not Path(command[0]).is_absolute()):
            raise ValueError(f"{key} must be an argv list with an absolute executable")
    return config


def run_command(argv: list[str]) -> subprocess.CompletedProcess:
    return subprocess.run(argv, capture_output=True, text=True, timeout=5, check=False)


def confirms_idle(payload: object, token: str) -> bool:
    """Reject missing fields, booleans as counts, stale responses and partial scope."""
    if not isinstance(payload, dict):
        return False
    return (
        type(payload.get("protocol")) is int and payload["protocol"] == 1
        and payload.get("request_id") == token
        and type(payload.get("ctid")) is int and payload["ctid"] == 106
        and payload.get("coverage") == "all_inference_work"
        and payload.get("admission_closed") is True
        and payload.get("drain_persistent") is True
        and all(type(payload.get(key)) is int and payload[key] == 0
                for key in ("active_requests", "queued_jobs", "active_batches"))
    )


def guarded_shutdown(
    config: dict,
    *,
    execute: bool = False,
    run: Callable = run_command,
    report: Callable = print,
) -> int:
    """Caller must hold the host lock; adapter drain persists until release/reboot.

    prepare/release receive a unique request ID as their last argument. A
    prepare timeout may still have drained admission remotely: always attempt
    idempotent release unless shutdown was attempted. Once shutdown is attempted,
    retain the drain even if its result is uncertain.
    """
    token = secrets.token_hex(32)
    shutdown_attempted = False
    outcome = BUSY
    try:
        result = run([*config["prepare_command"], token])
        payload = json.loads(result.stdout) if result.returncode == 0 else None
        if not confirms_idle(payload, token):
            report("Host left on: busy, incomplete, stale, or inconclusive workload evidence.")
        elif not execute:
            report("Dry run: adapter confirms drained and idle; no shutdown issued.")
            outcome = 0
        else:
            shutdown_attempted = True
            outcome = 1  # Retain the caller reservation if shutdown becomes uncertain.
            # Never execute a shell command supplied by a caller or API response.
            result = run(["/usr/sbin/shutdown", "-h", "now"])
            if result.returncode == 0:
                report("Physical host shutdown accepted; admission remains closed.")
                outcome = 0
            else:
                report("Shutdown failed; admission remains closed for inspection.")
    except (OSError, ValueError, TypeError, KeyError, subprocess.SubprocessError):
        report("Host shutdown unconfirmed: activity/command check failed; failing closed.")
    finally:
        if not shutdown_attempted:
            try:
                result = run([*config["release_command"], token])
                if result.returncode != 0:
                    report("Drain release failed; inspect the service before retrying.")
                    outcome = BUSY
            except (OSError, subprocess.SubprocessError):
                report("Drain release failed; inspect the service before retrying.")
                outcome = BUSY
    return outcome


def main(argv: list[str] | None = None) -> int:
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config", type=Path,
                        default=Path("/etc/sun-data/inference-shutdown-guard.json"))
    parser.add_argument("--execute", action="store_true")
    parser.add_argument("shutdown_command", nargs="?", choices=["shutdown -h now"])
    args = parser.parse_args(argv)
    # Compatible with with_inference_machine(), which passes this exact argument.
    execute = args.execute or args.shutdown_command is not None
    if os.geteuid() != 0 or socket.gethostname().split(".")[0] != "pve-nlp":
        print("Host left on: guard must run as root on pve-nlp.")
        return BUSY
    try:
        config = load_config(args.config)
        # /run is root-owned; a host-wide lock protects independent callers.
        descriptor = os.open("/run/sun-data-inference-shutdown.lock",
                             os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
        with os.fdopen(descriptor, "w") as lock:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
            return guarded_shutdown(config, execute=execute)
    except (OSError, ValueError, KeyError, TypeError):
        print("Host left on: configuration unavailable/unverified or another guard is active.")
        return BUSY
