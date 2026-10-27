# Lifecycle subsystem

- Read `README.md` before changing policy or dashboard integration.
- Keep archive (retrieval), pause (processing), and removal (deletion) distinct.
- The shared control files are the only lifecycle authority. Do not repurpose
  `catalog.talents.active`, OAuth status, or client permission fields.
- Keep this package dependency-free; the R reader must accept the same contract.
- Do not introduce a daemon, queue platform, or new database for these switches.
- Never delete data or revoke credentials as a side effect of a switch change.
- Check malformed state, stale edits, and preservation/resume behavior in the
  focused tests when changing this contract.
