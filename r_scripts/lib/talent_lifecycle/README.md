# R talent lifecycle integration

`policy.R` reads the shared lifecycle files and exposes `talent_work_allowed()`,
`talent_assert_processing()`, and `talent_processing_filter()`. It never caches
policy or writes runtime state when sourced.

`dashboard.R` provides the internal admin UI and calls the Python atomic writer.
It reads catalog identities through a short read-only connection; it does not
grant the dashboard a writable lakehouse connection.

See `py_scripts/lib/talent_lifecycle/README.md` for the single contract,
deployment steps, boundaries, and separate removal requirements. Keep contract
changes compatible across the R reader and Python package.
