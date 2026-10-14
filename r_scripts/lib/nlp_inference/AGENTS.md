# NLP inference library

- Read README.md and `r_scripts/README.md` before editing. Keep the README module map current.
- Keep configuration, SSH transport, API calls, machine readiness, runtime ownership, cleanup, and batch orchestration in their respective modules.
- `load.R` only sources definitions into the caller's environment. Preserve nested batch state when the library is sourced again.
- Keep modules under 200 lines and aim for functions under 60 lines. Extract named operations before enlarging orchestration; document any coherent-algorithm exception in the README.
- New NLP model clients belong in focused client modules here. Keep subtitle reconstruction and database publication in their existing libraries.
- Preserve the public lifecycle and punctuation interfaces. Do not duplicate lifecycle logic in callers or compatibility loaders.
- Persist reservation identity before reserving remotely. Hold the process lock through cleanup; recover only matching saved ownership under that lock. Retain metadata when release fails.
- Keep recovery separate from shutdown policy. Health means readiness, not idleness. Never infer permission to shut down from a local worker's death.
- Exercise lifecycle changes with mocked remote calls and temporary locks, including normal exit, startup failure, contention, SIGTERM/SIGKILL, and recovery failure. Do not test by launching production processing or changing host power state.
