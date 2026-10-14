# NLP inference library

Shared R clients and lifecycle support for the NLP inference container and its
physical host. Load the public interface with:

```r
source(here::here("r_scripts", "lib", "nlp_inference", "load.R"))
with_inference_machine({
  punctuate_text("text to punctuate")
})
```

| Module | Responsibility |
| --- | --- |
| `load.R` | Explicit dependency order; definitions only, in the caller's environment. |
| `config.R` | Environment defaults, validation, and punctuation URL. |
| `transport.R` | SSH execution and bounded readiness polling. |
| `api.R` | API health and batch reservation/release calls. |
| `machine.R` | Conditional wake-on-LAN, container startup, readiness, and power-down confirmation. |
| `locks.R` | Process lock, atomic ownership metadata, and stale reservation recovery. |
| `cleanup.R` | Reservation release and optional guarded shutdown at batch exit. |
| `lifecycle.R` | Shared nested scope and startup orchestration. |
| `punctuation.R` | Punctuation requests and response validation. |

Public functions retain their existing names: `with_inference_machine()`,
`ensure_inference_machine()`, `inference_punctuation_url()`, `punctuate_text()`,
and `parse_punctuation_response()`. The old utility and subtitle punctuation
paths remain forwarding loaders for existing scripts. New callers should use
`load.R`. Modules share the caller's environment so scoped loading and test
doubles work; repeated loading preserves an active nested lifecycle.

Startup acquires the process lock, recovers any identified stale reservation,
persists a new token, ensures machine readiness, then reserves the batch.
Container status/start failures during host boot are retried within the configured
readiness timeout; a concurrent starter is accepted once the CT is running.
Cleanup releases the reservation before applying optional shutdown policy.
Process death releases the kernel lock; the next run uses the persisted token
to clear stale runtime state. Legacy locks without metadata still need manual
inspection. See [the R runtime documentation](../../README.md#inference-machine-power-lifecycle)
for environment settings, dependencies, and recovery/shutdown semantics.

Keep domain processing outside this library: subtitle reconstruction,
checkpoint publication, and lakehouse writes remain in their existing modules.

Run focused checks from the repository root:

```bash
Rscript --vanilla r_scripts/tests/test_inference_machine.R
Rscript --vanilla r_scripts/tests/test_subtitle_sentence_reconstruction.R
```

Lifecycle checks mock remote calls and use temporary locks, including actual
worker termination. They do not wake, reserve, or shut down the production host.
