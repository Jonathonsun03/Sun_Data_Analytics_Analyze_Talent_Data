# Subtitle backfill library

Backfill functions are separated by responsibility:

| File | Responsibility |
| --- | --- |
| `subtitle_backfill_tracks.R` | Lists noncurrent tracks, loads sources, and rechecks checksums. |
| `subtitle_backfill_checkpoints.R` | Builds, loads, validates, and saves checkpoints. |
| `subtitle_backfill_database.R` | Records canonical run and attempt status. |
| `subtitle_backfill_access.R` | Short read/write connections, collection waits, and lock retries. |
| `subtitle_backfill_inference.R` | Runs or reuses punctuation for each block. |
| `subtitle_backfill_speakers.R` | Walks adjacent sentences and assigns inferred speaker turns. |
| `subtitle_speaker_client.R` | Qwen requests, prompt, and stop-aware retries. |
| `subtitle_speaker_checkpoints.R` | Atomic cache of successful speaker comparisons. |
| `subtitle_backfill_reconstruction.R` | Validates and publishes one reconstructed video. |
| `subtitle_backfill_runtime.R` | Loads runner settings and waits on collection. |
| `subtitle_backfill_controls.R` | Dashboard controls and monotonic runtime deadline. |
| `subtitle_backfill_batch.R` | Decides whether one candidate video needs reconstruction. |

The executable entrypoint is
`r_scripts/run/Subtitle_clean/backfill_subtitle_sentences.R`. The public Bash
wrapper is `bin/linux/subtitles/run_subtitle_sentence_backfill.sh`. Executable
batches with pending work perform the inference API readiness and conditional
wake-on-LAN check once before entering the per-video loop.

Each track actually entered by a batch is recorded in
`ops.subtitle_backfill_attempts` before processing starts and updated with its
terminal status, metrics, publication run ID, and error. This is the canonical
batch-membership source for operational reporting; the aggregate
`ops.pipeline_runs.error_summary` remains a concise run summary.

A cooperative dashboard stop request is checked between inference blocks,
speaker comparisons, retries, and videos. Completed checkpoint files are retained,
the batch is recorded as `cancelled`, and a later run resumes only incomplete work.
Existing lakehouses constrain attempt statuses, so the interrupted video is
recorded as `failed` with an explicit dashboard-stop reason, without a schema migration.
The shell wrapper owns run-token markers and exit cleanup in the narrow shared
`Logs/subtitle_sentence_backfill/control` directory; the dashboard checks its lock
before reporting that the run stopped.

The per-video flow is:

1. `subtitle_backfill_tracks.R` computes source checksums in the initial DuckDB
   query and excludes tracks already current for that checksum and pipeline.
2. `subtitle_backfill_batch.R` loads each selected source track, rechecks its
   current status, and loads database checkpoints.
3. `subtitle_backfill_inference.R` reuses checkpoints or requests missing
   punctuation blocks, saving each successful response through
   `subtitle_backfill_checkpoints.R`.
4. `subtitle_backfill_database.R` opens a short write connection so the
   reconstruction can validate and publish through
   `r_scripts/lib/duckdb/subtitle_sentence_publish.R`.

After punctuation reconstruction, Qwen 3B evaluates adjacent sentences only
inside the same source-derived turn. Explicit `>>` boundaries are always
preserved. The cumulative result is published as `inferred_speaker_turn_id`;
`speaker_turn_id` and `speaker_change` retain the source-caption interpretation.

The shared DuckDB table definitions remain in
`r_scripts/lib/duckdb/subtitle_sentence_schema.R` because they define canonical
storage rather than backfill execution.

## Resuming within a daily runtime budget

Executable runs default to a 14,400-second (four-hour) processing budget from
runner configuration, including collection waits and inference startup. Override
with `SUBTITLE_BACKFILL_MAX_RUNTIME_SEC` or the wrapper's `--max-runtime-sec`;
zero explicitly disables it. Dry runs do not use a deadline. This is a per-run
budget, not a quota aggregated across manual restarts. A normal 07:00 Eastern
start requests a graceful stop around 11:00; a missed-run catch-up gets four
hours from its own start.

The deadline shares the cooperative dashboard stop condition. It is checked
between videos, punctuation blocks, speaker comparisons and retries, and while
waiting for database access. An in-flight model request may finish up to its
configured timeout; completed work is saved before the next stop check. A
transaction already executing is allowed to finish. Terminal bookkeeping has a
bounded collection wait and lock retries, then the inference reservation is
released. Consequently this is four hours plus drain/cleanup time, not SIGKILL
at exactly four hours.

Punctuation retains its existing checkpoint format. Each video's adjacent
`.rds.speakers.rds` file stores successful classifications and an identity hash
covering the source checksum, track, pipeline, reconstructed sentences, model,
endpoint, and prompt. Each result is saved atomically before proceeding. Changed
identity or `--force` prevents reuse; failed requests are never cached. Turn IDs
are rebuilt from cached labels and explicit boundaries on resume. Direct callers
must supply `speaker_cache_identity` to opt into persistent speaker reuse.

All runner reads and writes, including run/attempt bookkeeping, now observe the
collection marker and retry lock conflicts. This is cooperative priority, not a
cross-process transaction lock: another process can start after the marker
check, so retries remain necessary. No global coordinator or database migration
is introduced. The runner still closes DuckDB before inference. Terminal status
writes can fail if collection or another database user outlasts the cleanup grace;
saved inference checkpoints survive those failures.

Focused checks: `test_subtitle_resume_budget.R`, `test_subtitle_speaker_turns.R`,
`test_subtitle_backfill_runner_controls.R`, and `test_subtitle_sentence_backfill.R`
under `r_scripts/tests/`.

The existing reconstruction function retains its longer signature and sequential
validation/publication flow to preserve callers; cache, transport, access, and
stop logic are extracted into the focused modules above rather than expanded
inline. The existing checkpoint module retains its database row-schema helpers
and file interface; new speaker persistence is in its own module.

## Read-only failure audit

Run `r_scripts/run/Subtitle_clean/audit_subtitle_backfill.R` from the repository
root using the scheduled service's R executable, library paths, and environment.
It prints the Git revision, helper fingerprints, R/package versions, stage
timings, and active call names on failure. It reuses existing punctuation
checkpoints and suppresses checkpoint writes and publication. Missing or invalid
cached punctuation fails the audit rather than requesting new punctuation.

```bash
Rscript --vanilla r_scripts/run/Subtitle_clean/audit_subtitle_backfill.R LcqXuzMFT8g mock
Rscript --vanilla r_scripts/run/Subtitle_clean/audit_subtitle_backfill.R LcqXuzMFT8g live 1
```

Mock mode exercises the speaker-turn loop with `uncertain` responses for uncached
comparisons. Live mode sends at most the specified number of uncached speaker
comparisons (default one), with one attempt and a maximum 30-second timeout per
request; remaining comparisons use mock responses. Live mode requires an already
available API. Audit results are diagnostic only, not publishable coding or
speaker-turn results. Redirect logs to a DataLake output location when retaining
or comparing runs across machines.

## Per-talent processing pause

The shared lifecycle policy filters paused talent codes at discovery and rechecks
between videos, punctuation blocks, speaker comparisons/retries, and before
publication. A pause preserves checkpoints and drains the current batch safely.
The next run skips paused talents. Archive alone does not pause this workflow.
Read `py_scripts/lib/talent_lifecycle/README.md` for the shared mount setup.
