# Transcript cleaning notebooks

This folder contains executable documentation for the subtitle-to-sentence
pipeline.

- `pipeline_overview.qmd` explains the complete architecture, storage model,
  quality gates, and remaining production backfill work.
- `stream_example.qmd` runs a real livestream excerpt through deterministic
  overlap cleaning and the FullStop service.
- `short_example.qmd` runs a complete real Short subtitle track through the
  same process.

All ordinary renders are read-only. Example publication is opt-in and writes
only derived rows to an isolated `notebook_*_example` source scope. Raw rows in
`text.subtitle_units` are never changed.

## Render and preview

From the repository root, resolve the configured output root:

```bash
TRANSCRIPT_NOTEBOOK_OUTPUT_ROOT="$(
  Rscript --vanilla -e '
    source("r_scripts/lib/utils/repo_env.R")
    source("r_scripts/lib/utils/datalake_root.r")
    load_repo_env(repo_root = getwd())
    cat(file.path(get_datalake_root(), "Rendered_Notebooks", "transcript_cleaning"))
  '
)"
mkdir -p "$TRANSCRIPT_NOTEBOOK_OUTPUT_ROOT"
```

Render all three notebooks without publishing example rows:

```bash
quarto render r_scripts/notebooks/tests/transcript_cleaning/pipeline_overview.qmd \
  --output-dir "$TRANSCRIPT_NOTEBOOK_OUTPUT_ROOT/pipeline_overview"
quarto render r_scripts/notebooks/tests/transcript_cleaning/stream_example.qmd \
  --output-dir "$TRANSCRIPT_NOTEBOOK_OUTPUT_ROOT/stream_example"
quarto render r_scripts/notebooks/tests/transcript_cleaning/short_example.qmd \
  --output-dir "$TRANSCRIPT_NOTEBOOK_OUTPUT_ROOT/short_example"
```

To preview one rendered notebook without rerunning its pipeline:

```bash
python3 -m http.server 4242 \
  --bind 127.0.0.1 \
  --directory "$TRANSCRIPT_NOTEBOOK_OUTPUT_ROOT"
```
