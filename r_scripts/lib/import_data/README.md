# import_data

## TalentFiles

Defined in `talent_files.R`.

### Purpose

Load talent CSV files from one or more folders, attach a `date` column parsed from the filename, and group the loaded data frames by file "type" (filename with the trailing `_YYYY-MM-DD` removed).

### Inputs

`Paths` can be one of:

- A character vector of directory paths. For each directory, the function lists files directly inside (non-recursive) and keeps `.csv` files.
- A list where each element is a character vector of file paths (already enumerated). In this case, no directory listing is performed.

If you want to pass a datalake root or staging root, ensure those roots directly contain the CSV files you want to load. The function does **not** traverse subdirectories.

### Output

A list (one element per input folder) where each element is a list of data frames split by type.

### Notes

- Missing paths are warned and skipped.
- The `date` column is parsed from the first `YYYY-MM-DD` found in the filename; if not found, `date` is `NA`.
- `latest_talent_snapshot_path()` selects the newest file for one snapshot type,
  using the filename date, modification time, and filename as deterministic
  descending sort keys.

## Text Playback Stream Lookup

Defined in `text_playback_streams.R`.

Use `list_text_playback_streams()` to join `text_playback/*.csv` files to title metadata by `Video ID`. Use `find_text_playback_streams()` when you know a title fragment and want the matching replay CSV path.

```r
source(here::here("r_scripts", "lib", "clean_data", "CleanData.R"))
source(here::here("r_scripts", "lib", "import_data", "text_playback_streams.R"))

matches <- find_text_playback_streams(
  "GSSR",
  talent = "Nova"
)

replay <- read_text_playback_stream(matches, row = 1)
```

## Qualitative Transcript Coding

Defined in `qualitative_transcripts.R`.

`load_qualitative_transcripts_wide()` is the supported analysis interface for
versioned qualitative coding stored in the unified talent lakehouse. It resolves
the codebook-specific wide view, joins the exact coded transcript text, and
returns the code columns as R logical vectors.

The loader also retrieves video/talent dimensions from `catalog` and chat
payment metadata from `text.chat_messages`. These fields are joined at read
time; they are not duplicated in the qualitative coding tables.

```r
source(here::here("r_scripts", "lib", "utils", "datalake_root.r"))
source(here::here("r_scripts", "lib", "duckdb", "db_connect.R"))
source(here::here(
  "r_scripts",
  "lib",
  "import_data",
  "qualitative_transcripts.R"
))

qualitative_data <- load_qualitative_transcripts_wide(
  codebook_id = "chat_monetary_growth_v1",
  dataset_id = "chat_monetary_growth_variance_30_video",
  pipeline_run_id = "latest",
  response_status = NULL
)
```

Use `response_status = "coded"` to exclude missing or invalid coding responses.
Use an explicit `pipeline_run_id` instead of `"latest"` when an analysis must
pin one coding run. `load_qualitative_codebook()` returns the matching codebook
definitions from DuckDB. `load_qualitative_video_performance()` retrieves the
matching rows from `analytics.video_latest_performance`.

Do not add a separate loader per codebook. New codebooks are resolved through
the metadata in `qualitative.codebooks`, and their generated wide views are
consumed by the same `load_qualitative_transcripts_wide()` function.
