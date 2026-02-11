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
