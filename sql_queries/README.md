# SQL Queries

This directory stores reusable, language-neutral SQL queries shared by Python,
R, notebooks, and reporting workflows.

## Purpose

- Keep SQL in `.sql` files rather than embedding the same query in several
  Python or R files.
- Use bound parameters for values such as talent codes, dates, and date ranges.
- Keep Python and R responsible for loading queries, binding parameters, and
  returning language-specific data frames.
- Keep these executable queries separate from natural-language prompts under
  `prompts/`.

## Organization

Queries are organized first by database, then by subject area. The current
library is:

```text
sql_queries/
  unified_db/
    daily/
      public_subscribers.sql
      subscriber_daily.sql
      video_analytics.sql
      video_demographics.sql
      video_geography.sql
      video_monetary.sql
    metadata/
      list_tables.sql
    talents/
      find_talent.sql
      sample_talents.sql
```

Add another subject directory only when implemented queries need it. Do not
create empty placeholder query files.

## Python loading

Use the shared loader rather than building repository paths in each script:

```python
from py_scripts.lib.utils.sql_queries import load_sql_query

query = load_sql_query("unified_db", "daily", "video_analytics.sql")
data = connection.execute(query, [talent_code, selected_date]).fetchdf()
```

R can read the same UTF-8 `.sql` files and pass the query text to
`DBI::dbGetQuery()` with `params`. Language-specific connection and result
handling remains under `py_scripts/lib/` and `r_scripts/lib/`.

## Query conventions

- Use `snake_case.sql` filenames that describe the result.
- Prefer one reusable query per file.
- Use `?` placeholders for DuckDB values instead of string interpolation.
- Qualify tables with their schema, such as
  `clean.video_analytics_snapshots` or `main.talents`.
- Include a short SQL comment describing the result grain and parameters.
- Do not put credentials, machine-specific paths, or generated data in SQL
  files.

Example:

```sql
-- One row per video for a talent on a snapshot date.
-- Parameters: 1 = talent_code, 2 = snapshot_date.
SELECT *
FROM clean.video_analytics_snapshots
WHERE talent_code = ?
  AND snapshot_date = CAST(? AS DATE)
ORDER BY video_id;
```
