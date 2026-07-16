-- One row per table in the unified DuckDB database.
-- Parameters: none.
SELECT
  schema_name,
  table_name
FROM duckdb_tables()
ORDER BY
  schema_name,
  table_name;
