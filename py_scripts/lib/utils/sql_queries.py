from __future__ import annotations

from pathlib import Path

from py_scripts.lib.utils.repo_paths import find_repo_root


SQL_QUERIES_ROOT = find_repo_root(Path(__file__)) / "sql_queries"


def sql_query_path(*parts: str | Path) -> Path:
    """Return a validated path within the shared SQL query library."""
    query_path = (SQL_QUERIES_ROOT.joinpath(*parts)).resolve()
    if not query_path.is_relative_to(SQL_QUERIES_ROOT):
        raise ValueError(f"SQL query path is outside {SQL_QUERIES_ROOT}: {query_path}")
    if query_path.suffix.lower() != ".sql":
        raise ValueError(f"SQL query must use the .sql extension: {query_path}")
    if not query_path.is_file():
        raise FileNotFoundError(f"SQL query not found: {query_path}")
    return query_path


def load_sql_query(*parts: str | Path) -> str:
    """Load a UTF-8 SQL query from the shared query library."""
    return sql_query_path(*parts).read_text(encoding="utf-8").strip()
