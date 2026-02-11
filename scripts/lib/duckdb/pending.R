get_pending_titles <- function(
    con,
    to_ingest,
    taxonomy_version,
    prompt_version,
    model,
    key = c("video_id", "title_hash")
) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package `DBI` is required.")
  }
  key <- match.arg(key)

  DBI::dbWriteTable(
    con,
    "staging_videos",
    to_ingest,
    temporary = TRUE,
    overwrite = TRUE
  )

  if (key == "title_hash") {
    pending <- DBI::dbGetQuery(
      con,
      "SELECT s.*
       FROM staging_videos s
       LEFT JOIN classifications c
         ON s.title_hash = c.title_hash
        AND c.taxonomy_version = ?
        AND c.prompt_version = ?
        AND c.model = ?
       WHERE c.title_hash IS NULL",
      params = list(taxonomy_version, prompt_version, model)
    )
  } else {
    pending <- DBI::dbGetQuery(
      con,
      "SELECT s.*
       FROM staging_videos s
       LEFT JOIN classifications c
         ON s.video_id = c.video_id
        AND c.taxonomy_version = ?
        AND c.prompt_version = ?
        AND c.model = ?
       WHERE c.video_id IS NULL",
      params = list(taxonomy_version, prompt_version, model)
    )
  }

  pending
}
