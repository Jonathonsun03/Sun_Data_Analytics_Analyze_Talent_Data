normalize_title <- function(x) {
  x %>%
    enc2utf8() %>%
    stringr::str_squish() %>%
    stringr::str_to_lower()
}

build_to_ingest <- function(Classification, talent_id, talent_name) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package `digest` is required.")
  }

  Classification %>%
    dplyr::transmute(
      video_id = as.character(`Video ID`),
      talent_id = talent_id,
      talent_name = talent_name,
      title_raw = as.character(Title),
      title_norm = normalize_title(Title),
      title_hash = digest::digest(
        paste(talent_id, normalize_title(Title), sep = "||"),
        algo = "xxhash64"
      ),
      content_type = as.character(`Content Type`),
      published_at = as.POSIXct(`Published At`, tz = "UTC")
    )
}

upsert_videos <- function(con, to_ingest) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package `DBI` is required.")
  }

  DBI::dbExecute(
    con,
    "INSERT INTO videos (
       video_id, talent_id, talent_name, title_raw, title_norm, title_hash,
       content_type, published_at, last_seen
     )
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
     ON CONFLICT (video_id, talent_id) DO UPDATE SET
       title_raw = excluded.title_raw,
       title_norm = excluded.title_norm,
       title_hash = excluded.title_hash,
       content_type = excluded.content_type,
       published_at = excluded.published_at,
       talent_name = excluded.talent_name,
       last_seen = CURRENT_TIMESTAMP",
    params = list(
      to_ingest$video_id,
      to_ingest$talent_id,
      to_ingest$talent_name,
      to_ingest$title_raw,
      to_ingest$title_norm,
      to_ingest$title_hash,
      to_ingest$content_type,
      to_ingest$published_at
    )
  )
}
