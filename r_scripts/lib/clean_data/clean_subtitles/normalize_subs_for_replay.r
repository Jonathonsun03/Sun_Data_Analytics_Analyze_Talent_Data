normalize_subs_for_replay <- function(subs, file_path = NULL, drop_pause = TRUE) {
  nm <- names(subs)

  pick <- function(cands) {
    i <- match(tolower(cands), tolower(nm), nomatch = 0)
    i <- i[i > 0]
    if (length(i) == 0) NA_character_ else nm[i[1]]
  }

  video_col <- pick(c("VideoID", "video_id"))
  start_col <- pick(c("start_sec", "start_sec_clean", "start_time", "start"))
  stop_col <- pick(c("stop_sec", "end_sec", "stop_sec_clean", "stop_time", "end"))
  text_col <- pick(c("Text", "text"))
  unit_type_col <- pick(c("unit_type", "UnitType"))

  if (is.na(text_col) || is.na(start_col)) {
    stop("Subtitle data missing text/start columns.")
  }

  if (is.na(video_col)) {
    if (is.null(file_path)) stop("Missing VideoID and no file_path provided.")
    vid <- stringr::str_match(basename(file_path), "_([A-Za-z0-9_-]{11})_subtitles\\.csv$")[, 2]
    if (is.na(vid) || vid == "") {
      stop("Could not extract VideoID from file name: ", basename(file_path))
    }
    subs$VideoID <- vid
    video_col <- "VideoID"
  }

  out <- dplyr::transmute(
    subs,
    VideoID = as.character(.data[[video_col]]),
    start_sec = as.numeric(period_to_seconds(.data[[start_col]])),
    stop_sec = if (!is.na(stop_col)) as.numeric(period_to_seconds(.data[[stop_col]])) else NA_real_,
    Text = stringr::str_squish(as.character(.data[[text_col]])),
    unit_type = if (!is.na(unit_type_col)) as.character(.data[[unit_type_col]]) else NA_character_
  ) %>%
    dplyr::mutate(
      stop_sec = dplyr::if_else(is.na(.data$stop_sec), .data$start_sec, .data$stop_sec),
      stop_sec = pmax(.data$stop_sec, .data$start_sec, na.rm = TRUE)
    ) %>%
    dplyr::filter(!is.na(.data$start_sec), !is.na(.data$VideoID), .data$VideoID != "") %>%
    dplyr::filter(!is.na(.data$Text), .data$Text != "")

  if (isTRUE(drop_pause)) {
    out <- out %>%
      dplyr::filter(
        !(.data$unit_type %in% c("PAUSE", "pause")) &
          !stringr::str_detect(.data$Text, "^\\[PAUSE\\]$|^\\[MUSIC\\]$|^\\[APPLAUSE\\]$")
      )
  }

  out %>%
    dplyr::select(.data$VideoID, .data$start_sec, .data$stop_sec, .data$Text) %>%
    dplyr::arrange(.data$VideoID, .data$start_sec, .data$stop_sec)
}
