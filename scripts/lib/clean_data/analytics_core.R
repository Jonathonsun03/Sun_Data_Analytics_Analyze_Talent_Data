.normalize_talent_files <- function(files, talent_index = NULL) {
  if (is.data.frame(files)) return(files)
  if (!is.list(files)) {
    stop("`files` must be a data frame or a list returned by TalentFiles().")
  }

  # If this already looks like a per-type list, return as-is.
  if (!is.null(names(files)) && any(nzchar(names(files)))) {
    return(files)
  }

  # If TalentFiles() was called on a single path, unwrap the single element.
  if (length(files) == 1 && is.list(files[[1]])) {
    return(files[[1]])
  }

  # If multiple talents were passed, require an explicit index.
  if (!is.null(talent_index)) {
    if (!is.numeric(talent_index) || length(talent_index) != 1) {
      stop("`talent_index` must be a single numeric index.")
    }
    if (talent_index < 1 || talent_index > length(files)) {
      stop("`talent_index` out of range for provided `files` list.")
    }
    return(files[[talent_index]])
  }

  stop(
    "Multiple talent folders detected. ",
    "Pass a single talent folder to TalentFiles(), or provide `talent_index`."
  )
}

.get_type_data <- function(files, type, talent_index = NULL) {
  files <- .normalize_talent_files(files, talent_index)

  if (!is.list(files) || is.null(names(files)) || !(type %in% names(files))) {
    stop("Type not found in `files`: ", type)
  }

  dfs <- files[[type]]
  if (is.data.frame(dfs)) return(dfs)
  if (is.list(dfs)) return(dplyr::bind_rows(dfs))

  stop("Unexpected data structure for type: ", type)
}

prepare_analytics <- function(files,
                              talent = NULL,
                              analytics_type = "video_analytics",
                              talent_index = NULL) {

  if (is.null(talent)) talent <- "unknown"
  message("Preparing analytics data for: ", talent,
          " (type = ", analytics_type, ")")

  # 1) Get raw analytics for a given talent
  analytics_raw <- .get_type_data(
    files = files,
    type = analytics_type,
    talent_index = talent_index
  )
  
  # 2) Keep only useful columns & deduplicate by video/channel
  analytics_clean <- analytics_raw %>%
    dplyr::select(
      dplyr::all_of(c(
        "Video ID",
        "Channel ID",
        "Channel Name",
        "Title",
        "Published At",
        "date",
        "Content Type",
        "views",
        "estimatedMinutesWatched",
        "averageViewDuration",
        "averageViewPercentage",
        "subscribersGained",
        "subscribersLost"
      )),
      dplyr::any_of(c(
        "Estimated Revenue",
        "CPM",
        "DurationSeconds",
        "DurationISO"
      ))
    ) %>%
    dplyr::distinct(`Video ID`, `Channel ID`, `Channel Name`, .keep_all = TRUE)
  
  # 3) Return cleaned analytics (stream titles not available yet)
  analytics_clean
}
