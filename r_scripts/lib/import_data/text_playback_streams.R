text_playback_extract_video_id <- function(path) {
  base <- basename(as.character(path))
  match <- regexec("_([A-Za-z0-9_-]{11})\\.csv$", base)
  found <- regmatches(base, match)

  vapply(
    found,
    function(x) {
      if (length(x) >= 2L) {
        x[[2]]
      } else {
        NA_character_
      }
    },
    character(1)
  )
}

text_playback_parse_published_date <- function(x) {
  x <- trimws(as.character(x))
  x[!nzchar(x)] <- NA_character_
  out <- rep(as.Date(NA), length(x))

  patterns <- list(
    "%Y-%m-%d" = "^\\d{4}-\\d{2}-\\d{2}$",
    "%m-%d-%Y" = "^\\d{1,2}-\\d{1,2}-\\d{4}$",
    "%m/%d/%Y" = "^\\d{1,2}/\\d{1,2}/\\d{4}$",
    "%Y/%m/%d" = "^\\d{4}/\\d{1,2}/\\d{1,2}$"
  )

  for (fmt in names(patterns)) {
    missing <- is.na(out) & !is.na(x) & grepl(patterns[[fmt]], x)
    if (any(missing)) {
      out[missing] <- suppressWarnings(as.Date(x[missing], format = fmt))
    }
  }

  out
}

default_text_playback_talent_data_root <- function(talent_data_root = NULL) {
  if (!is.null(talent_data_root) && nzchar(trimws(talent_data_root))) {
    return(normalizePath(talent_data_root, winslash = "/", mustWork = FALSE))
  }

  if (exists("get_datalake_root", mode = "function")) {
    return(normalizePath(get_datalake_root(), winslash = "/", mustWork = FALSE))
  }

  env_root <- Sys.getenv("TALENT_DATALAKE_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = FALSE))
  }

  if (.Platform$OS.type == "windows") {
    return("Z:/DataLake/Sun_Data_Analytics/Talent_data")
  }

  candidates <- c(
    "/mnt/datalake/DataLake/Sun_Data_Analytics/Talent_data",
    "/mnt/router_data/DataLake/Sun_Data_Analytics/Talent_data",
    "/mnt/datalake/Datalake/Sun_Data_Analytics/Talent_data"
  )
  existing <- candidates[dir.exists(candidates)]
  if (length(existing) > 0L) {
    return(existing[[1]])
  }

  stop("Datalake Talent_data root not found. Set TALENT_DATALAKE_ROOT.", call. = FALSE)
}

text_playback_talent_key <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- tolower(x)
  gsub("[^a-z0-9]+", "", x)
}

resolve_text_playback_talent_path <- function(talent, talent_data_root = NULL) {
  root <- default_text_playback_talent_data_root(talent_data_root)

  if (exists("select_talent", mode = "function")) {
    return(as.character(select_talent(talent, root = root)))
  }

  paths <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  names <- basename(paths)
  q <- text_playback_talent_key(talent)
  keys <- text_playback_talent_key(names)

  exact <- paths[keys == q]
  if (length(exact) == 1L) {
    return(exact[[1]])
  }

  partial <- paths[grepl(q, keys, fixed = TRUE)]
  if (length(partial) == 1L) {
    return(partial[[1]])
  }
  if (length(partial) > 1L) {
    stop(
      "Multiple talents matched query: ", talent, ". Matches: ",
      paste(basename(partial), collapse = ", "),
      call. = FALSE
    )
  }

  stop("No talent matched query in datalake Talent_data root: ", talent, call. = FALSE)
}

resolve_text_playback_path <- function(
  talent = NULL,
  text_playback_path = NULL,
  talent_data_root = NULL,
  must_exist = TRUE
) {
  if (!is.null(text_playback_path) && nzchar(trimws(text_playback_path))) {
    out <- normalizePath(text_playback_path, winslash = "/", mustWork = FALSE)
    if (isTRUE(must_exist) && !dir.exists(out)) {
      stop("text_playback path does not exist: ", out, call. = FALSE)
    }
    return(out)
  }

  if (is.null(talent) || !nzchar(trimws(talent))) {
    stop("Provide either `talent` or `text_playback_path`.", call. = FALSE)
  }
  talent_path <- resolve_text_playback_talent_path(
    talent = talent,
    talent_data_root = talent_data_root
  )

  if (length(talent_path) != 1L) {
    stop(
      "`talent` resolved to ", length(talent_path), " paths; pass a more specific ",
      "talent name or `text_playback_path`.",
      call. = FALSE
    )
  }

  out <- normalizePath(file.path(talent_path[[1]], "text_playback"), winslash = "/", mustWork = FALSE)
  if (isTRUE(must_exist) && !dir.exists(out)) {
    stop("text_playback path does not exist: ", out, call. = FALSE)
  }
  out
}

list_text_playback_streams <- function(
  talent = NULL,
  text_playback_path = NULL,
  titles = NULL,
  talent_data_root = NULL,
  content_type = NULL
) {
  path <- resolve_text_playback_path(
    talent = talent,
    text_playback_path = text_playback_path,
    talent_data_root = talent_data_root,
    must_exist = TRUE
  )

  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  index <- tibble::tibble(
    source_path = normalizePath(files, winslash = "/", mustWork = FALSE),
    source_file = basename(files),
    `Video ID` = text_playback_extract_video_id(files)
  )

  if (is.null(titles)) {
    if (!exists("load_title_classifications", mode = "function")) {
      stop(
        "`load_title_classifications()` is not loaded. Source CleanData.R or pass ",
        "a title data frame via `titles`.",
        call. = FALSE
      )
    }
    titles <- load_title_classifications(talent = talent)
  }

  if (!is.data.frame(titles)) {
    stop("`titles` must be a data frame returned by `load_title_classifications()`.", call. = FALSE)
  }
  if (!("Video ID" %in% names(titles))) {
    if ("video_id" %in% names(titles)) {
      titles <- dplyr::rename(titles, `Video ID` = .data$video_id)
    } else {
      stop("`titles` must include `Video ID` or `video_id`.", call. = FALSE)
    }
  }

  out <- index %>%
    dplyr::filter(!is.na(.data$`Video ID`), nzchar(.data$`Video ID`)) %>%
    dplyr::left_join(titles, by = "Video ID") %>%
    dplyr::mutate(
      published_date = text_playback_parse_published_date(.data$published_at),
      title_search = tolower(paste(.data$title_raw, .data$source_file))
    )

  if (!is.null(content_type) && length(content_type) > 0L) {
    keep_types <- tolower(trimws(as.character(content_type)))
    out <- out %>%
      dplyr::filter(tolower(trimws(as.character(.data$content_type))) %in% keep_types)
  }

  out %>%
    dplyr::arrange(dplyr::desc(.data$published_date), .data$title_raw, .data$source_file)
}

find_text_playback_streams <- function(
  query,
  talent = NULL,
  text_playback_path = NULL,
  titles = NULL,
  talent_data_root = NULL,
  content_type = "live",
  ignore_case = TRUE
) {
  if (missing(query) || is.null(query) || !nzchar(trimws(query))) {
    stop("`query` must be a non-empty title search string.", call. = FALSE)
  }

  streams <- list_text_playback_streams(
    talent = talent,
    text_playback_path = text_playback_path,
    titles = titles,
    talent_data_root = talent_data_root,
    content_type = content_type
  )

  needle <- trimws(as.character(query[[1]]))
  haystack <- paste(streams$title_raw, streams$source_file)
  if (isTRUE(ignore_case)) {
    needle <- tolower(needle)
    haystack <- tolower(haystack)
  }

  streams %>%
    dplyr::filter(grepl(needle, haystack, fixed = TRUE)) %>%
    dplyr::select(
      dplyr::any_of(c(
        "title_raw",
        "content_type",
        "published_at",
        "published_date",
        "Video ID",
        "source_file",
        "source_path",
        "talent_name",
        "topic",
        "tags",
        "primary_reference"
      ))
    )
}

read_text_playback_stream <- function(stream, row = 1L, ...) {
  if (is.data.frame(stream)) {
    if (!("source_path" %in% names(stream))) {
      stop("`stream` data frame must include `source_path`.", call. = FALSE)
    }
    path <- stream$source_path[[row]]
  } else {
    path <- as.character(stream[[1]])
  }

  readr::read_csv(path, show_col_types = FALSE, ...)
}

list_text_playback_talents <- function(talent_data_root = NULL) {
  root <- default_text_playback_talent_data_root(talent_data_root)
  paths <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  text_paths <- file.path(paths, "text_playback")

  tibble::tibble(
    talent_name = basename(paths),
    talent_path = normalizePath(paths, winslash = "/", mustWork = FALSE),
    text_playback_path = normalizePath(text_paths, winslash = "/", mustWork = FALSE),
    text_playback_exists = dir.exists(text_paths),
    text_playback_file_count = vapply(
      text_paths,
      function(path) {
        if (!dir.exists(path)) {
          return(0L)
        }
        length(list.files(path, pattern = "\\.csv$", full.names = TRUE))
      },
      integer(1)
    )
  ) %>%
    dplyr::filter(.data$text_playback_exists, .data$text_playback_file_count > 0L) %>%
    dplyr::arrange(.data$talent_name)
}
