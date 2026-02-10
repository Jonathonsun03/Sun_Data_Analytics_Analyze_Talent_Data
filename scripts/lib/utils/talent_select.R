normalize_talent_path <- function(x) {
  x <- str_replace_all(
    x,
    c("<e3><80><90>" = "\u3010", "<e3><80><91>" = "\u3011")
  )
  x <- enc2utf8(x)
  normalizePath(x, winslash = "/", mustWork = FALSE)
}

list_talents <- function(root = get_staging_root()) {
  paths <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  tibble(
    name = enc2utf8(basename(paths)),
    path = normalize_talent_path(paths)
  ) %>%
    arrange(name)
}

select_talent <- function(query, root = get_staging_root(), ignore_case = TRUE) {
  talents <- list_talents(root)
  if (is.numeric(query)) {
    if (length(query) != 1 || query < 1 || query > nrow(talents)) {
      stop("Numeric query out of range.")
    }
    return(talents$path[[query]])
  }

  q <- enc2utf8(as.character(query))
  name_match <- if (ignore_case) {
    str_to_lower(talents$name) == str_to_lower(q)
  } else {
    talents$name == q
  }

  if (any(name_match)) {
    return(talents$path[[which(name_match)[1]]])
  }

  partial_match <- if (ignore_case) {
    str_detect(str_to_lower(talents$name), str_to_lower(q))
  } else {
    str_detect(talents$name, q)
  }

  hits <- talents[partial_match, , drop = FALSE]
  if (nrow(hits) == 1) return(hits$path[[1]])
  if (nrow(hits) == 0) stop("No talent matched query: ", q)

  stop(
    "Multiple talents matched query: ",
    q,
    ". Matches: ",
    paste(hits$name, collapse = ", ")
  )
}
