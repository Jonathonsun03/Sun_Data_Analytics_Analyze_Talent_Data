sanitize_code_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- gsub("_+", "_", x)
  ifelse(grepl("^[0-9]", x), paste0("x_", x), x)
}

build_code_column_lookup <- function(codebook) {
  primary_codes <- codebook %>%
    dplyr::filter(!is.na(.data[["Primary Code ID"]])) %>%
    dplyr::distinct(
      code_id = .data[["Primary Code ID"]],
      code_label = .data[["Primary Code"]]
    )

  secondary_codes <- codebook %>%
    dplyr::filter(!is.na(.data[["Secondary Code ID"]])) %>%
    dplyr::distinct(
      code_id = .data[["Secondary Code ID"]],
      code_label = .data[["Secondary Code"]]
    )

  dplyr::bind_rows(primary_codes, secondary_codes) %>%
    dplyr::filter(!is.na(.data$code_id), !is.na(.data$code_label)) %>%
    dplyr::mutate(
      old_name = paste0("code_", .data$code_id),
      new_name = sanitize_code_name(paste("code", .data$code_id, .data$code_label))
    ) %>%
    dplyr::distinct(.data$old_name, .keep_all = TRUE)
}

build_code_hierarchy_edges <- function(codebook) {
  codebook %>%
    dplyr::filter(
      !is.na(.data[["Primary Code ID"]]),
      !is.na(.data[["Secondary Code ID"]]),
      trimws(.data[["Primary Code ID"]]) != "",
      trimws(.data[["Secondary Code ID"]]) != ""
    ) %>%
    dplyr::transmute(
      primary_code = trimws(.data[["Primary Code ID"]]),
      secondary_code = trimws(.data[["Secondary Code ID"]])
    ) %>%
    dplyr::distinct()
}

select_top_hierarchy_codes <- function(code_counts,
                                       codebook,
                                       available_code_cols,
                                       n = 8L,
                                       prefer = c("secondary", "primary")) {
  prefer <- match.arg(prefer)
  hierarchy_edges <- build_code_hierarchy_edges(codebook)

  ranked_codes <- code_counts %>%
    dplyr::mutate(code_col = paste0("code_", .data$code)) %>%
    dplyr::filter(.data$code_col %in% available_code_cols) %>%
    dplyr::mutate(.rank = dplyr::row_number())

  if (nrow(hierarchy_edges) == 0L || nrow(ranked_codes) == 0L) {
    return(list(
      selected = ranked_codes %>% dplyr::slice_head(n = n),
      excluded = ranked_codes %>% dplyr::slice_head(n = 0),
      hierarchy_edges = hierarchy_edges
    ))
  }

  candidate_codes <- ranked_codes$code

  excluded_codes <- hierarchy_edges %>%
    dplyr::filter(
      .data$primary_code %in% candidate_codes,
      .data$secondary_code %in% candidate_codes
    ) %>%
    dplyr::transmute(
      code = if (identical(prefer, "secondary")) .data$primary_code else .data$secondary_code,
      excluded_for = if (identical(prefer, "secondary")) .data$secondary_code else .data$primary_code,
      hierarchy_preference = prefer
    ) %>%
    dplyr::distinct()

  excluded_ranked_codes <- ranked_codes %>%
    dplyr::inner_join(excluded_codes, by = "code") %>%
    dplyr::arrange(.data$.rank)

  selected_codes <- ranked_codes %>%
    dplyr::anti_join(excluded_codes, by = "code") %>%
    dplyr::slice_head(n = n)

  list(
    selected = selected_codes,
    excluded = excluded_ranked_codes,
    hierarchy_edges = hierarchy_edges
  )
}

rename_code_columns <- function(data, lookup) {
  rename_map <- lookup$new_name
  names(rename_map) <- lookup$old_name
  rename_map <- rename_map[names(rename_map) %in% names(data)]

  data %>%
    dplyr::rename_with(
      .fn = function(x) unname(rename_map[x]),
      .cols = dplyr::all_of(names(rename_map))
    )
}

parse_positive_code_ids <- function(x) {
  if (is.na(x) || trimws(x) == "") {
    return(character())
  }

  strsplit(x, ";", fixed = TRUE)[[1]] %>%
    trimws() %>%
    purrr::discard(~ .x == "") %>%
    unique()
}

codes_to_pairs <- function(codes) {
  codes <- sort(unique(codes))

  if (length(codes) < 2) {
    return(tibble::tibble(code_1 = character(), code_2 = character()))
  }

  pair_matrix <- utils::combn(codes, 2)
  tibble::tibble(code_1 = pair_matrix[1, ], code_2 = pair_matrix[2, ])
}

count_code_pairs_by_window <- function(data,
                                       codes_col = "positive_codes",
                                       group_cols = c("source_file", "video_id"),
                                       window_size_back = 4L) {
  group_cols <- intersect(group_cols, names(data))
  window_size_back <- as.integer(window_size_back)
  if (is.na(window_size_back) || window_size_back < 0L) {
    stop("`window_size_back` must be a non-negative integer.", call. = FALSE)
  }

  if (codes_col %in% names(data)) {
    codes_by_row <- purrr::map(
      data[[codes_col]],
      parse_positive_code_ids
    )
    code_ids <- sort(unique(unlist(codes_by_row, use.names = FALSE)))
    code_matrix <- matrix(
      FALSE,
      nrow = nrow(data),
      ncol = length(code_ids),
      dimnames = list(NULL, code_ids)
    )
    code_index <- stats::setNames(seq_along(code_ids), code_ids)
    for (i in seq_along(codes_by_row)) {
      row_codes <- codes_by_row[[i]]
      if (length(row_codes) > 0L) {
        code_matrix[i, unname(code_index[row_codes])] <- TRUE
      }
    }
  } else {
    wide_code_cols <- grep("^code_", names(data), value = TRUE)
    if (length(wide_code_cols) == 0L) {
      stop(
        "Data must contain `", codes_col,
        "` or at least one wide `code_*` column.",
        call. = FALSE
      )
    }
    code_matrix <- as.matrix(data[wide_code_cols])
    code_matrix[is.na(code_matrix)] <- FALSE
    storage.mode(code_matrix) <- "logical"
    code_ids <- sub("^code_", "", wide_code_cols)
    code_order <- order(code_ids)
    code_ids <- code_ids[code_order]
    code_matrix <- code_matrix[, code_order, drop = FALSE]
  }

  if (length(code_ids) < 2L || nrow(code_matrix) == 0L) {
    return(tibble::tibble(
      code_1 = character(),
      code_2 = character(),
      window_pair_count = integer()
    ))
  }

  groups <- if (length(group_cols) == 0L) {
    rep.int(1L, nrow(data))
  } else {
    group_data <- lapply(data[group_cols], function(x) {
      x <- as.character(x)
      x[is.na(x)] <- "<NA>"
      x
    })
    do.call(
      interaction,
      c(group_data, list(drop = TRUE, lex.order = TRUE))
    )
  }

  window_matrix <- matrix(
    FALSE,
    nrow = nrow(code_matrix),
    ncol = ncol(code_matrix)
  )
  group_rows <- split(seq_len(nrow(data)), groups)
  for (rows in group_rows) {
    group_size <- length(rows)
    for (offset in 0:min(window_size_back, group_size - 1L)) {
      destinations <- rows[seq.int(1L + offset, group_size)]
      sources <- rows[seq_len(group_size - offset)]
      window_matrix[destinations, ] <- window_matrix[destinations, , drop = FALSE] |
        code_matrix[sources, , drop = FALSE]
    }
  }

  pair_index <- utils::combn(seq_along(code_ids), 2L)
  counts <- vapply(seq_len(ncol(pair_index)), function(i) {
    sum(
      window_matrix[, pair_index[1L, i]] &
        window_matrix[, pair_index[2L, i]]
    )
  }, integer(1))
  keep <- counts > 0L

  tibble::tibble(
    code_1 = code_ids[pair_index[1L, keep]],
    code_2 = code_ids[pair_index[2L, keep]],
    window_pair_count = counts[keep]
  ) %>%
    dplyr::arrange(
      dplyr::desc(.data$window_pair_count),
      .data$code_1,
      .data$code_2
    )
}

select_frequent_code_pairs <- function(data,
                                       codebook,
                                       n = 8L,
                                       window_size_back = 4L,
                                       codes_col = "positive_codes",
                                       group_cols = c("source_file", "video_id"),
                                       prefer = c("secondary", "primary")) {
  prefer <- match.arg(prefer)
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L) {
    stop("`n` must be a positive integer.", call. = FALSE)
  }

  code_pair_counts <- count_code_pairs_by_window(
    data = data,
    codes_col = codes_col,
    group_cols = group_cols,
    window_size_back = window_size_back
  )

  code_counts <- code_pair_counts %>%
    dplyr::select(code = code_1, window_pair_count) %>%
    dplyr::bind_rows(
      code_pair_counts %>%
        dplyr::select(code = code_2, window_pair_count)
    ) %>%
    dplyr::group_by(.data$code) %>%
    dplyr::summarise(
      pair_participation_count = sum(.data$window_pair_count),
      distinct_pair_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      dplyr::desc(.data$pair_participation_count),
      dplyr::desc(.data$distinct_pair_count)
    )

  hierarchy_selection <- select_top_hierarchy_codes(
    code_counts = code_counts,
    codebook = codebook,
    available_code_cols = names(data),
    n = n,
    prefer = prefer
  )

  list(
    selected = hierarchy_selection$selected,
    excluded = hierarchy_selection$excluded,
    hierarchy_edges = hierarchy_selection$hierarchy_edges,
    selected_code_cols = hierarchy_selection$selected$code_col,
    code_counts = code_counts,
    code_pair_counts = code_pair_counts
  )
}
