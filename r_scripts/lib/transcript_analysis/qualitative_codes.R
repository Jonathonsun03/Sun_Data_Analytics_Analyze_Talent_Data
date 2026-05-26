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

  data_with_codes <- data %>%
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      .codes = purrr::map(.data[[codes_col]], parse_positive_code_ids)
    )

  if (length(group_cols) > 0) {
    data_with_codes <- data_with_codes %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))
  }

  data_with_codes %>%
    dplyr::mutate(
      .group_row_id = dplyr::row_number(),
      .window_codes = purrr::map(
        .data$.group_row_id,
        ~ .data$.codes[seq.int(max(1L, .x - window_size_back), .x)] %>%
          unlist(use.names = FALSE) %>%
          unique()
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(.row_id, pair = purrr::map(.data$.window_codes, codes_to_pairs)) %>%
    tidyr::unnest(cols = "pair") %>%
    dplyr::count(.data$code_1, .data$code_2, name = "window_pair_count", sort = TRUE)
}
