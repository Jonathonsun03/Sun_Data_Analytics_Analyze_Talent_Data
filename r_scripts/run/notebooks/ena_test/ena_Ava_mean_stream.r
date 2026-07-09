library(here)
library(readr)
library(dplyr)
library(purrr)
library(rENA)

# Setup -------------------------------------------------------------------

source(here::here("r_scripts", "lib", "utils", "source_dir.r"))
source(here::here("r_scripts", "lib", "utils", "datalake_root.r"))
source(here::here("r_scripts", "lib", "clean_data", "qualitative_data_prep", "qualitative_data_prep.r"))
source_dir("r_scripts", "lib", "ENA_prep", "analysis_helpers")

# Clean up functions ----------------------

sanitize_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- gsub("_+", "_", x)
  x <- ifelse(grepl("^[0-9]", x), paste0("x_", x), x)
  x
}

build_code_column_lookup <- function(codebook) {
  primary_codes <- codebook %>%
    filter(!is.na(.data[["Primary Code ID"]])) %>%
    distinct(
      code_id = .data[["Primary Code ID"]],
      code_label = .data[["Primary Code"]]
    )

  secondary_codes <- codebook %>%
    filter(!is.na(.data[["Secondary Code ID"]])) %>%
    distinct(
      code_id = .data[["Secondary Code ID"]],
      code_label = .data[["Secondary Code"]]
    )

  bind_rows(primary_codes, secondary_codes) %>%
    filter(!is.na(.data$code_id), !is.na(.data$code_label)) %>%
    mutate(
      old_name = paste0("code_", .data$code_id),
      new_name = sanitize_name(paste("code", .data$code_id, .data$code_label))
    ) %>%
    distinct(.data$old_name, .keep_all = TRUE)
}

build_code_hierarchy_edges <- function(codebook) {
  codebook %>%
    filter(
      !is.na(.data[["Primary Code ID"]]),
      !is.na(.data[["Secondary Code ID"]]),
      trimws(.data[["Primary Code ID"]]) != "",
      trimws(.data[["Secondary Code ID"]]) != ""
    ) %>%
    transmute(
      primary_code = trimws(.data[["Primary Code ID"]]),
      secondary_code = trimws(.data[["Secondary Code ID"]])
    ) %>%
    distinct()
}

select_top_hierarchy_codes <- function(code_counts,
                                       codebook,
                                       available_code_cols,
                                       n = 8L,
                                       prefer = c("secondary", "primary")) {
  prefer <- match.arg(prefer)
  hierarchy_edges <- build_code_hierarchy_edges(codebook)

  ranked_codes <- code_counts %>%
    mutate(code_col = paste0("code_", .data$code)) %>%
    filter(.data$code_col %in% available_code_cols) %>%
    mutate(.rank = dplyr::row_number())

  if (nrow(hierarchy_edges) == 0L || nrow(ranked_codes) == 0L) {
    return(list(
      selected = ranked_codes %>% slice_head(n = n),
      excluded = ranked_codes %>% slice_head(n = 0),
      hierarchy_edges = hierarchy_edges
    ))
  }

  candidate_codes <- ranked_codes$code

  excluded_codes <- hierarchy_edges %>%
    filter(
      .data$primary_code %in% candidate_codes,
      .data$secondary_code %in% candidate_codes
    ) %>%
    transmute(
      code = if (identical(prefer, "secondary")) .data$primary_code else .data$secondary_code,
      excluded_for = if (identical(prefer, "secondary")) .data$secondary_code else .data$primary_code,
      hierarchy_preference = prefer
    ) %>%
    distinct()

  excluded_ranked_codes <- ranked_codes %>%
    inner_join(excluded_codes, by = "code") %>%
    arrange(.data$.rank)

  selected_codes <- ranked_codes %>%
    anti_join(excluded_codes, by = "code") %>%
    slice_head(n = n)

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
    rename_with(
      .fn = function(x) unname(rename_map[x]),
      .cols = all_of(names(rename_map))
    )
}

parse_positive_code_ids <- function(x) {
  if (is.na(x) || trimws(x) == "") {
    return(character())
  }

  strsplit(x, ";", fixed = TRUE)[[1]] %>%
    trimws() %>%
    discard(~ .x == "") %>%
    unique()
}

extract_talent_name_from_request_custom_id <- function(request_custom_id) {
  request_custom_id <- ifelse(is.na(request_custom_id), "", request_custom_id)

  vapply(request_custom_id, function(id) {
    id_parts <- strsplit(id, "__", fixed = TRUE)[[1]]
    run_id <- if (length(id_parts) >= 2L) {
      id_parts[[length(id_parts) - 1L]]
    } else {
      id
    }

    sub("_.*$", "", run_id)
  }, character(1), USE.NAMES = FALSE)
}

codes_to_pairs <- function(codes) {
  codes <- sort(unique(codes))

  if (length(codes) < 2) {
    return(tibble::tibble(code_1 = character(), code_2 = character()))
  }

  pair_matrix <- utils::combn(codes, 2)

  tibble::tibble(
    code_1 = pair_matrix[1, ],
    code_2 = pair_matrix[2, ]
  )
}

count_code_pairs_by_window <- function(data,
                                       codes_col = "positive_codes",
                                       group_cols = c("source_file", "video_id"),
                                       window_size_back = 4L) {
  group_cols <- intersect(group_cols, names(data))

  data_with_codes <- data %>%
    mutate(
      .row_id = dplyr::row_number(),
      .codes = map(.data[[codes_col]], parse_positive_code_ids)
    )

  if (length(group_cols) > 0) {
    data_with_codes <- data_with_codes %>%
      group_by(across(all_of(group_cols)))
  }

  data_with_codes %>%
    mutate(
      .group_row_id = dplyr::row_number(),
      .window_codes = map(
        .data$.group_row_id,
        ~ .data$.codes[seq.int(max(1L, .x - window_size_back), .x)] %>%
          unlist(use.names = FALSE) %>%
          unique()
      )
    ) %>%
    ungroup() %>%
    transmute(.row_id, pair = map(.data$.window_codes, codes_to_pairs)) %>%
    tidyr::unnest(cols = "pair") %>%
    count(.data$code_1, .data$code_2, name = "window_pair_count", sort = TRUE)
}

ena_group_centroids <- function(ena_set,
                                group_col = "talent_name",
                                dims = c("SVD1", "SVD2")) {
  points <- tibble::as_tibble(ena_set$points)

  missing_cols <- setdiff(c(group_col, dims), names(points))
  if (length(missing_cols) > 0L) {
    stop(
      "Missing column(s) in ENA_set$points: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  points %>%
    filter(!is.na(.data[[group_col]])) %>%
    group_by(group = .data[[group_col]]) %>%
    summarise(
      across(all_of(dims), ~ mean(.x, na.rm = TRUE)),
      n_points = dplyr::n(),
      .groups = "drop"
    )
}

ena_centroids_to_point_list <- function(centroids,
                                        group_col = "group",
                                        dims = c("SVD1", "SVD2")) {
  missing_cols <- setdiff(c(group_col, dims), names(centroids))
  if (length(missing_cols) > 0L) {
    stop(
      "Missing column(s) in centroid table: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  centroids %>%
    select(all_of(c(group_col, dims))) %>%
    split(.[[group_col]]) %>%
    map(~ as.matrix(.x[, dims, drop = FALSE]))
}

# Script

coding_book <- resolve_datalake_path(
  r"(Z:\DataLake\Sun_Data_Analytics\Processed\Talent_Data\Qualitative Codebooks\library\selections\chat_monetary_growth\batch_002\batch_002_codebook.csv)"
)

batch_run_dirs <- c(
  r"(Z:\DataLake\Sun_Data_Analytics\Processed\Talent_Data\qualitative_batch_runs\chat_monetary_growth_batch_002_20260523_233919)"
)

batch_review_paths <- batch_run_dirs %>%
  purrr::map_chr(resolve_datalake_path) %>%
  file.path("coding_review.csv")

import_data <- batch_review_paths %>%
  purrr::set_names(basename(dirname(.))) %>%
  purrr::map_dfr(
    ~ readr::read_csv(.x, show_col_types = FALSE),
    .id = "batch_run"
  ) %>%
  mutate(talent_name = extract_talent_name_from_request_custom_id(.data$request_custom_id)) %>%
  select(
    batch_run,
    talent_name,
    request_custom_id,
    source_file,
    video_id,
    row_number,
    timecode,
    source,
    speaker,
    message_type,
    text,
    positive_codes,
    starts_with("code_")
  )

glimpse(import_data)
codebook_data <- readr::read_csv(coding_book, show_col_types = FALSE)

coding_book
batch_review_paths

code_column_lookup <- build_code_column_lookup(codebook_data)
import_data_named <- rename_code_columns(import_data, code_column_lookup)

code_column_lookup
names(import_data_named)

glimpse(import_data_named)

code_window_size_back <- 4L

code_pair_counts <- count_code_pairs_by_window(
  import_data,
  window_size_back = code_window_size_back
)

top_code_pairs <- code_pair_counts %>%
  slice_head(n = 6)

code_counts <- code_pair_counts %>%
  select(code = code_1, window_pair_count) %>%
  bind_rows(
    code_pair_counts %>%
      select(code = code_2, window_pair_count)
  ) %>%
  group_by(.data$code) %>%
  summarise(
    pair_participation_count = sum(.data$window_pair_count),
    distinct_pair_count = dplyr::n(),
    .groups = "drop"
  ) %>%
  arrange(desc(.data$pair_participation_count), desc(.data$distinct_pair_count))

top_code_hierarchy_preference <- "secondary"

top_code_selection <- select_top_hierarchy_codes(
  code_counts = code_counts,
  codebook = codebook_data,
  available_code_cols = names(import_data),
  n = 8,
  prefer = top_code_hierarchy_preference
)

top_codes <- top_code_selection$selected
hierarchy_excluded_codes <- top_code_selection$excluded

top_code_cols <- top_codes$code_col

ENA <- ENA_setup(
  df = import_data,
  units_col = c("talent_name", "row_number"),
  conversation_cols = "text",
  codes_cols = all_of(top_code_cols)
)

ENA_accum <- ENA_accumulate_from_prep(ENA)
ENA_set <- rENA::ena.make.set(ENA_accum)

ENA_lineweights <- as.matrix(ENA_set$line.weights)

mean_network <- colMeans(ENA_lineweights, na.rm = TRUE)

ena_plot <- rENA::ena.plot(ENA_set, title = "ENA Plot")
talent_centroids <- ena_group_centroids(
  ENA_set,
  group_col = "talent_name"
)

talent_centroid_points <- ena_centroids_to_point_list(talent_centroids)

ena_plot_with_centroids <- plot(ENA_set, title = "ENA Plot") |>
  add_network(colors = "blue", edge.multiplier = 50) |>
  add_ena_point_groups(
    point_list = talent_centroid_points,
    colors = c(Ava = "red", Nova = "purple"),
    shape = "square",
    label.offset = "top right"
  )

ena_plot_with_centroids
