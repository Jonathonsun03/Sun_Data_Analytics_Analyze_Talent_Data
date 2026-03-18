#' Clean code names for display
#' @param code Character string of code name
#' @return Cleaned code name
clean_code <- function(code) {
  code %>%
    str_remove_all("_code") %>%
    str_replace_all("_", " ") %>%
    str_replace(" & ", " &\n")
}

#' Extract and prepare code data from ENA set
#' @param ena_set An ENA set object
#' @param svd_dim Character string specifying which SVD dimension (e.g., "SVD1", "SVD2")
#' @param descending Logical, whether to sort in descending order
#' @return Data frame with code loadings and cleaned names
get_code_data <- function(ena_set, svd_dim = "SVD1", descending = TRUE) {
  
  nodes <- ena_set$rotation$nodes
  if (is.null(nodes)) {
    stop("ena_set$rotation$nodes not found.")
  }
  
  # Validate SVD dimension exists
  if (!svd_dim %in% colnames(nodes)) {
    stop(paste("SVD dimension", svd_dim, "not found. Available dimensions:",
               paste(grep("^SVD", colnames(nodes), value = TRUE), collapse = ", ")))
  }
  
  code_col <- if ("code" %in% colnames(nodes)) {
    "code"
  } else if ("codes" %in% colnames(nodes)) {
    "codes"
  } else {
    stop("No code column found in ena_set$rotation$nodes.")
  }
  
  code_data <- nodes %>%
    mutate(across(starts_with("SVD"), as.numeric)) %>%
    rename(svd_value = !!sym(svd_dim))
  
  if (descending) {
    code_data <- code_data %>% arrange(desc(svd_value))
  } else {
    code_data <- code_data %>% arrange(svd_value)
  }
  
  code_data %>%
    mutate(
      codes_clean = sapply(.data[[code_col]], clean_code),
      rank = row_number()
    )
}

#' Create code bands for visualization
#' @param code_data Data frame from get_code_data()
#' @param padding Numeric value for padding at top of bands
#' @return Data frame with ymin and ymax for bandshttp://127.0.0.1:25353/graphics/plot_zoom_png?width=1200&height=900
create_code_bands <- function(code_data, padding = 0.05) {
  code_data %>%
    arrange(svd_value) %>%
    mutate(
      ymin = svd_value,
      ymax = lead(svd_value, default = max(svd_value) + padding)
    )
}

#' Prepare plot data from ENA points
#' @param ena_set An ENA set object
#' @param svd_dim Character string specifying which SVD dimension
#' @param date_col Character string specifying the date column name
#' @param date_format Character string for date parsing (e.g., "mdy", "ymd", "dmy")
#' @param group_col Character string specifying grouping column (default: date_col)
#' @return Summarized data frame ready for plotting
get_plot_data <- function(ena_set, 
                          svd_dim = "SVD1", 
                          date_col = "date",
                          date_format = "mdy",
                          group_col = NULL) {
  
  if (is.null(group_col)) {
    group_col <- date_col
  }
  
  # Get the parsing function
  parse_date <- switch(date_format,
                       "mdy" = lubridate::mdy,
                       "ymd" = lubridate::ymd,
                       "dmy" = lubridate::dmy,
                       "mdy_hms" = lubridate::mdy_hms,
                       "ymd_hms" = lubridate::ymd_hms,
                       lubridate::mdy)  # default
  
  ena_set$points %>%
    mutate(across(starts_with("SVD"), as.numeric)) %>%
    rename(svd_value = !!sym(svd_dim),
           date_var = !!sym(date_col)) %>%
    mutate(date_var = parse_date(date_var)) %>%
    group_by(date_var) %>%
    summarize(
      Mean_SVD = mean(svd_value, na.rm = TRUE),
      SD_SVD = sd(svd_value, na.rm = TRUE),
      N = n(),
      .groups = "drop"
    )
}

#' Create ENA time series plot with code loading bands
#' @param plot_data Data frame from get_plot_data()
#' @param code_data Data frame from get_code_data()
#' @param code_bands Data frame from create_code_bands()
#' @param svd_dim Character string for axis label
#' @param show_bands Logical, whether to show colored bands
#' @param show_lines Logical, whether to show horizontal lines at code loadings
#' @param show_labels Logical, whether to show code labels
#' @param show_smooth Logical, whether to show smoothed trend line
#' @param band_alpha Numeric, transparency of bands (0-1)
#' @param title Character string for plot title
#' @return ggplot object
plot_ena_timeseries <- function(plot_data,
                                code_data,
                                code_bands = NULL,
                                svd_dim = "SVD1",
                                show_bands = TRUE,
                                show_lines = TRUE,
                                show_labels = TRUE,
                                show_smooth = TRUE,
                                band_alpha = 0.3,
                                title = NULL) {
  
  # Create bands if not provided
  if (is.null(code_bands) && show_bands) {
    code_bands <- create_code_bands(code_data)
  }
  
  # Get date range for bands
  date_range <- range(plot_data$date_var, na.rm = TRUE)
  date_padding <- as.numeric(diff(date_range)) * 0.02
  
  # Default title
  if (is.null(title)) {
    title <- paste("ENA", svd_dim, "Over Time with Code Loading Bands")
  }
  
  # Start plot
  p <- ggplot(plot_data, aes(x = date_var, y = Mean_SVD))
  
  # Add colored bands
  if (show_bands && !is.null(code_bands)) {
    p <- p +
      geom_rect(data = code_bands,
                aes(xmin = date_range[1] - date_padding,
                    xmax = date_range[2] + date_padding,
                    ymin = ymin,
                    ymax = ymax,
                    fill = svd_value),
                inherit.aes = FALSE,
                alpha = band_alpha) +
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0)
  }
  
  # Add horizontal lines at code loadings
  if (show_lines) {
    p <- p +
      geom_hline(data = code_data,
                 aes(yintercept = svd_value),
                 linetype = "dotted",
                 alpha = 0.5,
                 color = "gray30")
  }
  
  # Add code labels on the right side
  if (show_labels) {
    p <- p +
      geom_text(data = code_data,
                aes(x = date_range[2] + date_padding * 0.5,
                    y = svd_value,
                    label = codes_clean),
                hjust = 0,
                size = 2.5,
                inherit.aes = FALSE)
  }
  
  # Zero line
  p <- p +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8)
  
  # Smooth line
  if (show_smooth) {
    p <- p +
      geom_smooth(se = TRUE, color = "blue", fill = "lightblue", alpha = 0.5)
  }
  
  # Points
  p <- p +
    geom_point(aes(size = N), alpha = 0.8, color = "black") +
    scale_size_continuous(name = "N", range = c(2, 6))
  
  # Expand x-axis to fit labels
  if (show_labels) {
    p <- p + scale_x_date(expand = expansion(mult = c(0.02, 0.15)))
  }
  
  # Labels and theme
  p <- p +
    labs(
      x = "Date",
      y = svd_dim,
      title = title
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    guides(fill = "none")
  
  return(p)
}

#' Main wrapper function to create ENA time series visualization
#' @param ena_set An ENA set object
#' @param svd_dim Character string specifying which SVD dimension (e.g., "SVD1", "SVD2")
#' @param date_col Character string specifying the date column name
#' @param date_format Character string for date parsing
#' @param ... Additional arguments passed to plot_ena_timeseries()
#' @return ggplot object
ena_timeseries_plot <- function(ena_set,
                                svd_dim = "SVD1",
                                date_col = "date",
                                date_format = "mdy",
                                ...) {
  
  # Get code data
  code_data <- get_code_data(ena_set, svd_dim = svd_dim)
  
  # Create code bands
  code_bands <- create_code_bands(code_data)
  
  # Get plot data
  plot_data <- get_plot_data(ena_set, 
                             svd_dim = svd_dim, 
                             date_col = date_col,
                             date_format = date_format)
  
  # Create plot
  plot_ena_timeseries(plot_data = plot_data,
                      code_data = code_data,
                      code_bands = code_bands,
                      svd_dim = svd_dim,
                      ...)
}

ena_top2_theme_timeseries <- function(
    ena_set,
    svd_dim = "SVD1",
    group_col = "date",
    group_format = c("none", "date"),
    date_format = c("ymd", "mdy", "dmy", "ymd_hms", "mdy_hms"),
    title = NULL,
    label_prefix_remove = "^is_",
    ambiguous_margin = 0.03,
    show_ambiguous = FALSE,
    point_size_by = c("nearest_distance", "N"),
    repel = TRUE,
    return_data = FALSE
) {
  # --- deps ---
  stopifnot(!is.null(ena_set$points), !is.null(ena_set$rotation$nodes))
  group_format <- match.arg(group_format)
  date_format <- match.arg(date_format)
  point_size_by <- match.arg(point_size_by)
  
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  
  # --- validate svd_dim ---
  if (!svd_dim %in% names(ena_set$points)) {
    stop("`svd_dim` not found in ena_set$points. Got: ", svd_dim)
  }
  if (!svd_dim %in% names(ena_set$rotation$nodes)) {
    stop("`svd_dim` not found in ena_set$rotation$nodes. Got: ", svd_dim)
  }
  
  # --- anchors: code loadings for the chosen dimension ---
  # rENA nodes typically have `code`; fall back to `codes` if needed
  code_col <- dplyr::case_when(
    "code" %in% names(ena_set$rotation$nodes) ~ "code",
    "codes" %in% names(ena_set$rotation$nodes) ~ "codes",
    TRUE ~ NA_character_
  )
  if (is.na(code_col)) stop("No `code`/`codes` column found in ena_set$rotation$nodes.")
  
  anchors <- ena_set$rotation$nodes |>
    dplyr::select(theme = dplyr::all_of(code_col),
                  loading = dplyr::all_of(svd_dim)) |>
    dplyr::mutate(loading = as.numeric(loading))
  
  # --- grouping parse (optional) ---
  parse_date <- switch(
    date_format,
    "mdy"     = lubridate::mdy,
    "ymd"     = lubridate::ymd,
    "dmy"     = lubridate::dmy,
    "mdy_hms" = lubridate::mdy_hms,
    "ymd_hms" = lubridate::ymd_hms
  )
  
  points_df <- ena_set$points |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("SVD"), as.numeric))
  
  if (!group_col %in% names(points_df)) {
    stop("`group_col` not found in ena_set$points: ", group_col)
  }
  
  if (group_format == "date") {
    points_df <- points_df |>
      dplyr::mutate(.group = parse_date(.data[[group_col]]))
  } else {
    points_df <- points_df |>
      dplyr::mutate(.group = .data[[group_col]])
  }
  
  # --- daily/ grouped mean for chosen SVD dimension ---
  grouped <- points_df |>
    dplyr::group_by(.group) |>
    dplyr::summarize(
      averageSVD = mean(.data[[svd_dim]], na.rm = TRUE),
      N = dplyr::n(),
      .groups = "drop"
    )
  
  # --- distance to anchors + pick top 2 per group ---
  dist_tbl <- grouped |>
    tidyr::crossing(anchors) |>
    dplyr::mutate(distance = abs(averageSVD - loading)) |>
    dplyr::group_by(.group) |>
    dplyr::arrange(distance, .by_group = TRUE) |>
    dplyr::summarize(
      averageSVD       = dplyr::first(averageSVD),
      N                = dplyr::first(N),
      nearest_theme    = dplyr::first(theme),
      nearest_distance = dplyr::first(distance),
      second_theme     = dplyr::nth(theme, 2),
      second_distance  = dplyr::nth(distance, 2),
      margin           = second_distance - nearest_distance,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      nearest_theme_lbl = stringr::str_replace_all(nearest_theme, label_prefix_remove, ""),
      second_theme_lbl  = stringr::str_replace_all(second_theme,  label_prefix_remove, ""),
      top2_lbl          = paste0("Closest: ", nearest_theme_lbl, "\n2nd: ", second_theme_lbl),
      ambiguous         = margin < ambiguous_margin
    )
  
  # --- title ---
  if (is.null(title)) {
    title <- paste0("Average ENA ", svd_dim, " Over Time with Top-2 Nearest Themes")
  }
  
  # --- choose size mapping ---
  size_var <- if (point_size_by == "nearest_distance") "nearest_distance" else "N"
  size_name <- if (point_size_by == "nearest_distance") "Distance to nearest" else "N"
  
  # --- plot ---
  p <- ggplot2::ggplot(dist_tbl, ggplot2::aes(x = .group, y = averageSVD)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(size = .data[[size_var]])) +
    ggplot2::scale_size_continuous(name = size_name) +
    ggplot2::labs(
      title = title,
      x = group_col,
      y = paste0("Average ", svd_dim)
    ) +
    ggplot2::theme_minimal()
  
  if (show_ambiguous) {
    p <- p + ggplot2::geom_point(
      data = dplyr::filter(dist_tbl, ambiguous),
      shape = 1,
      size = 4,
      inherit.aes = TRUE
    )
  }
  
  if (repel) {
    if (!requireNamespace("ggrepel", quietly = TRUE)) {
      stop("Package `ggrepel` is required for repel labels. Install it or set `repel = FALSE`.")
    }
    p <- p + ggrepel::geom_text_repel(
      ggplot2::aes(label = top2_lbl),
      size = 3,
      lineheight = 0.95,
      min.segment.length = 0,
      box.padding = 0.35,
      point.padding = 0.2,
      max.overlaps = Inf
    )
  } else {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = top2_lbl),
      size = 3,
      vjust = -0.8
    )
  }
  
  if (return_data) return(list(plot = p, data = dist_tbl))
  p
}
