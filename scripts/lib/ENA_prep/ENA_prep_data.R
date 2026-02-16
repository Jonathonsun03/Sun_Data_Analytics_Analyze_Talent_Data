library(tidyselect)
library(dplyr)
library(rENA)

ENA_setup <- function(df, units_col, conversation_cols, codes_cols, metadata_cols = NULL){
  
  # Convert df to data frame if needed
  df <- as.data.frame(df)
  
  # Handle units
  unit <- df[units_col]
  
  # Handle conversation
  conversation <- df[conversation_cols]
  
  # Handle codes with tidyselect support
  # Use eval_select to support starts_with, ends_with, contains, etc.
  codes_idx <- eval_select(enquo(codes_cols), df)
  codes <- df[codes_idx]
  
  # Handle metadata
  if (!is.null(metadata_cols)) {
    metadata <- df[metadata_cols]
  } else {
    metadata <- NULL
  }
  
  return(list(units = unit,
              conversation = conversation,
              codes = codes,
              metadata = metadata))
}


ENA_accumulate_from_prep <- function(ENAColumns, window.size.back = 4, ...) {
  # Basic validation (keeps errors readable)
  stopifnot(is.list(ENAColumns))
  for (nm in c("units", "conversation", "codes")) {
    if (is.null(ENAColumns[[nm]])) stop("ENAColumns must include `", nm, "`.")
  }
  
  ena.accumulate.data(
    units = ENAColumns$units,
    conversation = ENAColumns$conversation,
    metadata = ENAColumns$metadata,  # NULL is fine
    codes = ENAColumns$codes,
    window.size.back = window.size.back,
    ...
  )
}

ENA_lineweight <- function(ENA_set, filter_col = NULL, filter_val = NULL) {
  
  # Start with the line.weights data
  data <- ENA_set$line.weights
  
  # Apply filter only if both filter_col and filter_val are provided
  if (!is.null(filter_col) && !is.null(filter_val)) {
    data <- data %>%
      filter(!!sym(filter_col) == filter_val)
  }
  
  # Calculate mean line weights
  lineweight <- data %>%
    as.matrix() %>%
    colMeans() %>%
    as.vector()
  
  return(lineweight)
}

ENA_lineweight_tbl <- function(ENA_set, unit_val = NULL, unit_col = "actor") {
  
  lw_mat <- as.matrix(ENA_set$line.weights)
  
  # Pull metadata that aligns with rows of line.weights
  meta <- ENA_set$meta.data
  if (is.null(meta)) stop("ENA_set$meta.data not found; need the row metadata that matches line.weights.")
  
  if (!is.null(unit_val)) {
    keep <- meta[[unit_col]] == unit_val
    lw_mat <- lw_mat[keep, , drop = FALSE]
  }
  
  lw <- colMeans(lw_mat, na.rm = TRUE)
  
  tibble::tibble(
    edge = names(lw),
    weight = as.numeric(lw)
  ) %>%
    dplyr::arrange(dplyr::desc(weight))
}

# Build an ENA lineweight table with separated code columns and optional z-score.
prepare_ena_lineweight_tbl <- function(ENA_set,
                                       unit_col = "actor",
                                       unit_val = NULL,
                                       add_z = TRUE) {
  tbl <- ENA_lineweight_tbl(
    ENA_set = ENA_set,
    unit_val = unit_val,
    unit_col = unit_col
  ) %>%
    tidyr::separate(
      col = edge,
      into = c("Code_1", "Code_2"),
      sep = " & "
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("Code"),
        as.factor
      )
    ) %>%
    dplyr::arrange(dplyr::desc(weight))

  if (isTRUE(add_z)) {
    if (!exists("add_weight_z_score", mode = "function")) {
      stop("add_weight_z_score() is not loaded. Source scripts/lib/clean_data/analytics_core.R first.")
    }
    tbl <- add_weight_z_score(tbl, weight_col = "weight", z_col = "z_score")
  }

  tbl
}

ENA_points <- function(ENA_set, filter_col = NULL, filter_val = NULL) {
  
  # Start with the line.weights data
  data <- ENA_set$points
  
  # Apply filter only if both filter_col and filter_val are provided
  if (!is.null(filter_col) && !is.null(filter_val)) {
    data <- data %>%
      filter(!!sym(filter_col) == filter_val)
  }
  
  # Calculate mean line weights
  lineweight <- data %>%
    as.matrix() %>%
    colMeans() %>%
    as.vector()
  
  return(lineweight)
}

add_ena_point_groups <- function(
    base_plot,
    point_list,
    colors = NULL,
    shape = "circle",
    show_legend = TRUE,
    label.offset = "top left",
    label.font.size = NULL,
    label.font.color = NULL,
    label.font.family = NULL
) {
  stopifnot(is.list(point_list))
  
  group_names <- names(point_list)
  
  group_names <- ifelse(is.na(group_names) | group_names == "", "UNKNOWN", group_names)
  group_names <- make.unique(group_names)
  names(point_list) <- group_names
  
  if (is.null(colors) || is.null(names(colors)) || any(!group_names %in% names(colors))) {
    colors <- setNames(grDevices::rainbow(length(group_names)), group_names)
  }
  
  plot <- base_plot
  
  for (group_name in group_names) {
    pts <- point_list[[group_name]]
    
    # ---- Normalize pts into a 1-row, 2-col object (x,y) ----
    if (is.atomic(pts) && is.null(dim(pts))) {
      # plain numeric vector: interpret as ONE point (x,y)
      if (length(pts) < 2) {
        stop(sprintf("Point group '%s' has < 2 values; need at least x,y.", group_name))
      }
      pts <- matrix(pts[1:2], ncol = 2)   # <-- key fix (no reshaping into many rows)
    } else {
      # matrix/data.frame: take first two columns as x,y
      pts <- as.data.frame(pts)
      if (ncol(pts) < 2) {
        stop(sprintf("Point group '%s' has < 2 columns; need x,y.", group_name))
      }
      pts <- as.matrix(pts[, 1:2, drop = FALSE])
    }
    
    n <- nrow(pts)
    group_labels <- rep(group_name, n)
    
    plot <- ena.plot.points(
      plot,
      points = pts,
      labels = group_labels,
      label.offset = label.offset,
      label.font.size = label.font.size,
      label.font.color = label.font.color,
      label.font.family = label.font.family,
      colors = colors[[group_name]],
      shape = shape,
      legend.name = if (show_legend) group_name else NULL
    )
  }
  
  plot
}



