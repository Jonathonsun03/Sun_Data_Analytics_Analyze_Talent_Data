# =========================
# ENA Grid Cells + Boxed Bearings + Plotly Tooltips (FULL SCRIPT)
# =========================

library(dplyr)
library(tibble)
library(ggplot2)
library(ggrepel)

# -------------------------
# 1) Helpers: pull code node positions + point positions
# -------------------------
ena_code_nodes_2d <- function(ena_set, dims = c("SVD1","SVD2")) {
  nodes <- ena_set$rotation$nodes
  
  code_col <- intersect(c("code","codes","node","nodes"), names(nodes))[1]
  if (is.na(code_col)) stop("Can't find code column in ena_set$rotation$nodes")
  
  nodes %>%
    transmute(
      code = .data[[code_col]],
      x    = as.numeric(.data[[dims[1]]]),
      y    = as.numeric(.data[[dims[2]]])
    ) %>%
    filter(is.finite(x), is.finite(y))
}

# (Optional utility; not strictly required below, but handy)
ena_points_2d <- function(ena_set, dims = c("SVD1","SVD2")) {
  ena_set$points %>%
    transmute(
      x = as.numeric(.data[[dims[1]]]),
      y = as.numeric(.data[[dims[2]]])
    ) %>%
    filter(is.finite(x), is.finite(y))
}

# -------------------------
# 2) Grid construction from code midpoint cuts
# -------------------------
axis_cuts_from_codes <- function(nodes, v = "x") {
  s <- sort(nodes[[v]])
  (head(s, -1) + tail(s, -1)) / 2
}

rect_grid_from_cuts <- function(xcuts, ycuts, xlim, ylim) {
  xb <- c(xlim[1], xcuts, xlim[2])
  yb <- c(ylim[1], ycuts, ylim[2])
  
  expand.grid(ix = seq_len(length(xb) - 1), iy = seq_len(length(yb) - 1)) %>%
    as_tibble() %>%
    mutate(
      xmin = xb[ix], xmax = xb[ix + 1],
      ymin = yb[iy], ymax = yb[iy + 1],
      xmid = (xmin + xmax) / 2,
      ymid = (ymin + ymax) / 2
    )
}

# -------------------------
# 2b) Nearest code helpers for grid-aligned bearings
# -------------------------
nearest_code_to_value <- function(values, code_positions, code_names) {
  vapply(values, function(v) {
    code_names[which.min(abs(code_positions - v))]
  }, character(1))
}

# -------------------------
# 2c) Grid header helpers
# -------------------------
make_grid_headers <- function(grid, nodes) {
  cols <- grid %>%
    distinct(ix, xmin, xmax, xmid) %>%
    arrange(ix) %>%
    mutate(code_x = nearest_code_to_value(xmid, nodes$x, nodes$code))
  
  rows <- grid %>%
    distinct(iy, ymin, ymax, ymid) %>%
    arrange(iy) %>%
    mutate(code_y = nearest_code_to_value(ymid, nodes$y, nodes$code))
  
  list(cols = cols, rows = rows)
}

# -------------------------
# 3) 2D color map for each cell (smooth gradient across ix, iy)
# -------------------------
cell_colors_2d <- function(grid) {
  nx <- max(grid$ix)
  ny <- max(grid$iy)
  
  xs <- (grid$ix - 0.5) / nx
  ys <- (grid$iy - 0.5) / ny
  
  r <- xs
  b <- ys
  g <- 0.35 + 0.30 * (1 - abs(xs - ys))
  
  rgb(r, g, b)
}

# -------------------------
# 4) Assign each point to a cell (ix, iy)
# -------------------------
assign_points_to_grid <- function(pts, xb, yb) {
  pts %>%
    mutate(
      ix = findInterval(x, xb, rightmost.closed = TRUE),
      iy = findInterval(y, yb, rightmost.closed = TRUE)
    ) %>%
    mutate(
      ix = pmin(pmax(ix, 1), length(xb) - 1),
      iy = pmin(pmax(iy, 1), length(yb) - 1),
      cell_id = paste0("(", ix, ",", iy, ")")
    )
}

# -------------------------
# 5) Main plot: grid + nodes + unit points + tooltips + optional unit labels
# -------------------------
plot_ena_grid_cells <- function(
    ena_set,
    dims = c("SVD1","SVD2"),
    alpha = 0.20,
    show_cell_labels = FALSE,
    unit_label_var = NULL,      # e.g., "Date" or "date"
    label_frequency = 1,
    point_size = 2.8,
    point_stroke = 0.7,
    show_code_nodes = TRUE,
    return_data = FALSE
) {
  
  # --- get code nodes ---
  nodes <- ena_code_nodes_2d(ena_set, dims)
  
  # --- points (keep original columns, add x/y + row id) ---
  pts <- ena_set$points %>%
    mutate(
      x = as.numeric(.data[[dims[1]]]),
      y = as.numeric(.data[[dims[2]]])
    ) %>%
    filter(is.finite(x), is.finite(y)) %>%
    mutate(.row_id = row_number())
  
  # --- build cuts from node midpoints ---
  xcuts <- axis_cuts_from_codes(nodes, "x")
  ycuts <- axis_cuts_from_codes(nodes, "y")
  
  xlim <- range(c(pts$x, nodes$x))
  ylim <- range(c(pts$y, nodes$y))
  
  # --- construct grid ---
  grid <- rect_grid_from_cuts(xcuts, ycuts, xlim, ylim)
  
  xb <- c(xlim[1], xcuts, xlim[2])
  yb <- c(ylim[1], ycuts, ylim[2])
  
  grid <- grid %>%
    mutate(
      cell_fill = cell_colors_2d(cur_data_all()),
      cell_id   = paste0("(", ix, ",", iy, ")")
    )
  
  # --- bin points into grid & carry cell color/id ---
  pts_binned <- assign_points_to_grid(pts, xb, yb) %>%
    left_join(grid %>% select(ix, iy, cell_fill, cell_id), by = c("ix","iy")) %>%
    mutate(.label_me = (.row_id %% label_frequency == 0))
  
  # --- attach "box category" labels (column + row categories) ---
  headers <- make_grid_headers(grid, nodes)
  
  col_map <- headers$cols %>% select(ix, code_x)
  row_map <- headers$rows %>% select(iy, code_y)
  
  pts_binned <- pts_binned %>%
    left_join(col_map, by = "ix") %>%
    left_join(row_map, by = "iy") %>%
    mutate(
      code_x = gsub("^c_", "", code_x),
      code_y = gsub("^c_", "", code_y),
      code_x = gsub("_", " ", code_x),
      code_y = gsub("_", " ", code_y)
    ) %>%
    mutate(cell_id = paste0("(", ix, ",", iy, ")")) 
  
  # --- build Plotly tooltip text ---
  if (!is.null(unit_label_var) && unit_label_var %in% names(pts_binned)) {
    pts_binned <- pts_binned %>%
      mutate(
        tooltip = paste0(
          unit_label_var, ": ", .data[[unit_label_var]],
          "<br>Column (SVD1 bin): ", code_x,
          "<br>Row (SVD2 bin): ", code_y,
          "<br>Cell: ", cell_id
        )
      )
  } else {
    pts_binned <- pts_binned %>%
      mutate(
        tooltip = paste0(
          "Column (SVD1 bin): ", code_x,
          "<br>Row (SVD2 bin): ", code_y,
          "<br>Cell: ", cell_id
        )
      )
  }
  
  # =========================
  # IMPORTANT: initialize p
  # =========================
  p <- ggplot() +
    geom_rect(
      data = grid,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = cell_fill),
      alpha = alpha,
      color = "gray80",
      linewidth = 0.35
    ) +
    scale_fill_identity()
  
  if (isTRUE(show_cell_labels)) {
    p <- p +
      geom_text(
        data = grid,
        aes(x = xmid, y = ymid, label = cell_id),
        size = 2.5,
        alpha = 0.6
      )
  }
  
  # optional: show code nodes + labels
  if (isTRUE(show_code_nodes)) {
    p <- p +
      geom_point(data = nodes, aes(x = x, y = y), size = 3, color = "black") +
      ggrepel::geom_text_repel(
        data = nodes,
        aes(x = x, y = y, label = code),
        size = 3
      )
  }
  
  # unit points on top (with tooltip mapped to aes(text=...))
  p <- p +
    geom_point(
      data = pts_binned,
      aes(x = x, y = y, fill = cell_fill, text = tooltip),
      shape = 21,
      size = point_size,
      stroke = point_stroke,
      color = "black",
      alpha = 0.95
    ) +
    scale_fill_identity()
  
  # optional: unit labels (printed labels)
  if (!is.null(unit_label_var) && unit_label_var %in% names(pts_binned)) {
    p <- p +
      ggrepel::geom_label_repel(
        data = filter(pts_binned, .label_me),
        aes(x = x, y = y, label = .data[[unit_label_var]]),
        size = 3,
        label.size = 0.2,
        alpha = 0.90,
        min.segment.length = 0,
        segment.alpha = 0.35
      )
  }
  
  p <- p +
    labs(
      x = dims[1],
      y = dims[2],
      title = "ENA space binned into code-midpoint cells (units colored by cell)"
    ) +
    theme_minimal()
  
  # attach grid so we can add boxed bearings later
  attr(p, "grid") <- grid
  
  if (isTRUE(return_data)) {
    return(list(plot = p, grid = grid, pts_binned = pts_binned))
  } else {
    return(p)
  }
}

# -------------------------
# 6) Add boxed bearings aligned to the grid (top + left)
# -------------------------
add_boxed_grid_bearings <- function(
    p, ena_set, grid, dims = c("SVD1","SVD2"),
    header_height_frac = 0.10,
    side_width_frac    = 0.14,
    base_text_size = 3,          # your "normal" size
    min_text_size  = 1.8,        # floor for tiny bins
    max_text_size  = 3.2,        # ceiling
    angle_x   = 45,
    clean     = TRUE,
    border_col = "gray70",
    fill_col   = "white",
    wrap_min_width = 6,          # minimum wrap width (characters)
    wrap_scale = 38              # bigger = fewer line breaks, smaller = more wrapping
) {
  
  nodes <- ena_code_nodes_2d(ena_set, dims)
  
  # --- grid extents ---
  xlim <- range(c(grid$xmin, grid$xmax))
  ylim <- range(c(grid$ymin, grid$ymax))
  xr <- diff(xlim)
  yr <- diff(ylim)
  
  # --- band extents ---
  header_ymin <- ylim[2]
  header_ymax <- ylim[2] + header_height_frac * yr
  
  side_xmin <- xlim[1] - side_width_frac * xr
  side_xmax <- xlim[1]
  
  # --- one box per column ---
  cols <- grid %>%
    distinct(ix, xmin, xmax, xmid) %>%
    arrange(ix) %>%
    mutate(code = nearest_code_to_value(xmid, nodes$x, nodes$code))
  
  # --- one box per row ---
  rows <- grid %>%
    distinct(iy, ymin, ymax, ymid) %>%
    arrange(iy) %>%
    mutate(code = nearest_code_to_value(ymid, nodes$y, nodes$code))
  
  # --- clean labels ---
  if (isTRUE(clean)) {
    cols$code <- cols$code %>% gsub("^c_", "", .) %>% gsub("^is_", "", .) %>% gsub("_", " ", .)
    rows$code <- rows$code %>% gsub("^c_", "", .) %>% gsub("^is_", "", .) %>% gsub("_", " ", .)
  }
  
  # =========================
  # Dynamic sizing + wrapping
  # =========================
  
  # width of each column as fraction of full x-range
  cols <- cols %>%
    mutate(
      w_frac = (xmax - xmin) / xr,
      w_frac = ifelse(is.finite(w_frac), w_frac, NA_real_),
      wrap_w = pmax(wrap_min_width, floor(w_frac * wrap_scale)),
      wrap_w = ifelse(is.finite(wrap_w) & !is.na(wrap_w), as.integer(wrap_w), wrap_min_width),
      
      # IMPORTANT: rowwise pairing of (code, wrap_w)
      label = mapply(
        FUN = function(s, w) {
          s <- as.character(s)
          if (is.na(s) || !nzchar(s)) s <- ""          # or "NA"
          paste(strwrap(s, width = w), collapse = "\n")
        },
        s = code,
        w = wrap_w,
        USE.NAMES = FALSE
      )
    )
  
  
  # text size scales with width, but capped
  # (sqrt gives a gentle shrink for small boxes)
  med_w <- median(cols$w_frac, na.rm = TRUE)
  cols <- cols %>%
    mutate(
      text_size = base_text_size * sqrt(pmax(w_frac, 1e-6) / pmax(med_w, 1e-6)),
      text_size = pmin(pmax(text_size, min_text_size), max_text_size)
    )
  
  # (Optional) similar logic for y labels (usually fewer collisions)
  if (!is.finite(yr) || yr == 0) yr <- 1
  
  rows %>%
    mutate(
      h_frac = (ymax - ymin) / yr,
      wrap_w = pmax(wrap_min_width, floor(h_frac * (wrap_scale * 0.9)))
    ) %>%
    filter(is.na(code) | is.na(wrap_w) | !is.finite(wrap_w))
  
  
  rows <- rows %>%
    mutate(
      h_frac = (ymax - ymin) / yr,
      h_frac = ifelse(is.finite(h_frac), h_frac, NA_real_),
      
      wrap_w = pmax(wrap_min_width, floor(h_frac * (wrap_scale * 0.9))),
      wrap_w = ifelse(is.finite(wrap_w) & !is.na(wrap_w), as.integer(wrap_w), wrap_min_width),
      
      # rowwise pairing (code, wrap_w)
      label = mapply(
        FUN = function(s, w) {
          s <- as.character(s)
          if (is.na(s) || !nzchar(s)) s <- ""
          paste(strwrap(s, width = w), collapse = "\n")
        },
        s = code,
        w = wrap_w,
        USE.NAMES = FALSE
      ),
      
      text_size = pmin(
        pmax(
          base_text_size * sqrt(pmax(h_frac, 1e-6) / pmax(median(h_frac, na.rm = TRUE), 1e-6)),
          min_text_size
        ),
        max_text_size
      )
    )
  
  p +
    # =========================
  # TOP HEADER BAND (x bearings)
  # =========================
  geom_rect(
    data = cols,
    aes(xmin = xmin, xmax = xmax, ymin = header_ymin, ymax = header_ymax),
    inherit.aes = FALSE,
    fill = fill_col,
    color = border_col,
    linewidth = 0.35
  ) +
    geom_text(
      data = cols,
      aes(x = xmid, y = (header_ymin + header_ymax) / 2, label = label, size = text_size),
      inherit.aes = FALSE,
      angle = angle_x,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.95
    ) +
    scale_size_identity() +
    geom_segment(
      data = cols,
      aes(x = xmin, xend = xmin, y = header_ymin, yend = ylim[2]),
      inherit.aes = FALSE,
      color = "gray85",
      linewidth = 0.3
    ) +
    
    # =========================
  # LEFT SIDE BAND (y bearings)
  # =========================
  geom_rect(
    data = rows,
    aes(xmin = side_xmin, xmax = side_xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = fill_col,
    color = border_col,
    linewidth = 0.35
  ) +
    geom_text(
      data = rows,
      aes(x = (side_xmin + side_xmax) / 2, y = ymid, label = label, size = text_size),
      inherit.aes = FALSE,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.95
    ) +
    scale_size_identity() +
    geom_segment(
      data = rows,
      aes(x = side_xmax, xend = xlim[1], y = ymin, yend = ymin),
      inherit.aes = FALSE,
      color = "gray85",
      linewidth = 0.3
    ) +
    
    coord_cartesian(
      xlim = c(side_xmin, xlim[2]),
      ylim = c(ylim[1], header_ymax),
      clip = "off"
    ) +
    theme(plot.margin = margin(12, 12, 12, 12))
}


# =========================
# EXAMPLE USAGE
# =========================
# CellGrids <- plot_ena_grid_cells(
#   ena_set = ENA_set,
#   dims = c("SVD1","SVD2"),
#   alpha = 0.25,
#   unit_label_var = "Date",   # <- must exist in ENA_set$points (case-sensitive)
#   label_frequency = 1,
#   point_size = 3.0,
#   show_code_nodes = TRUE
# )
#
# grid <- attr(CellGrids, "grid")
#
# CellGrids <- add_boxed_grid_bearings(
#   p = CellGrids,
#   ena_set = ENA_set,
#   grid = grid
# )
#
# # For Plotly tooltips:
# # library(plotly)
# # CellGrids %>% ggplotly(tooltip = "text")
