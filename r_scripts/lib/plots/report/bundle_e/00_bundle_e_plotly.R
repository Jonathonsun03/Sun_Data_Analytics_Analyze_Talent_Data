bundle_e_ggplotly <- function(
  plot_obj,
  tooltip = NULL,
  rotate_y_axis_title = TRUE,
  height = NULL,
  include_title = TRUE,
  compact = FALSE,
  tickangle = -45
) {
  if (inherits(plot_obj, "ggplot")) {
    plot_obj <- plot_obj +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = abs(tickangle), hjust = 1, vjust = 1)
      )
  }

  p <- if (is.null(tooltip)) {
    plotly::ggplotly(plot_obj)
  } else {
    plotly::ggplotly(plot_obj, tooltip = tooltip)
  }

  if (!inherits(plot_obj, "ggplot")) {
    return(p)
  }

  title_txt <- plot_obj$labels$title
  subtitle_txt <- plot_obj$labels$subtitle

  wrap_for_plotly <- function(x, width = 80) {
    txt <- trimws(as.character(x))
    if (!nzchar(txt)) {
      return("")
    }
    txt <- gsub("\\s+", " ", txt)
    paste(strwrap(txt, width = width), collapse = "\n")
  }

  title_txt <- if (is.null(title_txt) || !nzchar(trimws(as.character(title_txt)))) {
    ""
  } else {
    as.character(title_txt[[1]])
  }
  subtitle_txt <- if (is.null(subtitle_txt) || !nzchar(trimws(as.character(subtitle_txt)))) {
    ""
  } else {
    as.character(subtitle_txt[[1]])
  }

  title_plain <- wrap_for_plotly(title_txt, width = 54)
  subtitle_plain <- wrap_for_plotly(subtitle_txt, width = 88)

  title_txt <- gsub("\n", "<br>", htmltools::htmlEscape(title_plain), fixed = TRUE)
  subtitle_txt <- gsub("\n", "<br>", htmltools::htmlEscape(subtitle_plain), fixed = TRUE)

  title_lines <- max(1L, length(strsplit(title_plain, "\n", fixed = TRUE)[[1]]))
  subtitle_lines <- if (nzchar(subtitle_plain)) {
    length(strsplit(subtitle_plain, "\n", fixed = TRUE)[[1]])
  } else {
    0L
  }

  current_top <- p$x$layout$margin$t
  if (is.null(current_top) || !is.finite(current_top)) {
    current_top <- 60
  }
  needed_top <- 68 + (title_lines * 24) + (subtitle_lines * 20) + 14
  top_margin <- if (isTRUE(include_title)) {
    max(as.numeric(current_top), as.numeric(needed_top))
  } else if (isTRUE(compact)) {
    24
  } else {
    36
  }
  attr_height <- attr(plot_obj, "bundle_e_plotly_height", exact = TRUE)
  if (is.null(height) && !is.null(attr_height) && is.finite(attr_height)) {
    height <- as.numeric(attr_height)
  }

  combined_title <- if (nzchar(subtitle_txt)) {
    paste0(
      "<b>", title_txt, "</b>",
      "<br><span style='font-size:0.78em;'>", subtitle_txt, "</span>"
    )
  } else {
    paste0("<b>", title_txt, "</b>")
  }

  if (isTRUE(rotate_y_axis_title) && length(p$x$layout$annotations) > 0) {
    anns <- p$x$layout$annotations
    for (i in seq_along(anns)) {
      ann <- anns[[i]]
      ann_x <- if (is.null(ann$x)) NA_real_ else suppressWarnings(as.numeric(ann$x))
      ann_y <- if (is.null(ann$y)) NA_real_ else suppressWarnings(as.numeric(ann$y))
      is_axis_annot <- identical(ann$annotationType, "axis")
      is_y_axis_title <- isTRUE(is_axis_annot) &&
        is.finite(ann_x) && is.finite(ann_y) &&
        abs(ann_x - 0) < 1e-9 &&
        abs(ann_y - 0.5) < 1e-9

      if (is_y_axis_title) {
        anns[[i]]$textangle <- -90
        anns[[i]]$xanchor <- "center"
        anns[[i]]$yanchor <- "middle"
        anns[[i]]$xshift <- -58
      }
    }
    p$x$layout$annotations <- anns
  }

  axis_keys <- names(p$x$layout)[grepl("^xaxis[0-9]*$", names(p$x$layout))]
  for (axis_key in axis_keys) {
    if (is.null(p$x$layout[[axis_key]])) {
      p$x$layout[[axis_key]] <- list()
    }
    p$x$layout[[axis_key]]$automargin <- TRUE
    p$x$layout[[axis_key]]$tickangle <- tickangle
  }

  current_bottom <- p$x$layout$margin$b
  if (is.null(current_bottom) || !is.finite(current_bottom)) {
    current_bottom <- 60
  }
  attr_margin_l <- attr(plot_obj, "bundle_e_plotly_margin_l", exact = TRUE)
  attr_margin_b <- attr(plot_obj, "bundle_e_plotly_margin_b", exact = TRUE)
  margin_cfg <- list(
    t = top_margin,
    b = if (isTRUE(compact)) {
      max(
        62,
        if (is.null(attr_margin_b) || !is.finite(attr_margin_b)) 0 else as.numeric(attr_margin_b)
      )
    } else {
      max(
        as.numeric(current_bottom),
        96,
        if (is.null(attr_margin_b) || !is.finite(attr_margin_b)) 0 else as.numeric(attr_margin_b)
      )
    }
  )
  if (isTRUE(rotate_y_axis_title)) {
    current_left <- p$x$layout$margin$l
    if (is.null(current_left) || !is.finite(current_left)) {
      current_left <- 80
    }
    margin_cfg$l <- if (isTRUE(compact)) {
      max(
        86,
        if (is.null(attr_margin_l) || !is.finite(attr_margin_l)) 0 else as.numeric(attr_margin_l)
      )
    } else {
      max(
        as.numeric(current_left),
        132,
        if (is.null(attr_margin_l) || !is.finite(attr_margin_l)) 0 else as.numeric(attr_margin_l)
      )
    }
  }

  layout_args <- list(
    margin = margin_cfg,
    autosize = TRUE
  )
  if (isTRUE(include_title)) {
    layout_args$title <- list(
      text = combined_title,
      x = 0,
      xanchor = "left",
      y = 0.98,
      yanchor = "top",
      pad = list(t = 10, b = 12)
    )
  }
  if (!is.null(height) && is.finite(height)) {
    layout_args$height <- as.numeric(height)
  }

  p <- do.call(plotly::layout, c(list(p = p), layout_args))
  plotly::config(p, responsive = TRUE, displaylogo = FALSE)
}
