sun_data_brand_colors <- function() {
  c(
    midnight = "#08163A",
    blue = "#4580E6",
    orange = "#F35C06",
    cloud = "#E9EDF2",
    ink = "#151F36",
    steel = "#9AA6B8"
  )
}

sun_data_palette_base <- function(variant = c("brand", "blue", "orange", "neutral")) {
  variant <- match.arg(variant)

  switch(
    variant,
    brand = c("#4580E6", "#F35C06", "#2A5DAE", "#FF8A45", "#6EA4FF", "#B94C1A", "#7F8EA3", "#C5CEDA"),
    blue = c("#DCEBFF", "#AFCBFF", "#79A9FF", "#4580E6", "#2A5DAE", "#1B3E78", "#08163A"),
    orange = c("#FFF1E8", "#FFD4BC", "#FFAC7C", "#F35C06", "#C14A07", "#8F3607", "#5F2306"),
    neutral = c("#151F36", "#3A4A66", "#5A6B86", "#7F8EA3", "#A8B3C1", "#C5CEDA", "#E9EDF2")
  )
}

sun_data_palette <- function(n, variant = c("brand", "blue", "orange", "neutral")) {
  variant <- match.arg(variant)
  if (length(n) != 1 || is.na(n) || n < 1) {
    stop("`n` must be a positive integer.")
  }
  n <- as.integer(n)
  base <- sun_data_palette_base(variant)

  if (n == 1) {
    return(base[1])
  }
  if (n <= length(base)) {
    return(base[seq_len(n)])
  }
  rep(base, length.out = n)
}

scale_color_sun_data <- function(..., variant = c("brand", "blue", "orange", "neutral"), na.value = "#9AA6B8") {
  variant <- match.arg(variant)
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) sun_data_palette(n, variant = variant),
    na.value = na.value,
    ...
  )
}

scale_fill_sun_data <- function(..., variant = c("brand", "blue", "orange", "neutral"), na.value = "#9AA6B8") {
  variant <- match.arg(variant)
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) sun_data_palette(n, variant = variant),
    na.value = na.value,
    ...
  )
}

sun_data_gradient <- function(variant = c("blue", "orange", "diverging")) {
  variant <- match.arg(variant)
  switch(
    variant,
    blue = c("#EAF2FF", "#AFCBFF", "#4580E6", "#1B3E78", "#08163A"),
    orange = c("#FFF3EA", "#FFCFAF", "#F35C06", "#B7480A", "#6E2C07"),
    diverging = c("#F35C06", "#E9EDF2", "#4580E6")
  )
}

scale_color_sun_data_c <- function(..., variant = c("blue", "orange", "diverging"), na.value = "#9AA6B8") {
  variant <- match.arg(variant)
  ggplot2::scale_color_gradientn(
    colours = sun_data_gradient(variant = variant),
    na.value = na.value,
    ...
  )
}

scale_fill_sun_data_c <- function(..., variant = c("blue", "orange", "diverging"), na.value = "#9AA6B8") {
  variant <- match.arg(variant)
  ggplot2::scale_fill_gradientn(
    colours = sun_data_gradient(variant = variant),
    na.value = na.value,
    ...
  )
}
