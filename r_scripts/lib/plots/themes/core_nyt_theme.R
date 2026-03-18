library(ggplot2)

theme_nyt <- function(base_size = 14, base_family = "serif") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#DDDDDD", size = 0.3),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        family = base_family,
        face = "bold",
        size = base_size * 1.3,
        hjust = 0,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        family = base_family,
        size = base_size * 1.0,
        hjust = 0,
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        family = base_family,
        size = base_size * 0.8,
        hjust = 1,
        color = "#555555",
        margin = margin(t = 10)
      ),
      axis.title.x = element_text(
        family = base_family,
        size = base_size * 1.0,
        margin = margin(t = 8)
      ),
      axis.title.y = element_text(
        family = base_family,
        size = base_size * 1.0,
        margin = margin(r = 8)
      ),
      axis.text = element_text(
        family = base_family,
        size = base_size * 0.9,
        color = "#333333"
      ),
      axis.ticks = element_line(color = "#333333", size = 0.3),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = base_size * 0.9),
      legend.text = element_text(size = base_size * 0.85),
      strip.background = element_rect(fill = "#F5F5F5", color = NA),
      strip.text = element_text(
        family = base_family,
        face = "bold",
        size = base_size * 0.9
      )
    )
}
