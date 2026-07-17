# Domain plot composition root.

plot_domain_source <- function(...) {
  path <- here::here("r_scripts", "lib", "plots", "domains", ...)
  if (!file.exists(path)) {
    stop("Plot domain module not found: ", path, call. = FALSE)
  }
  source(path)
}

plot_domain_source("common", "dual_metric.R")
plot_domain_source("publishing", "day_of_week.R")
plot_domain_source("publishing", "topic_weekday.R")
plot_domain_source("content_strategy", "collaboration.R")
plot_domain_source("content_strategy", "topics.R")
plot_domain_source("content_strategy", "tags.R")
plot_domain_source("audience", "geography.R")

invisible(TRUE)
