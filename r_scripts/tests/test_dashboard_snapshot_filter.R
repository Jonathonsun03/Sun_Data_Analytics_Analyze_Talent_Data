suppressPackageStartupMessages({
  library(dplyr)
})

source(file.path("r_scripts", "lib", "dashboard", "data", "sources.R"))
source(file.path("r_scripts", "lib", "plots", "report", "bundle_A", "00_Bundle_A_Helpers.R"))
source(file.path("r_scripts", "lib", "dashboard", "data", "filters.R"))

assert_equal <- function(x, y, message) {
  if (!identical(x, y)) {
    stop(message, call. = FALSE)
  }
}

snapshots <- tibble::tibble(
  `Video ID` = rep(c("video-1", "video-2"), each = 3),
  `Published At` = as.POSIXct(rep("2026-01-01", 6), tz = "UTC"),
  date = rep(as.Date(c("2026-08-15", "2026-08-16", "2026-08-17")), 2),
  views = c(10, 15, 21, 20, 24, 30)
)

windowed <- dashboard_apply_snapshot_window(
  snapshots,
  start_date = "2026-08-16",
  end_date = "2026-08-17"
)
assert_equal(
  sort(unique(windowed$date)),
  as.Date(c("2026-08-16", "2026-08-17")),
  "The dashboard window must use analytics snapshot dates."
)
assert_equal(
  nrow(windowed),
  4L,
  "The snapshot window should retain every video's daily rows."
)

latest <- dashboard_latest_snapshot_rows(windowed)
assert_equal(
  unique(latest$date),
  as.Date("2026-08-17"),
  "Cross-sectional metrics must use the latest selected snapshot."
)
assert_equal(
  sort(latest$`Video ID`),
  c("video-1", "video-2"),
  "The latest selected snapshot must retain every tracked video."
)

cat("dashboard snapshot filter tests passed\n")
