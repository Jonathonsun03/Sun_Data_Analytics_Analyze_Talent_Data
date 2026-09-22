if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Package `dplyr` is required for this test.", call. = FALSE)
}

source(file.path("r_scripts", "lib", "dashboard", "metrics", "viewer_activity.R"))
source(file.path("r_scripts", "lib", "plots", "domains", "audience", "viewer_activity.R"))

assert_equal <- function(actual, expected, message) {
  if (!identical(actual, expected)) stop(message, call. = FALSE)
}

activity <- data.frame(
  talent_code = c("T1", "T1", "T1", "T2"),
  talent_name = c("Talent 1", "Talent 1", "Talent 1", "Talent 2"),
  channel_id = c("C1", "C1", "C1", "C2"),
  video_id = c("V1", "V1", "V2", "V3"),
  video_title = c("Video 1", "Video 1", "Video 2", "Video 3"),
  stream_at = as.POSIXct(c("2026-01-01", "2026-01-01", "2026-01-02", "2026-01-03"), tz = "UTC"),
  user_id = c("U1", "U2", "U1", "U3"),
  latest_username_in_video = c("One", "Two", "One", "Three"),
  message_count = c(10, 2, 5, 7),
  paid_message_count = c(1, 0, 0, 0),
  membership_event_count = c(0, 1, 0, 0)
)

network <- viewer_activity_bipartite_prep(activity, max_videos = 2L, max_users = 2L)
assert_equal(nrow(network$engagement_edges), 2L, "The selected engagement edges are incorrect.")
assert_equal(sum(network$engagement_edges$message_count), 17, "Message weights changed during network preparation.")
assert_equal(nrow(network$videos), 2L, "The requested video bound was not applied.")
assert_equal(nrow(network$streamers), 2L, "Streamer ownership nodes are incorrect.")

video_summary <- viewer_activity_video_summary(activity)
assert_equal(video_summary$messages[[1]], 12, "Video message totals are incorrect.")

message("Viewer activity network tests passed.")
