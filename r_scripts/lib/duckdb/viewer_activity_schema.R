init_viewer_activity_schema <- function(con) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package `DBI` is required.", call. = FALSE)
  }

  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS analysis")
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS ops")
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS ops.pipeline_runs (
       pipeline_run_id VARCHAR PRIMARY KEY,
       pipeline_name VARCHAR NOT NULL,
       started_at TIMESTAMP NOT NULL,
       completed_at TIMESTAMP,
       status VARCHAR NOT NULL,
       error_summary VARCHAR
     )"
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS analysis.viewer_video_activity (
       talent_code VARCHAR NOT NULL,
       channel_id VARCHAR NOT NULL,
       video_id VARCHAR NOT NULL,
       user_id VARCHAR NOT NULL,
       latest_username_in_video VARCHAR,
       stream_at TIMESTAMP NOT NULL,
       message_count BIGINT NOT NULL,
       text_message_count BIGINT NOT NULL,
       paid_message_count BIGINT NOT NULL,
       membership_event_count BIGINT NOT NULL,
       first_message_second DOUBLE,
       last_message_second DOUBLE,
       active_span_seconds DOUBLE,
       pipeline_run_id VARCHAR NOT NULL,
       calculated_at TIMESTAMP NOT NULL,
       PRIMARY KEY (talent_code, channel_id, video_id, user_id)
     )"
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS analysis.viewer_profiles (
       talent_code VARCHAR NOT NULL,
       user_id VARCHAR NOT NULL,
       current_username VARCHAR,
       first_observed_stream_at TIMESTAMP NOT NULL,
       first_observed_video_id VARCHAR NOT NULL,
       first_observed_message_second DOUBLE,
       last_observed_stream_at TIMESTAMP NOT NULL,
       last_observed_video_id VARCHAR NOT NULL,
       last_observed_message_second DOUBLE,
       videos_chatted_in BIGINT NOT NULL,
       total_messages BIGINT NOT NULL,
       text_messages BIGINT NOT NULL,
       paid_messages BIGINT NOT NULL,
       membership_events BIGINT NOT NULL,
       first_membership_event_stream_at TIMESTAMP,
       mean_messages_per_active_video DOUBLE NOT NULL,
       median_messages_per_active_video DOUBLE NOT NULL,
       public_subscription_matched BOOLEAN NOT NULL,
       public_subscribed_at TIMESTAMP,
       public_subscriber_snapshot_date DATE,
       subscription_date_versions BIGINT,
       subscription_date_changed BOOLEAN NOT NULL,
       pipeline_run_id VARCHAR NOT NULL,
       calculated_at TIMESTAMP NOT NULL,
       PRIMARY KEY (talent_code, user_id)
     )"
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS analysis.company_viewer_profiles (
       company_code VARCHAR NOT NULL,
       user_id VARCHAR NOT NULL,
       current_username VARCHAR,
       configured_talent_count BIGINT NOT NULL,
       talents_chatted_with BIGINT NOT NULL,
       talent_reach_rate DOUBLE NOT NULL,
       talent_codes VARCHAR[] NOT NULL,
       primary_talent_code VARCHAR NOT NULL,
       primary_talent_message_share DOUBLE NOT NULL,
       cross_talent_chatter BOOLEAN NOT NULL,
       first_observed_stream_at TIMESTAMP NOT NULL,
       last_observed_stream_at TIMESTAMP NOT NULL,
       videos_chatted_in BIGINT NOT NULL,
       total_messages BIGINT NOT NULL,
       text_messages BIGINT NOT NULL,
       paid_messages BIGINT NOT NULL,
       membership_events BIGINT NOT NULL,
       first_membership_event_stream_at TIMESTAMP,
       public_subscription_talent_count BIGINT NOT NULL,
       public_subscribed_talent_codes VARCHAR[],
       earliest_public_subscribed_at TIMESTAMP,
       latest_public_subscribed_at TIMESTAMP,
       company_mapping_checksum VARCHAR NOT NULL,
       pipeline_run_id VARCHAR NOT NULL,
       calculated_at TIMESTAMP NOT NULL,
       PRIMARY KEY (company_code, user_id)
     )"
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS analysis.global_viewer_profiles (
       user_id VARCHAR PRIMARY KEY,
       current_username VARCHAR,
       talents_chatted_with BIGINT NOT NULL,
       talent_codes VARCHAR[] NOT NULL,
       primary_talent_code VARCHAR NOT NULL,
       primary_talent_message_share DOUBLE NOT NULL,
       cross_talent_chatter BOOLEAN NOT NULL,
       first_observed_stream_at TIMESTAMP NOT NULL,
       last_observed_stream_at TIMESTAMP NOT NULL,
       videos_chatted_in BIGINT NOT NULL,
       total_messages BIGINT NOT NULL,
       text_messages BIGINT NOT NULL,
       paid_messages BIGINT NOT NULL,
       membership_events BIGINT NOT NULL,
       first_membership_event_stream_at TIMESTAMP,
       public_subscription_talent_count BIGINT NOT NULL,
       public_subscribed_talent_codes VARCHAR[],
       earliest_public_subscribed_at TIMESTAMP,
       latest_public_subscribed_at TIMESTAMP,
       pipeline_run_id VARCHAR NOT NULL,
       calculated_at TIMESTAMP NOT NULL
     )"
  )

  invisible(TRUE)
}
