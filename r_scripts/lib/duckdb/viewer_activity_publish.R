viewer_activity_new_run_id <- function(now = Sys.time()) {
  paste0(
    "viewer_activity_",
    format(as.POSIXct(now, tz = "UTC"), "%Y%m%dT%H%M%SZ", tz = "UTC"),
    "_",
    sprintf("%08x", sample.int(.Machine$integer.max, 1L))
  )
}

viewer_activity_validate_company_memberships <- function(con, memberships) {
  required <- c("company_code", "company_name", "talent_code")
  missing <- setdiff(required, names(memberships))
  if (length(missing) > 0L) {
    stop(
      "Company talent mapping is missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  memberships <- memberships[, required, drop = FALSE]
  memberships[] <- lapply(memberships, function(value) trimws(as.character(value)))
  if (nrow(memberships) == 0L || any(!nzchar(as.matrix(memberships)))) {
    stop("Company talent mapping values cannot be empty.", call. = FALSE)
  }
  if (anyDuplicated(memberships[c("company_code", "talent_code")])) {
    stop("Company/talent pairs must be unique.", call. = FALSE)
  }

  inconsistent_names <- memberships |>
    dplyr::distinct(.data$company_code, .data$company_name) |>
    dplyr::count(.data$company_code, name = "name_count") |>
    dplyr::filter(.data$name_count > 1L)
  if (nrow(inconsistent_names) > 0L) {
    stop("Each company_code must have exactly one company_name.", call. = FALSE)
  }

  active_talents <- DBI::dbGetQuery(
    con,
    "SELECT talent_code FROM catalog.talents WHERE active"
  )$talent_code
  unknown <- setdiff(memberships$talent_code, active_talents)
  if (length(unknown) > 0L) {
    stop(
      "Company mapping contains unknown or inactive talent codes: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  memberships |>
    dplyr::arrange(.data$company_code, .data$talent_code)
}

viewer_activity_register_memberships <- function(con, memberships) {
  relation_name <- "viewer_activity_company_memberships"
  try(duckdb::duckdb_unregister(con, relation_name), silent = TRUE)
  duckdb::duckdb_register(con, relation_name, as.data.frame(memberships))
  relation_name
}

viewer_activity_video_insert_sql <- function() {
  "INSERT INTO analysis.viewer_video_activity (
     talent_code, channel_id, video_id, user_id,
     latest_username_in_video, stream_at,
     message_count, text_message_count, paid_message_count,
     membership_event_count, first_message_second, last_message_second,
     active_span_seconds, pipeline_run_id, calculated_at
   )
   SELECT
     chat.talent_code,
     chat.channel_id,
     chat.video_id,
     chat.user_id,
     ARG_MAX(chat.username, COALESCE(chat.time_in_seconds, -1)),
     COALESCE(video.actual_start_at, video.published_at) AS stream_at,
     COUNT(*) AS message_count,
     COUNT(*) FILTER (WHERE chat.message_type = 'text_message'),
     COUNT(*) FILTER (WHERE chat.message_type = 'paid_message'),
     COUNT(*) FILTER (WHERE chat.message_type = 'membership_item'),
     MIN(chat.time_in_seconds),
     MAX(chat.time_in_seconds),
     GREATEST(
       MAX(chat.time_in_seconds) - MIN(chat.time_in_seconds),
       0
     ),
     ?,
     ?
   FROM text.chat_messages AS chat
   JOIN catalog.videos AS video
     USING (talent_code, channel_id, video_id)
   WHERE chat.user_id IS NOT NULL
     AND LENGTH(TRIM(chat.user_id)) > 0
     AND COALESCE(video.actual_start_at, video.published_at) IS NOT NULL
   GROUP BY
     chat.talent_code, chat.channel_id, chat.video_id, chat.user_id,
     stream_at"
}

viewer_activity_profile_insert_sql <- function() {
  "INSERT INTO analysis.viewer_profiles (
     talent_code, user_id, current_username,
     first_observed_stream_at, first_observed_video_id,
     first_observed_message_second,
     last_observed_stream_at, last_observed_video_id,
     last_observed_message_second,
     videos_chatted_in, total_messages, text_messages, paid_messages,
     membership_events, first_membership_event_stream_at,
     mean_messages_per_active_video, median_messages_per_active_video,
     public_subscription_matched, public_subscribed_at,
     public_subscriber_snapshot_date, subscription_date_versions,
     subscription_date_changed, pipeline_run_id, calculated_at
   )
   WITH subscription_history AS (
     SELECT
       talent_code,
       subscriber_channel_id,
       COUNT(DISTINCT subscribed_at) AS subscription_date_versions
     FROM clean.public_subscriber_snapshots
     GROUP BY talent_code, subscriber_channel_id
   ), ranked_subscriptions AS (
     SELECT
       talent_code,
       subscriber_channel_id,
       subscribed_at,
       snapshot_date,
       ROW_NUMBER() OVER (
         PARTITION BY talent_code, subscriber_channel_id
         ORDER BY snapshot_date DESC, ingested_at DESC, subscribed_at DESC
       ) AS snapshot_rank
     FROM clean.public_subscriber_snapshots
   ), latest_subscriptions AS (
     SELECT
       ranked.talent_code,
       ranked.subscriber_channel_id,
       ranked.subscribed_at,
       ranked.snapshot_date,
       history.subscription_date_versions
     FROM ranked_subscriptions AS ranked
     JOIN subscription_history AS history
       USING (talent_code, subscriber_channel_id)
     WHERE ranked.snapshot_rank = 1
   ), profile_rollup AS (
     SELECT
       talent_code,
       user_id,
       ARG_MAX(current_username, last_ordering_key) AS current_username,
       MIN(stream_at) AS first_observed_stream_at,
       ARG_MIN(video_id, first_ordering_key) AS first_observed_video_id,
       ARG_MIN(first_message_second, first_ordering_key) AS first_message_second,
       MAX(stream_at) AS last_observed_stream_at,
       ARG_MAX(video_id, last_ordering_key) AS last_observed_video_id,
       ARG_MAX(last_message_second, last_ordering_key) AS last_message_second,
       COUNT(*) AS videos_chatted_in,
       SUM(message_count) AS total_messages,
       SUM(text_message_count) AS text_messages,
       SUM(paid_message_count) AS paid_messages,
       SUM(membership_event_count) AS membership_events,
       MIN(stream_at) FILTER (
         WHERE membership_event_count > 0
       ) AS first_membership_event_stream_at,
       AVG(message_count) AS mean_messages_per_active_video,
       MEDIAN(message_count) AS median_messages_per_active_video
     FROM (
       SELECT
         activity.*,
         activity.latest_username_in_video AS current_username,
         STRUCT_PACK(
           stream_at := activity.stream_at,
           message_second := COALESCE(activity.first_message_second, -1),
           video_id := activity.video_id
         ) AS first_ordering_key,
         STRUCT_PACK(
           stream_at := activity.stream_at,
           message_second := COALESCE(activity.last_message_second, -1),
           video_id := activity.video_id
         ) AS last_ordering_key
       FROM analysis.viewer_video_activity AS activity
     ) AS ordered_activity
     GROUP BY talent_code, user_id
   )
   SELECT
     profile.talent_code,
     profile.user_id,
     profile.current_username,
     profile.first_observed_stream_at,
     profile.first_observed_video_id,
     profile.first_message_second,
     profile.last_observed_stream_at,
     profile.last_observed_video_id,
     profile.last_message_second,
     profile.videos_chatted_in,
     profile.total_messages,
     profile.text_messages,
     profile.paid_messages,
     profile.membership_events,
     profile.first_membership_event_stream_at,
     profile.mean_messages_per_active_video,
     profile.median_messages_per_active_video,
     subscription.subscriber_channel_id IS NOT NULL,
     subscription.subscribed_at,
     subscription.snapshot_date,
     subscription.subscription_date_versions,
     COALESCE(subscription.subscription_date_versions > 1, FALSE),
     ?,
     ?
   FROM profile_rollup AS profile
   LEFT JOIN latest_subscriptions AS subscription
     ON subscription.talent_code = profile.talent_code
    AND subscription.subscriber_channel_id = profile.user_id"
}

viewer_activity_company_profile_insert_sql <- function() {
  "INSERT INTO analysis.company_viewer_profiles (
     company_code, user_id, current_username, configured_talent_count,
     talents_chatted_with, talent_reach_rate, talent_codes,
     primary_talent_code, primary_talent_message_share,
     cross_talent_chatter, first_observed_stream_at,
     last_observed_stream_at, videos_chatted_in, total_messages,
     text_messages, paid_messages, membership_events,
     first_membership_event_stream_at,
     public_subscription_talent_count, public_subscribed_talent_codes,
     earliest_public_subscribed_at, latest_public_subscribed_at,
     company_mapping_checksum, pipeline_run_id, calculated_at
   )
   WITH configured AS (
     SELECT company_code, COUNT(DISTINCT talent_code) AS configured_talent_count
     FROM viewer_activity_company_memberships
     GROUP BY company_code
   ), member_activity AS (
     SELECT membership.company_code, profile.*
     FROM analysis.viewer_profiles AS profile
     JOIN viewer_activity_company_memberships AS membership
       USING (talent_code)
   )
   SELECT
     activity.company_code,
     activity.user_id,
     ARG_MAX(activity.current_username, activity.last_observed_stream_at),
     configured.configured_talent_count,
     COUNT(DISTINCT activity.talent_code) AS talents_chatted_with,
     COUNT(DISTINCT activity.talent_code)::DOUBLE /
       configured.configured_talent_count AS talent_reach_rate,
     LIST(DISTINCT activity.talent_code ORDER BY activity.talent_code),
     ARG_MAX(activity.talent_code, activity.total_messages),
     MAX(activity.total_messages)::DOUBLE / SUM(activity.total_messages),
     COUNT(DISTINCT activity.talent_code) > 1,
     MIN(activity.first_observed_stream_at),
     MAX(activity.last_observed_stream_at),
     SUM(activity.videos_chatted_in),
     SUM(activity.total_messages),
     SUM(activity.text_messages),
     SUM(activity.paid_messages),
     SUM(activity.membership_events),
     MIN(activity.first_membership_event_stream_at),
     COUNT(*) FILTER (WHERE activity.public_subscription_matched),
     LIST(DISTINCT activity.talent_code ORDER BY activity.talent_code)
       FILTER (WHERE activity.public_subscription_matched),
     MIN(activity.public_subscribed_at),
     MAX(activity.public_subscribed_at),
     ?,
     ?,
     ?
   FROM member_activity AS activity
   JOIN configured USING (company_code)
   GROUP BY
     activity.company_code,
     activity.user_id,
     configured.configured_talent_count"
}

viewer_activity_global_profile_insert_sql <- function() {
  "INSERT INTO analysis.global_viewer_profiles (
     user_id, current_username, talents_chatted_with, talent_codes,
     primary_talent_code, primary_talent_message_share,
     cross_talent_chatter, first_observed_stream_at,
     last_observed_stream_at, videos_chatted_in, total_messages,
     text_messages, paid_messages, membership_events,
     first_membership_event_stream_at,
     public_subscription_talent_count, public_subscribed_talent_codes,
     earliest_public_subscribed_at, latest_public_subscribed_at,
     pipeline_run_id, calculated_at
   )
   SELECT
     user_id,
     ARG_MAX(current_username, last_observed_stream_at),
     COUNT(DISTINCT talent_code) AS talents_chatted_with,
     LIST(DISTINCT talent_code ORDER BY talent_code),
     ARG_MAX(talent_code, total_messages),
     MAX(total_messages)::DOUBLE / SUM(total_messages),
     COUNT(DISTINCT talent_code) > 1,
     MIN(first_observed_stream_at),
     MAX(last_observed_stream_at),
     SUM(videos_chatted_in),
     SUM(total_messages),
     SUM(text_messages),
     SUM(paid_messages),
     SUM(membership_events),
     MIN(first_membership_event_stream_at),
     COUNT(*) FILTER (WHERE public_subscription_matched),
     LIST(DISTINCT talent_code ORDER BY talent_code)
       FILTER (WHERE public_subscription_matched),
     MIN(public_subscribed_at),
     MAX(public_subscribed_at),
     ?,
     ?
   FROM analysis.viewer_profiles
   GROUP BY user_id"
}

viewer_activity_source_summary <- function(con, memberships) {
  mapped_talents <- unique(memberships$talent_code)
  placeholders <- paste(rep("?", length(mapped_talents)), collapse = ", ")
  DBI::dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COUNT(*) AS chat_rows, ",
      "COUNT(DISTINCT chat.user_id) AS global_viewer_profiles, ",
      "COUNT(DISTINCT (chat.talent_code, chat.user_id)) AS viewer_profiles, ",
      "COUNT(DISTINCT (chat.talent_code, chat.channel_id, chat.video_id, chat.user_id)) ",
      "  AS viewer_video_rows, ",
      "COUNT(DISTINCT chat.user_id) FILTER (",
      "  WHERE chat.talent_code IN (", placeholders, ")",
      ") AS mapped_company_viewers ",
      "FROM text.chat_messages AS chat ",
      "JOIN catalog.videos AS video USING (talent_code, channel_id, video_id) ",
      "WHERE chat.user_id IS NOT NULL ",
      "AND LENGTH(TRIM(chat.user_id)) > 0 ",
      "AND COALESCE(video.actual_start_at, video.published_at) IS NOT NULL"
    ),
    params = as.list(mapped_talents)
  )
}

publish_viewer_activity <- function(
    con,
    company_memberships,
    company_mapping_checksum,
    dry_run = TRUE) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("duckdb", quietly = TRUE) ||
      !requireNamespace("dplyr", quietly = TRUE)) {
    stop("Packages `DBI`, `duckdb`, and `dplyr` are required.", call. = FALSE)
  }
  if (length(company_mapping_checksum) != 1L ||
      is.na(company_mapping_checksum) ||
      !nzchar(trimws(company_mapping_checksum))) {
    stop("A company mapping checksum is required.", call. = FALSE)
  }

  memberships <- viewer_activity_validate_company_memberships(
    con,
    company_memberships
  )
  summary <- viewer_activity_source_summary(con, memberships)
  if (isTRUE(dry_run)) {
    return(list(
      published = FALSE,
      pipeline_run_id = NA_character_,
      source_summary = summary,
      company_memberships = memberships
    ))
  }

  init_viewer_activity_schema(con)
  run_id <- viewer_activity_new_run_id()
  started_at <- as.POSIXct(Sys.time(), tz = "UTC")
  calculated_at <- started_at
  DBI::dbExecute(
    con,
    "INSERT INTO ops.pipeline_runs (
       pipeline_run_id, pipeline_name, started_at, status
     ) VALUES (?, 'viewer_activity_refresh', ?, 'running')",
    params = list(run_id, started_at)
  )

  mapping_relation <- viewer_activity_register_memberships(con, memberships)
  on.exit(try(duckdb::duckdb_unregister(con, mapping_relation), silent = TRUE), add = TRUE)

  transaction_open <- FALSE
  tryCatch(
    {
      DBI::dbBegin(con)
      transaction_open <- TRUE

      DBI::dbExecute(con, "DELETE FROM analysis.company_viewer_profiles")
      DBI::dbExecute(con, "DELETE FROM analysis.global_viewer_profiles")
      DBI::dbExecute(con, "DELETE FROM analysis.viewer_profiles")
      DBI::dbExecute(con, "DELETE FROM analysis.viewer_video_activity")

      DBI::dbExecute(
        con,
        viewer_activity_video_insert_sql(),
        params = list(run_id, calculated_at)
      )
      DBI::dbExecute(
        con,
        viewer_activity_profile_insert_sql(),
        params = list(run_id, calculated_at)
      )
      DBI::dbExecute(
        con,
        viewer_activity_global_profile_insert_sql(),
        params = list(run_id, calculated_at)
      )
      DBI::dbExecute(
        con,
        viewer_activity_company_profile_insert_sql(),
        params = list(
          company_mapping_checksum,
          run_id,
          calculated_at
        )
      )

      DBI::dbExecute(
        con,
        "UPDATE ops.pipeline_runs
         SET completed_at = ?, status = 'completed', error_summary = NULL
         WHERE pipeline_run_id = ?",
        params = list(as.POSIXct(Sys.time(), tz = "UTC"), run_id)
      )
      DBI::dbCommit(con)
      transaction_open <- FALSE
    },
    error = function(error) {
      if (transaction_open) {
        try(DBI::dbRollback(con), silent = TRUE)
      }
      try(
        DBI::dbExecute(
          con,
          "UPDATE ops.pipeline_runs
           SET completed_at = ?, status = 'failed', error_summary = ?
           WHERE pipeline_run_id = ?",
          params = list(
            as.POSIXct(Sys.time(), tz = "UTC"),
            substr(conditionMessage(error), 1L, 1000L),
            run_id
          )
        ),
        silent = TRUE
      )
      stop(conditionMessage(error), call. = FALSE)
    }
  )

  row_counts <- DBI::dbGetQuery(
    con,
    "SELECT 'viewer_video_activity' AS relation, COUNT(*) AS rows
       FROM analysis.viewer_video_activity
     UNION ALL
     SELECT 'viewer_profiles', COUNT(*) FROM analysis.viewer_profiles
     UNION ALL
     SELECT 'global_viewer_profiles', COUNT(*)
       FROM analysis.global_viewer_profiles
     UNION ALL
     SELECT 'company_viewer_profiles', COUNT(*)
       FROM analysis.company_viewer_profiles
     ORDER BY relation"
  )

  list(
    published = TRUE,
    pipeline_run_id = run_id,
    source_summary = summary,
    row_counts = row_counts,
    company_memberships = memberships
  )
}
