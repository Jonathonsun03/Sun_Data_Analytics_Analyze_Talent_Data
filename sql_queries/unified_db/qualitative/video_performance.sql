-- One row per selected video with analysis-ready latest performance measures.
SELECT
  performance.video_id,
  CAST(performance.views AS DOUBLE) AS analytics_views,
  performance.estimated_minutes_watched
    AS analytics_estimated_minutes_watched,
  performance.average_view_duration
    AS analytics_average_view_duration,
  performance.average_view_percentage
    AS analytics_average_view_percentage,
  CAST(performance.subscribers_gained AS DOUBLE)
    AS analytics_subscribers_gained,
  CAST(performance.subscribers_lost AS DOUBLE)
    AS analytics_subscribers_lost,
  performance.duration_seconds / 60.0
    AS analytics_duration_minutes,
  sin(
    2 * pi() * extract(hour FROM performance.published_at) / 24.0
  ) AS analytics_publish_hour_sin,
  cos(
    2 * pi() * extract(hour FROM performance.published_at) / 24.0
  ) AS analytics_publish_hour_cos,
  CAST(
    dayofweek(performance.published_at) IN (0, 6)
    AS INTEGER
  ) AS analytics_is_weekend
FROM analytics.video_latest_performance performance
JOIN qualitative_loader_video_ids selected
  ON performance.video_id = selected.video_id
ORDER BY performance.video_id;
