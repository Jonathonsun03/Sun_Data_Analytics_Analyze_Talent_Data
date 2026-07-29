-- One row per selected coded transcript line, with catalog dimensions and codes.
-- Bound parameters are supplied by the optional rendered filter clauses.
-- Template fields contain caller-quoted identifiers or loader-generated clauses.
WITH coding_ranked AS (
  SELECT
    c.*,
    row_number() OVER (
      PARTITION BY c.transcript_line_id
      ORDER BY p.completed_at DESC NULLS LAST, c.pipeline_run_id DESC
    ) AS coding_rank
  FROM {{view_relation}} c
  LEFT JOIN ops.pipeline_runs p
    ON c.pipeline_run_id = p.pipeline_run_id
  {{coding_where}}
),
coding_selected AS (
  SELECT * EXCLUDE (coding_rank)
  FROM coding_ranked
  {{latest_filter}}
)
SELECT
  t.dataset_id,
  t.transcript_line_id,
  t.video_id,
  t.talent_code,
  talent.talent_name,
  video.title AS video_title,
  video.published_at,
  video.content_type,
  t.line_number,
  t.seconds,
  t.timecode,
  t.source,
  t.speaker,
  t.text,
  list_extract(t.source_record_keys, 1) AS source_record_key,
  t.alignment_status,
  t.source_file,
  t.legacy_row_id AS row_id,
  c.pipeline_run_id,
  c.codebook_id,
  c.request_custom_id,
  c.response_status,
  c.confidence,
  c.needs_review,
  c.review_reason,
  c.response_decision_count,
  c.response_duplicate_status,
  c.validation_error,
  {{code_columns}}
FROM qualitative.transcripts t
JOIN coding_selected c
  ON t.transcript_line_id = c.transcript_line_id
JOIN catalog.videos video
  ON t.video_id = video.video_id
JOIN catalog.talents talent
  ON t.talent_code = talent.talent_code
{{outer_where}}
ORDER BY t.talent_code, t.video_id, t.line_number;
