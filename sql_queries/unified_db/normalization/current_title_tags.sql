-- One row per raw tag assignment for current, active-version title results.
SELECT
  result.video_id,
  result.talent_code,
  status.title_version_id,
  result.classification_json
FROM classification.title_classification_status AS status
JOIN classification.title_classification_results AS result
  ON result.video_id = status.video_id
 AND result.title_hash = status.title_hash
 AND result.title_version_id = status.title_version_id
WHERE status.is_classified
QUALIFY ROW_NUMBER() OVER (
  PARTITION BY result.video_id
  ORDER BY result.created_at DESC NULLS LAST,
           result.confidence DESC NULLS LAST
) = 1
ORDER BY result.video_id;
