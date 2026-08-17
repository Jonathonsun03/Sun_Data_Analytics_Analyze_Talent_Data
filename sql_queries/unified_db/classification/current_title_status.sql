-- One row per available video for the active title-classification version.
-- classification_status is classified, changed_title, or new_or_unclassified.
SELECT
  video_id,
  talent_code,
  channel_id,
  title,
  title_hash,
  published_at,
  title_version_id,
  has_version_result,
  is_classified,
  classification_status
FROM classification.title_classification_status
ORDER BY talent_code, published_at, video_id;
