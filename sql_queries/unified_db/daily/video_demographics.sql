-- Video demographic segments for one talent on one snapshot date.
-- Parameters: 1 = talent_code, 2 = snapshot_date.
SELECT *
FROM clean.video_demographics
WHERE talent_code = ?
  AND snapshot_date = CAST(? AS DATE)
ORDER BY
  video_id,
  viewer_age,
  viewer_gender;
