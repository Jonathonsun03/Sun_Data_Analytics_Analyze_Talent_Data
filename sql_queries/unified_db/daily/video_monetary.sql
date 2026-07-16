-- Video monetary metrics for one talent on one snapshot date.
-- Parameters: 1 = talent_code, 2 = snapshot_date.
SELECT *
FROM clean.video_monetary_snapshots
WHERE talent_code = ?
  AND snapshot_date = CAST(? AS DATE)
ORDER BY video_id;
