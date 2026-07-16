-- Public subscriber observations for one talent on one snapshot date.
-- Parameters: 1 = talent_code, 2 = snapshot_date.
SELECT *
FROM clean.public_subscriber_snapshots
WHERE talent_code = ?
  AND snapshot_date = CAST(? AS DATE)
ORDER BY
  subscribed_at,
  subscriber_channel_id;
