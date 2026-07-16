-- Daily subscriber metrics for one talent on one observation date.
-- Parameters: 1 = talent_code, 2 = date.
SELECT *
FROM clean.subscriber_daily
WHERE talent_code = ?
  AND date = CAST(? AS DATE)
ORDER BY channel_id;
