-- One row containing the count of matching qualitative wide views.
-- Parameters: 1 = wide-view table name.
SELECT count(*) AS n
FROM information_schema.views
WHERE table_schema = 'qualitative'
  AND table_name = ?;
