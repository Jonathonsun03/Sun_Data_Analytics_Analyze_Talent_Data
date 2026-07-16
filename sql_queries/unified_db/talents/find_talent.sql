-- Talents whose talent or channel name contains the search text.
-- Parameters: 1 = talent name search, 2 = channel name search.
SELECT
  talent_code,
  talent_name,
  channel_id,
  channel_name,
  legacy_talent_id
FROM main.talents
WHERE talent_name ILIKE '%' || ? || '%'
   OR channel_name ILIKE '%' || ? || '%'
ORDER BY talent_name;
