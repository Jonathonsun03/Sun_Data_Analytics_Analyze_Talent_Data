-- One row per code in a qualitative codebook, including its wide-view metadata.
-- Parameters: 1 = codebook_id.
SELECT
  code_id,
  code_column_name,
  wide_view_name,
  display_order
FROM qualitative.codebooks
WHERE codebook_id = ?
ORDER BY display_order, code_id;
