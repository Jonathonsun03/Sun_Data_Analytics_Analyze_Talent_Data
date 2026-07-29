-- One row per code definition in a qualitative codebook.
-- Parameters: 1 = codebook_id.
SELECT
  primary_code_id AS "Primary Code ID",
  primary_code_name AS "Primary Code",
  secondary_code_id AS "Secondary Code ID",
  secondary_code_name AS "Secondary Code",
  definition AS "Definition",
  examples AS "Examples from text",
  code_id,
  code_column_name
FROM qualitative.codebooks
WHERE codebook_id = ?
ORDER BY display_order, code_id;
