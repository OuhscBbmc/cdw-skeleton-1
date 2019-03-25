SELECT
  DISTINCT
  cast(mid AS VARCHAR(16))  AS mid,
  '{category}'              AS category,
  '{broad_match_keyword}'   AS match_keyword,
  lower(description)        AS med_description,
  --instructions            AS med_instructions,
  lower(genericmed)         AS med_generic,
  '{project}'               AS project,
  'FALSE'                   AS desired
FROM
  centricity.medicate
WHERE
  lower(description)   like '%{broad_match_keyword}%'
  OR lower(genericmed) like '%{broad_match_keyword}%'
