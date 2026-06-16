SELECT
  distinct
  cast(mid as varchar(16))  as mid,
  '{category}'              as category,
  '{exact_match_keyword}'   as match_keyword,
  lower(description)        as med_description,
  --instructions            AS med_instructions,
  lower(genericmed)         as med_generic,
  '{project}'               as project,
  'FALSE'                   as desired
FROM
  centricity.medicate
WHERE
  description = '{exact_match_keyword}'
