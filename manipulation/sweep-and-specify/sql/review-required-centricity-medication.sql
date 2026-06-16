SELECT
  distinct
  cast(mid as varchar(16))  as mid,
  '{category}'              as category,
  '{broad_match_keyword}'   as match_keyword,
  lower(description)        as med_description,
  --instructions            AS med_instructions,
  lower(genericmed)         as med_generic,
  '{project}'               as project,
  'FALSE'                   as desired
FROM
  centricity.medicate
WHERE
  lower(description)   like '%{broad_match_keyword}%'
  or lower(genericmed) like '%{broad_match_keyword}%'
