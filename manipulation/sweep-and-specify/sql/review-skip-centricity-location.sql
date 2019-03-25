SELECT
  DISTINCT
  cast(locid AS VARCHAR(16))		 AS loc_id,
  '{category}'                   AS category,
  '{exact_match_keyword}'        AS match_keyword,
  l.name                         AS loc_name,
  l.abbrevname                   AS loc_abbrevated,
  concat( address1,', ',city,', ',state,', ',zip) AS loc_address,
  '{project}'                    AS project,
  'FALSE'                        AS desired
FROM
  centricity.locreg l
WHERE
  l.name = '{exact_match_keyword}'
