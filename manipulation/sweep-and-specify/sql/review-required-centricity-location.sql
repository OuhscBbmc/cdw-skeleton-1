SELECT
  distinct
  cast(locid as varchar(16))     as loc_id,
  '{category}'                   as category,
  '{broad_match_keyword}'        as match_keyword,
  l.name                         as loc_name,
  l.abbrevname                   as loc_abbrevated,
  concat( address1,', ',city,', ',state,', ',zip) as loc_address,
  '{project}'                    as project,
  'FALSE'                        as desired
FROM
  centricity.locreg l
WHERE
  l.name like '%{broad_match_keyword}%'
