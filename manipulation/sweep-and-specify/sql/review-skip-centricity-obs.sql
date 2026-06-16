SELECT
  distinct
  cast(oh.hdid as varchar(10))  as obs_hdid,
  '{category}'                  as category,
  '{exact_match_keyword}'       as match_keyword,
  lower(oh.name)                as obs_name,
  lower(oh.description)         as obs_description,
  lower(oh.unit)                as obs_units,
  cast(oh.active as varchar(1)) as obs_active_flag,
  '{project}'                   as project,
  'TRUE'                        as desired
FROM
  centricity.obshead oh
WHERE
  oh.name = '{exact_match_keyword}'
