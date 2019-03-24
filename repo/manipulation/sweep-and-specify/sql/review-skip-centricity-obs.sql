SELECT
  DISTINCT
  cast(oh.hdid AS varchar(10))  AS obs_hdid,
  '{category}'                  AS category,
  '{exact_match_keyword}'       AS match_keyword,
  lower(oh.name)                AS obs_name,
  lower(oh.description)         AS obs_description,
  lower(oh.unit)                AS obs_units,
  cast(oh.active AS varchar(1)) AS obs_active_flag,
  '{project}'                   AS project,
  'TRUE'                        AS desired
FROM
  centricity.obshead oh
WHERE
  oh.name = '{exact_match_keyword}' 
