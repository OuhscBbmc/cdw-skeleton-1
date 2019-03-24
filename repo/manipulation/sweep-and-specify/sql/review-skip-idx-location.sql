SELECT
  DISTINCT
  cast(locpk AS VARCHAR(16))		     AS loc_id,
  '{category}'							         AS category,
  '{exact_match_keyword}'            AS match_keyword,
  lower(locname)						         AS loc_name,
  lower(locmnem)						         AS loc_abbrevated,
  locdeactflag						           AS loc_deactivated_flag,
  locdeletedflag						         AS loc_deleted_flag,
  '{project}'						             AS project,
  'TRUE'							               AS desired
FROM
  idx.dim_location
WHERE
  locname = '{exact_match_keyword}'
