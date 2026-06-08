SELECT
  distinct
  cast(locpk as varchar(16))         as loc_id,
  '{category}'                       as category,
  '{exact_match_keyword}'            as match_keyword,
  lower(locname)                     as loc_name,
  lower(locmnem)                     as loc_abbrevated,
  locdeactflag                       as loc_deactivated_flag,
  locdeletedflag                     as loc_deleted_flag,
  '{project}'                         as project,
  'TRUE'                             as desired
FROM
  idx.dim_location
WHERE
  locname = '{exact_match_keyword}'
