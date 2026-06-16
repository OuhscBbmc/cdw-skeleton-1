SELECT
  distinct
  cast(schlocpk as varchar(10))     as loc_id,
  '{category}'                     as category,
  '{broad_match_keyword}'          as match_keyword,
  schloc                           as loc_name,
  schlocmnem                       as loc_abbrevated,
  schlocbillinglocname             as loc_billing_name,
  schlocclinicname                 as loc_clinic_name,
  schlocdeactflag                  as loc_deactivated_flag,
  schlocdeletedflag                as loc_deleted_flag,
  '{project}'                       as project,
  'FALSE'                           as desired
FROM
  idx.dim_schedloc
WHERE
  (schlocclinicname like '%{broad_match_keyword}%' or schloc like '%{broad_match_keyword}%')
