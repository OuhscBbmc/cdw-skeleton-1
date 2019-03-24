SELECT
  DISTINCT
  cast(schlocpk as varchar(10))		 AS loc_id,
  '{category}'							       AS category,
  '{broad_match_keyword}'          AS match_keyword,
  schloc		                       AS loc_name,
  schlocmnem	                     AS loc_abbrevated,
  schlocbillinglocname             AS loc_billing_name,
  schlocclinicname		             AS loc_clinic_name,
  schlocdeactflag                  AS loc_deactivated_flag,
  schlocdeletedflag                AS loc_deleted_flag,
  '{project}'						           AS project,
  'FALSE'							             AS desired
FROM
  idx.dim_schedloc
WHERE
  (schlocclinicname like '%{broad_match_keyword}%' or schloc like '%{broad_match_keyword}%')
