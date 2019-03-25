SELECT
  DISTINCT
  cast(cptpk AS VARCHAR(10))			AS cpt_id,
  '{category}'							      AS category,
  '{broad_match_keyword}'         AS match_keyword,
  lower(cptcode	)				          AS cpt_code,
  lower(cptdesc	)				          AS code_description,
  lower(cptcatname)				        AS cpt_category_name,
  cptdeactflag                    AS cpt_deactivated_flag,
  cptdeletedflag                  AS cpt_deleted_flag,
  '{project}'							        AS project,
  'FALSE'								          AS desired
FROM
  idx.dim_cpt
WHERE
  cptdesc like '%{broad_match_keyword}%'
