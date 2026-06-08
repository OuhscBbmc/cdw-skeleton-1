SELECT
  distinct
  cast(cptpk as varchar(10))        as cpt_id,
  '{category}'                      as category,
  '{exact_match_keyword}'           as match_keyword,
  lower(cptcode  )                    as cpt_code,
  lower(cptdesc  )                    as code_description,
  lower(cptcatname)                  as cpt_category_name,
  cptdeactflag                      as cpt_deactivated_flag,
  cptdeletedflag                    as cpt_deleted_flag,
  '{project}'                        as project,
  'TRUE'                            as desired
FROM
  idx.dim_cpt
WHERE
  cptcode = '{exact_match_keyword}'
