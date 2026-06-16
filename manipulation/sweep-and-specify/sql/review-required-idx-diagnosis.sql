SELECT
  distinct
  cast(diagpk as varchar(16))    as diag_id,
  '{category}'                    as category,
  '{broad_match_keyword}'         as match_keyword,
  diagcd                         as code,
  case when diagicdcmversion = 9 then 'ICD-9'
    when diagicdcmversion = 10 then 'ICD-10'
  end                            as code_type,
  lower(diagdesc)                as code_description,
  diagdeactflag                  as code_deactivated_flag,
  diagdeletedflag                as code_deleted_flag,
  '{project}'                    as project,
  'FALSE'                        as desired
FROM
  idx.dim_diagnosis
WHERE
  diagdesc like '%{broad_match_keyword}%'
