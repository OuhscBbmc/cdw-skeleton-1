SELECT
  DISTINCT
   CAST(diagpk AS VARCHAR(16))    AS diag_id,
  '{category}'							      AS category,
  '{exact_match_keyword}'         AS match_keyword,
   diagcd                         AS code,
   CASE WHEN diagicdcmversion = 9 THEN 'ICD-9'
        WHEN diagicdcmversion = 10 THEN 'ICD-10'
   END                            AS code_type,
   lower(diagdesc)                AS code_description,
   diagdeactflag                  AS code_deactivated_flag,
   diagdeletedflag                AS code_deleted_flag,
  '{project}'                    AS project,
   'TRUE'                         AS desired
FROM
  idx.dim_diagnosis
WHERE
  diagcd = '{exact_match_keyword}'
