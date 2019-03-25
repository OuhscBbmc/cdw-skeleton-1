SELECT
  DISTINCT
  cast(md.masterdiagnosisid AS VARCHAR(10))     AS diagnosis_id,
  '{category}'                                  AS category,
  '{broad_match_keyword}'                       AS match_keyword,
  md.code                                       AS code,
  CASE  WHEN md.CODETYPE = 1 THEN 'ICD-9-CM'
  --		WHEN md.CODETYPE = 2 THEN 'SNOMED CT'
        WHEN md.CODETYPE = 8 THEN 'ICD-10-CM'
  END                                           AS code_type,
  lower(md.longdescription)                     AS code_description,
  '{project}'                                   AS project,
  'FALSE'                                       AS desired
FROM
  centricity.masterdiagnosis  md
WHERE
  md.longdescription like '%{broad_match_keyword}%'
  and (md.CODETYPE = 1 or md.codetype = 8)
