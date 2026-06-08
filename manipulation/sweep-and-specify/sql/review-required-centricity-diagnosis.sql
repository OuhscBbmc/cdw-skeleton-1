SELECT
  distinct
  cast(md.masterdiagnosisid as varchar(10))     as diagnosis_id,
  '{category}'                                  as category,
  '{broad_match_keyword}'                       as match_keyword,
  md.code                                       as code,
  case  when md.CODETYPE = 1 then 'ICD-9-CM'
  --    WHEN md.CODETYPE = 2 THEN 'SNOMED CT'
    when md.CODETYPE = 8 then 'ICD-10-CM'
  end                                           as code_type,
  lower(md.longdescription)                     as code_description,
  '{project}'                                   as project,
  'FALSE'                                       as desired
FROM
  centricity.masterdiagnosis  md
WHERE
  md.longdescription like '%{broad_match_keyword}%'
  and (md.CODETYPE = 1 or md.codetype = 8)
