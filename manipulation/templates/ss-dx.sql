-- ================================================================================================
-- TEMPLATE: ss-dx.sql
-- Source:   cdw_outpost.lexis.dim_dx
-- Purpose:  Discovery query — find diagnosis concepts matching study keywords or ICD patterns.
--           Run this query, review results in SSMS or Excel, then set desired = 'TRUE' and
--           fill in category_1 (and category_2 if needed) for the rows to include.
--           The final ss_dx table is loaded from the approved rows.
--
--           Joins in dx.sql:   inner join {project_schema}.ss_dx ss on pr.problem_concept_id = ss.concept_id
--           Columns selected:  ss.category_1, ss.category_2, ss.category_3, ss.category_4
--
-- !! CUSTOMIZE keyword patterns and/or ICD code filters below
-- !! CUSTOMIZE number of category columns to match what dx.sql expects
-- ================================================================================================

-- ---- STEP 1: Discovery — review and annotate output before loading ss_dx -------------------

SELECT
  distinct
  d.concept_id
  ,d.vocabulary_id
  ,d.icd_code
  ,d.icd_description
  -- Mark desired and assign category after reviewing output:
  ,'TRUE'   as desired
  ,''       as category_1
  ,''       as category_2
FROM cdw_outpost.lexis.dim_dx d
WHERE
  -- Option A — keyword search on description:
  d.icd_description like '%{keyword}%'

  -- Option B — ICD code prefix (comment out Option A):
  -- d.icd_code like 'C61%'
  -- or d.icd_code like 'D07.5%'

  -- Option C — explicit ICD code list (comment out Option A):
  -- d.icd_code in ('N18.3', 'N18.4', 'N18.5', 'N18.6', 'N18.9')

  -- Add vocabulary filter if needed:
  -- and d.vocabulary_id = 'ICD10CM'
ORDER BY d.vocabulary_id, d.icd_code;


-- ---- STEP 2: Load ss_dx after PI review -----------------------------------------------------
-- After annotating the output above (desired, category_1, etc.), run this block
-- to populate the study-specific lookup table. Adjust the SELECT to match your annotations.

/*
DROP TABLE IF EXISTS [cdw_cache_staging].[{project_schema}].[ss_dx];
CREATE TABLE [cdw_cache_staging].[{project_schema}].[ss_dx] (
    concept_id      int             not null primary key,
    vocabulary_id   varchar(7)      not null,
    icd_code        varchar(8)      not null,
    icd_description varchar(255)    not null,
    category_1      varchar(100),
    category_2      varchar(100),
    -- Add category_3, category_4 as needed
);

INSERT INTO {project_schema}.ss_dx
SELECT
    d.concept_id
    ,d.vocabulary_id
    ,d.icd_code
    ,d.icd_description
    ,'{category_1}'     as category_1
    ,null               as category_2
FROM cdw_outpost.lexis.dim_dx d
WHERE
    d.concept_id in ({concept_id_1}, {concept_id_2})   -- paste approved concept_ids
ORDER BY d.vocabulary_id, d.icd_code;
*/
