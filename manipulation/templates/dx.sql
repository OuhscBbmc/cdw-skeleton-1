-- ================================================================================================
-- TEMPLATE: dx.sql
-- Source:   cdw_outpost.snowflake_2.problem  (unified problem list across all systems/time periods)
-- Purpose:  Diagnoses / problem list for included patients
--           snowflake_2.problem is the preferred source — it harmonizes Meditech, Centricity,
--           GECB, and Epic problem lists into a single concept-mapped table.
--           Use dx-meditech.sql instead if you need encounter-level (billing) diagnoses only.
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop
-- !! CUSTOMIZE the ss_dx join to match your study-specific diagnosis lookup table
-- !! CUSTOMIZE SELECT columns (add/remove category columns as needed)
-- ================================================================================================

use cdw_cache_staging;

declare @date_start   date = '{date_start}';   -- earliest problem_start_date to include
declare @date_stop    date = '{date_stop}';    -- latest  problem_start_date to include


DROP TABLE IF EXISTS [cdw_cache_staging].[{project_schema}].[dx];
CREATE TABLE [cdw_cache_staging].[{project_schema}].[dx] (
    dx_index                int             identity(1,1) primary key,
    mrn_mpi                 int             not null,
    problem_concept_id      int             ,
    vocabulary_id           varchar(8)      not null,   -- 'ICD10CM' | 'ICD9CM'
    icd_code                varchar(12)     not null,
    icd_description         varchar(255)    null,
    problem_start_date      date            null,
    dx_index_within_patient int             not null,   -- 1 = earliest occurrence per concept per patient
    -- Study-specific classification columns (from ss_dx lookup); add/remove as needed:
    category_1              varchar(255)    null,
    category_2              varchar(255)    null,
    category_3              varchar(255)    null,
    category_4              varchar(255)    null,
);

INSERT {project_schema}.dx
SELECT
    pr.mrn_mpi
    ,pr.problem_concept_id
    ,pr.vocabulary_id
    ,pr.icd_code
    ,pr.icd_description
    ,pr.problem_start_date
    ,row_number() over (
        partition by pr.mrn_mpi, pr.problem_concept_id
        order by pr.problem_start_date
     )                                                              as dx_index_within_patient
    ,ss.category_1
    ,ss.category_2
    ,ss.category_3
    ,ss.category_4
FROM cdw_outpost.snowflake_2.problem pr
  inner join {project_schema}.pt_pool pp      on pr.mrn_mpi = pp.mrn_mpi
  -- !! Join to your study-specific dx lookup; remove if pulling all diagnoses:
  inner join {project_schema}.ss_dx ss         on pr.problem_concept_id = ss.concept_id
WHERE
    pr.problem_start_date between @date_start and @date_stop;

-- (N rows affected) HH:MM:SS
