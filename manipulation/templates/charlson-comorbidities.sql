-- ================================================================================================
-- TEMPLATE: charlson-comorbidities.sql
-- Source:   cdw_outpost.snowflake_2.problem + cdw_outpost.lexis.dx_charlson
-- Purpose:  One row per patient with binary flags for each Charlson comorbidity category,
--           derived from the problem list. Used for risk adjustment.
--
--           lexis.dx_charlson maps problem_concept_id → comorbidity category flags.
--           The date window filters problems to a clinically meaningful lookback window —
--           commonly 2 years before an index date. Adjust @window_start_col and @window_stop_col
--           to reference the appropriate date column in your pt_pool.
--
-- !! CUSTOMIZE {project_schema}
-- !! CUSTOMIZE the date window in WHERE — default is 2 years before pt_pool.date_index
-- ================================================================================================

use cdw_cache_staging;


DROP TABLE IF EXISTS [cdw_cache_staging].[{project_schema}].[pt_charlson];
CREATE TABLE [cdw_cache_staging].[{project_schema}].[pt_charlson] (
    mrn_mpi                         int     not null primary key,
    dx_count_any                    int     not null,
    dx_count_charlson               int     not null,
    -- Charlson comorbidity flags:
    myocardial_infarction           bit     not null,
    congestive_heart_failure        bit     not null,
    peripheral_vascular_disease     bit     not null,
    stroke                          bit     not null,
    dementia                        bit     not null,
    pulmonary                       bit     not null,
    rheumatic                       bit     not null,
    peptic_ulcer_disease            bit     not null,
    liver_mild                      bit     not null,
    diabetes_mellitis               bit     not null,
    diabetes_mellitis_complicated   bit     not null,
    paralysis                       bit     not null,
    renal                           bit     not null,
    cancer                          bit     not null,
    liver_severe                    bit     not null,
    metastatic_cancer               bit     not null,
    hiv                             bit     not null,
);

INSERT INTO {project_schema}.pt_charlson
SELECT
    pr.mrn_mpi
    ,count(distinct pr.problem_concept_id)                                  as dx_count_any
    ,count(distinct c.concept_id)                                           as dx_count_charlson
    ,coalesce(max(cast(c.myocardial_infarction           as int)), 0)       as myocardial_infarction
    ,coalesce(max(cast(c.congestive_heart_failure        as int)), 0)       as congestive_heart_failure
    ,coalesce(max(cast(c.peripheral_vascular_diseas      as int)), 0)       as peripheral_vascular_disease
    ,coalesce(max(cast(c.stroke                          as int)), 0)       as stroke
    ,coalesce(max(cast(c.dementia                        as int)), 0)       as dementia
    ,coalesce(max(cast(c.pulmonary                       as int)), 0)       as pulmonary
    ,coalesce(max(cast(c.rheumatic                       as int)), 0)       as rheumatic
    ,coalesce(max(cast(c.peptic_ulcer_disease            as int)), 0)       as peptic_ulcer_disease
    ,coalesce(max(cast(c.liver_mild                      as int)), 0)       as liver_mild
    ,coalesce(max(cast(c.diabetes_mellitis               as int)), 0)       as diabetes_mellitis
    ,coalesce(max(cast(c.diabetes_mellitis_complicated   as int)), 0)       as diabetes_mellitis_complicated
    ,coalesce(max(cast(c.paralysis                       as int)), 0)       as paralysis
    ,coalesce(max(cast(c.renal                           as int)), 0)       as renal
    ,coalesce(max(cast(c.cancer                          as int)), 0)       as cancer
    ,coalesce(max(cast(c.liver_severe                    as int)), 0)       as liver_severe
    ,coalesce(max(cast(c.metastatic_cancer               as int)), 0)       as metastatic_cancer
    ,coalesce(max(cast(c.hiv                             as int)), 0)       as hiv
FROM cdw_outpost.snowflake_2.problem pr
left  join cdw_outpost.lexis.dx_charlson c  on pr.problem_concept_id = c.concept_id
inner join {project_schema}.pt_pool p       on pr.mrn_mpi = p.mrn_mpi
WHERE
    -- !! Adjust window to match your study design:
    pr.problem_start_date between dateadd(year, -2, p.date_index) and p.date_index
    -- Alternatives:
    -- pr.problem_start_date <= p.date_index                 -- all prior history
    -- pr.problem_start_date between @date_start and @date_stop  -- fixed window
GROUP BY pr.mrn_mpi;

-- (N rows affected) HH:MM:SS
