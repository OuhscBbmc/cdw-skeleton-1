-- ================================================================================================
-- TEMPLATE: elixhauser-comorbidities.sql
-- Source:   cdw_outpost.snowflake_2.problem + cdw_outpost.lexis.dx_elixhauser
-- Purpose:  One row per patient with binary flags for each Elixhauser comorbidity category.
--           More granular than Charlson (32 categories vs. 17); preferred for mortality/LOS models.
--
--           lexis.dx_elixhauser maps problem_concept_id → comorbidity category flags.
--           Adjust the WHERE date window to match your index date logic.
--
-- !! CUSTOMIZE {project_schema}
-- !! CUSTOMIZE the date window in WHERE
-- ================================================================================================

use cdw_cache_staging;


DROP TABLE IF EXISTS [cdw_cache_staging].[{project_schema}].[pt_elixhauser];
CREATE TABLE [cdw_cache_staging].[{project_schema}].[pt_elixhauser] (
    mrn_mpi                         int     not null primary key,
    dx_count_any                    int     not null,
    dx_count_elixhauser             int     not null,
    -- Elixhauser flags:
    congestive_heart_failure        bit     not null,
    cardiac_arrhythmias             bit     not null,
    valvular_diseases               bit     not null,
    pulmonary_circulation_disord    bit     not null,
    peripheral_vascular_disord      bit     not null,
    hypertension_uncomplicated      bit     not null,
    hypertension_complicated        bit     not null,
    paralysis                       bit     not null,
    other_neurological_disord       bit     not null,
    chronic_pulmonary_disease       bit     not null,
    diabetes_uncomp                 bit     not null,
    diabetes_comp                   bit     not null,
    hypothyroidism                  bit     not null,
    renal_failure                   bit     not null,
    liver_dis                       bit     not null,
    peptic_ulcer_dis                bit     not null,
    aids_hiv                        bit     not null,
    lymphoma                        bit     not null,
    meta_cancer                     bit     not null,
    solid_tumor_no_meta             bit     not null,
    rheum_arth                      bit     not null,
    coagulopathy                    bit     not null,
    obesity                         bit     not null,
    weight_loss                     bit     not null,
    fluid_electrolyte_disord        bit     not null,
    blood_loss_anemia               bit     not null,
    deficiency_anemia               bit     not null,
    alcohol_abuse                   bit     not null,
    drug_abuse                      bit     not null,
    psychoses                       bit     not null,
    depression                      bit     not null,
);

INSERT INTO {project_schema}.pt_elixhauser
SELECT
    pr.mrn_mpi
    ,count(distinct pr.problem_concept_id)                                          as dx_count_any
    ,count(distinct e.concept_id)                                                   as dx_count_elixhauser
    ,coalesce(max(cast(e.congestive_heart_failure        as tinyint)), 0)           as congestive_heart_failure
    ,coalesce(max(cast(e.cardiac_arrhythmias             as tinyint)), 0)           as cardiac_arrhythmias
    ,coalesce(max(cast(e.valvular_diseases               as tinyint)), 0)           as valvular_diseases
    ,coalesce(max(cast(e.pulmonary_circulation_disord    as tinyint)), 0)           as pulmonary_circulation_disord
    ,coalesce(max(cast(e.peripheral_vascular_disord      as tinyint)), 0)           as peripheral_vascular_disord
    ,coalesce(max(cast(e.hypertension_uncomplicated      as tinyint)), 0)           as hypertension_uncomplicated
    ,coalesce(max(cast(e.hypertension_complicated        as tinyint)), 0)           as hypertension_complicated
    ,coalesce(max(cast(e.paralysis                       as tinyint)), 0)           as paralysis
    ,coalesce(max(cast(e.other_neurological_disord       as tinyint)), 0)           as other_neurological_disord
    ,coalesce(max(cast(e.chronic_pulmonary_disease       as tinyint)), 0)           as chronic_pulmonary_disease
    ,coalesce(max(cast(e.diabetes_uncomp                 as tinyint)), 0)           as diabetes_uncomp
    ,coalesce(max(cast(e.diabetes_comp                   as tinyint)), 0)           as diabetes_comp
    ,coalesce(max(cast(e.hypothyroidism                  as tinyint)), 0)           as hypothyroidism
    ,coalesce(max(cast(e.renal_failure                   as tinyint)), 0)           as renal_failure
    ,coalesce(max(cast(e.liver_dis                       as tinyint)), 0)           as liver_dis
    ,coalesce(max(cast(e.peptic_ulcer_dis                as tinyint)), 0)           as peptic_ulcer_dis
    ,coalesce(max(cast(e.aids_hiv                        as tinyint)), 0)           as aids_hiv
    ,coalesce(max(cast(e.lymphoma                        as tinyint)), 0)           as lymphoma
    ,coalesce(max(cast(e.meta_cancer                     as tinyint)), 0)           as meta_cancer
    ,coalesce(max(cast(e.solid_tumor_no_meta             as tinyint)), 0)           as solid_tumor_no_meta
    ,coalesce(max(cast(e.rheum_arth                      as tinyint)), 0)           as rheum_arth
    ,coalesce(max(cast(e.coagulopathy                    as tinyint)), 0)           as coagulopathy
    ,coalesce(max(cast(e.obesity                         as tinyint)), 0)           as obesity
    ,coalesce(max(cast(e.weight_loss                     as tinyint)), 0)           as weight_loss
    ,coalesce(max(cast(e.fluid_electrolyte_disord        as tinyint)), 0)           as fluid_electrolyte_disord
    ,coalesce(max(cast(e.blood_loss_anemia               as tinyint)), 0)           as blood_loss_anemia
    ,coalesce(max(cast(e.deficiency_anemia               as tinyint)), 0)           as deficiency_anemia
    ,coalesce(max(cast(e.alcohol_abuse                   as tinyint)), 0)           as alcohol_abuse
    ,coalesce(max(cast(e.drug_abuse                      as tinyint)), 0)           as drug_abuse
    ,coalesce(max(cast(e.psychoses                       as tinyint)), 0)           as psychoses
    ,coalesce(max(cast(e.depression                      as tinyint)), 0)           as depression
FROM cdw_outpost.snowflake_2.problem pr
left  join cdw_outpost.lexis.dx_elixhauser e    on pr.problem_concept_id = e.concept_id
inner join {project_schema}.pt_pool p           on pr.mrn_mpi = p.mrn_mpi
WHERE
    -- !! Adjust window to match your study design:
    pr.problem_start_date between dateadd(year, -2, p.date_index) and p.date_index
GROUP BY pr.mrn_mpi;

-- (N rows affected) HH:MM:SS
