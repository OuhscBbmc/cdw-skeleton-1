-- ================================================================================================
-- TEMPLATE: dx-meditech.sql
-- Source:   cdw_meditech.meditech.dx
-- Applies:  encounter-level (billing) diagnoses; service dates < 2023-06-03
-- Purpose:  Encounter-associated diagnoses from Meditech - useful when you need the billing
--           diagnosis tied to a specific visit/account_number rather than the problem list.
--           For longitudinal problem-list diagnoses, prefer dx.sql (snowflake_2.problem).
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start       date = '{date_start}';
DECLARE @date_stop_legacy date = '2023-06-02';   -- hard ceiling; Meditech data ends here

DROP TABLE if exists {project_schema}.dx_meditech;
--exec dbo.generate_create_table_sp '{project_schema}.dx_meditech'
CREATE TABLE {project_schema}.dx_meditech (
  dx_meditech_index     int          identity primary key,
  account_number        char(12)     not null,
  mrn_mpi               int          not null,
  mrn_meditech_internal varchar(10)  not null,
  dx_priority           tinyint,   -- 1 = primary dx
  vocabulary_id         varchar(8),
  icd_code              varchar(20),
  omop_concept_id       int,
  icd_description       varchar(255),   -- FROM lexis.dx lookup
  visit_start_date      date,
  -- Study classification (optional):
  category_1            varchar(255),
  category_2            varchar(255),
);

INSERT {project_schema}.dx_meditech
SELECT
  d.account_number,na.mrn_mpi,d.mrn_meditech_internal,d.dx_priority,d.vocabulary_id,d.icd_code,d.omop_concept_id,lx.icd_description,d.visit_start_date,ss.category_1,ss.category_2
FROM cdw_meditech.meditech.dx d
  inner join cdw_mpi_1.groomed.node_assigned na   on d.mrn_meditech_internal = na.mrn_meditech_internal
  inner join {project_schema}.pt_pool pp           on na.mrn_mpi = pp.mrn_mpi
-- !! Join to your study-specific dx lookup; remove if pulling all diagnoses:
  inner join {project_schema}.ss_dx ss             on d.omop_concept_id = ss.concept_id
  left  join cdw_outpost.lexis.dx lx               on d.omop_concept_id = lx.concept_id
WHERE
  d.visit_start_date between @date_start and @date_stop_legacy
ORDER BY na.mrn_mpi, d.visit_start_date, d.dx_priority;

-- (N rows affected) HH:MM:SS
