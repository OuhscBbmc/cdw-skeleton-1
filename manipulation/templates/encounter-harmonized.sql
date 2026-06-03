-- ================================================================================================
-- TEMPLATE: encounter-harmonized.sql
-- Source:   UNION of Meditech (< 2023-06-03) and Epic (>= 2023-06-03)
-- Purpose:  Single encounter table spanning the full study window across both systems.
--           Columns are harmonized to a common shape; system-specific fields nulled where absent.
--
-- !! REQUIRES encounter-meditech and encounter-epic tables to already be populated, OR
--    run this as a standalone CREATE + INSERT with both source systems inline (see below).
-- !! CUSTOMIZE {project_schema}, date params, campus/department filters
-- ================================================================================================

use cdw_cache_staging;

declare @date_start   date = '{date_start}';   -- overall study start
declare @date_stop    date = '{date_stop}';    -- overall study stop (may span both systems)


DROP TABLE IF EXISTS [cdw_cache_staging].[{project_schema}].[encounter];
CREATE TABLE [cdw_cache_staging].[{project_schema}].[encounter] (
    encounter_index             int             identity(1,1) primary key,
    source_system               varchar(10)     not null,   -- 'meditech' | 'epic'
    -- Universal encounter key: account_number for meditech, encounter_key for epic
    account_number              char(12)        null,       -- Meditech only
    encounter_key               int             null,       -- Epic only
    mrn_mpi                     int             not null,
    -- Encounter
    encounter_start_date        date            not null,
    encounter_end_date          date            null,
    length_of_stay              smallint        null,
    patient_class               varchar(50)     null,   -- 'inpatient' | 'outpatient' | 'emergency'
    facility                    varchar(100)    null,
    department_or_campus        varchar(100)    null,
    discharge_disposition       varchar(100)    null,
    insurance_category          varchar(100)    null,
);

-- ---- Meditech arm (service dates < 2023-06-03) -----------------------------------------------
INSERT INTO {project_schema}.encounter
SELECT
    'meditech'                                              as source_system
    ,v.account_number
    ,null                                                   as encounter_key
    ,na.mrn_mpi
    ,v.visit_start_date                                     as encounter_start_date
    ,v.visit_stop_date                                      as encounter_end_date
    ,v.length_of_stay
    ,v.patient_status                                       as patient_class
    ,v.facility
    ,v.campus                                               as department_or_campus
    ,v.discharge_disposition
    ,null                                                   as insurance_category  -- join to gecb separately if needed
FROM cdw_meditech.meditech.visit v
  inner join cdw_mpi_1.groomed.node_assigned na   on v.mrn_meditech_internal = na.mrn_meditech_internal
  inner join {project_schema}.pt_pool pp           on na.mrn_mpi = pp.mrn_mpi
WHERE
    v.visit_start_date between @date_start and '2023-06-02'

UNION ALL

-- ---- Epic arm (service dates >= 2023-06-03) --------------------------------------------------
SELECT
    'epic'                                                  as source_system
    ,null                                                   as account_number
    ,e.encounter_key
    ,na.mrn_mpi
    ,cast(e.encounter_start_date as date)                   as encounter_start_date
    ,cast(e.encounter_end_date   as date)                   as encounter_end_date
    ,datediff(day, e.encounter_start_date, e.encounter_end_date) as length_of_stay
    ,e.encounter_type                                       as patient_class
    ,e.location_name                                        as facility
    ,e.department_name                                      as department_or_campus
    ,e.discharge_disposition
    ,ba.primary_benefit_payor_class                         as insurance_category
FROM cdw_epic.caboodle.encounter e
  inner join cdw_mpi_1.groomed.node_assigned na          on e.mrn_epic_durable = na.mrn_epic_durable
  inner join {project_schema}.pt_pool pp                 on na.mrn_mpi = pp.mrn_mpi
  left  join cdw_epic.caboodle.billing_account ba         on e.encounter_key = ba.primary_encounter_key
WHERE
    cast(e.encounter_start_date as date) between '2023-06-03' and @date_stop
;

-- (N rows affected) HH:MM:SS
