-- ================================================================================================
-- TEMPLATE: encounter-harmonized.sql
-- Source:   UNION of Meditech (< 2023-06-03) and Epic (>= 2023-06-03)
-- Purpose:  Single encounter table spanning the full study window across both systems.
--           Columns are harmonized to a common shape; system-specific fields nulled where absent.
--
-- !! REQUIRES encounter-meditech and encounter-epic tables to already be populated, OR
--    run this as a standalone CREATE + INSERT with both source systems inline (see below).
-- !! CUSTOMIZE {project_schema}, date params, campus/department filters
-- !! REVIEW epic_encounter_types if the study needs admin, documentation-only, messaging, or ED encounters.
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start       date          = '{date_start}';   -- overall study start
DECLARE @date_stop        date          = '{date_stop}';   -- overall study stop (may span both systems)
DECLARE @date_stop_legacy date          = '2023-06-02';
DECLARE @date_start_epic  date          = '2023-06-03';

DROP TABLE if exists {project_schema}.encounter;
--exec dbo.generate_create_table_sp '{project_schema}.encounter'
CREATE TABLE {project_schema}.encounter (
  encounter_index       int          identity primary key,
  source_system         varchar(10)  not null,               -- 'meditech' | 'epic'
  -- Universal encounter key: account_number for meditech, encounter_key for epic
  account_number        char(12),                            -- Meditech only
  encounter_key         int,                                 -- Epic only
  mrn_mpi               int          not null,
  -- Encounter
  encounter_start_date  date         not null,
  encounter_end_date    date,
  length_of_stay        smallint,
  patient_class         varchar(50),                         -- 'inpatient' | 'outpatient' | 'emergency'
  facility              varchar(100),
  department_or_campus  varchar(100),
  discharge_disposition varchar(100),
  insurance_category    varchar(100),
);

WITH epic_encounter_types as (
  SELECT 'Recurring Outpatient' as encounter_type union all
  SELECT 'Telemedicine' union all
  SELECT 'Surgery' union all
  SELECT 'Intake' union all
  SELECT 'Lab' union all
  SELECT 'Evaluation' union all
  SELECT 'Prep for Procedure' union all
  SELECT 'Postpartum Visit' union all
  SELECT 'Inpatient' union all
  SELECT 'Pre-Admission Testing' union all
  SELECT 'Appointment' union all
  SELECT 'Ancillary Procedure' union all
  SELECT 'Clinical Outpatient' union all
  SELECT 'Ophth Exam' union all
  SELECT 'Follow-Up' union all
  SELECT 'Education' union all
  SELECT 'Hospital' union all
  SELECT 'Routine Prenatal' union all
  SELECT 'Surgical Day Care Patient' union all
  SELECT 'Nutrition' union all
  SELECT 'Clinical Support' union all
  SELECT 'Treatment' union all
  SELECT 'Procedure Visit' union all
  SELECT 'Observation Patient' union all
  SELECT 'Initial Prenatal' union all
  SELECT 'Consult' union all
  SELECT 'Social Work' union all
  SELECT 'Hospital Encounter' union all
  SELECT 'Referred Outpatient' union all
  SELECT 'Specialty Pharmacy' union all
  SELECT 'Medication Management' union all
  SELECT 'Pharmacy Visit' union all
  SELECT 'Office Visit'
)

-- ---- Meditech arm (service dates < 2023-06-03) -----------------------------------------------
INSERT {project_schema}.encounter
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
  v.visit_start_date between @date_start and @date_stop_legacy

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
  inner join cdw_mpi_1.groomed.node_assigned na         on e.mrn_epic_durable = na.mrn_epic_durable
  inner join {project_schema}.pt_pool pp                on na.mrn_mpi = pp.mrn_mpi
  inner join epic_encounter_types eet                   on e.encounter_type = eet.encounter_type
  left  join cdw_epic.caboodle.billing_account ba       on e.encounter_key = ba.primary_encounter_key
WHERE
  cast(e.encounter_start_date as date) between @date_start_epic and @date_stop
;

-- (N rows affected) HH:MM:SS
