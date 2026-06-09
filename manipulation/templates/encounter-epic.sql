-- ================================================================================================
-- TEMPLATE: encounter-epic.sql
-- Source:   cdw_epic.caboodle.encounter
-- Applies:  service dates >= 2023-06-03  (Epic go-live)
-- Purpose:  Inpatient / outpatient encounters from Epic Caboodle
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop, @department_name, @encounter_type
-- !! REVIEW epic_encounter_types if the study needs admin, documentation-only, messaging, or ED encounters.
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start_epic date          = '2023-06-03';   -- Epic go-live floor; adjust forward if needed
DECLARE @date_stop       date          = '{date_stop}';   -- e.g., '2025-12-31'
-- Optionally narrow to specific departments or encounter types (semicolon-delimited):
DECLARE @department_name varchar(500)   = null;            -- e.g., 'OU CHILDREN''S PHYSICIANS;SOONER PEDIATRICS'
DECLARE @encounter_type  varchar(500)   = null;            -- additional filter inside the default visit-type list

DROP TABLE if exists {project_schema}.encounter_epic;
--exec dbo.generate_create_table_sp '{project_schema}.encounter_epic'
CREATE TABLE {project_schema}.encounter_epic (
  encounter_key               int          primary key,
  mrn_mpi                     int          not null,
  mrn_epic_durable            int          not null,
  encounter_epic_csn          bigint,
  -- Encounter details
  encounter_start_date        date         not null,
  encounter_end_date          date,
  length_of_stay              smallint,
  encounter_type              varchar(300),
  patient_class               varchar(50),                -- 'Inpatient' | 'Outpatient' | 'Emergency'
  department_name             varchar(100),
  facility_name               varchar(100),
  visit_provider_name         varchar(200),
  discharge_disposition       varchar(300),
  primary_benefit_payor_class varchar(100),
);

WITH insurance as (
  SELECT
    ba.primary_encounter_key
    ,ba.primary_benefit_payor_class
  FROM cdw_epic.caboodle.billing_account ba
),
epic_encounter_types as (
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
INSERT {project_schema}.encounter_epic
SELECT
  e.encounter_key
  ,na.mrn_mpi
  ,e.mrn_epic_durable
  ,e.encounter_epic_csn
  ,cast(e.encounter_start_date as date)           as encounter_start_date
  ,cast(e.encounter_end_date   as date)           as encounter_end_date
  ,datediff(day, e.encounter_start_date, e.encounter_end_date) as length_of_stay
  ,e.encounter_type
  ,null                                           as patient_class  -- !! join caboodle.hospital_admission on encounter_key for inpatient class
  ,e.department_name
  ,e.location_name                                as facility_name
  ,null                                           as visit_provider_name  -- !! join dim_provider via attending_provider_durable_key for name
  ,e.discharge_disposition
  ,i.primary_benefit_payor_class
FROM cdw_epic.caboodle.encounter e
  inner join cdw_mpi_1.groomed.node_assigned na   on e.mrn_epic_durable = na.mrn_epic_durable
  inner join {project_schema}.pt_pool pp          on na.mrn_mpi = pp.mrn_mpi
  inner join epic_encounter_types eet             on e.encounter_type = eet.encounter_type
  left  join insurance i                          on e.encounter_key = i.primary_encounter_key
WHERE
  cast(e.encounter_start_date as date) between @date_start_epic and @date_stop
  -- Uncomment to filter by department:
  -- and (@department_name is null or e.department_name in (SELECT [value] FROM string_split(@department_name, ';')))
  -- Uncomment to further narrow encounter type within the default visit-type list:
  -- and (@encounter_type is null or e.encounter_type in (SELECT [value] FROM string_split(@encounter_type, ';')))
ORDER BY na.mrn_mpi, e.encounter_start_date;

-- (N rows affected) HH:MM:SS
