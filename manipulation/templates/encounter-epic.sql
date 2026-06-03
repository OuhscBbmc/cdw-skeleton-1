-- ================================================================================================
-- TEMPLATE: encounter-epic.sql
-- Source:   cdw_epic.caboodle.encounter
-- Applies:  service dates >= 2023-06-03  (Epic go-live)
-- Purpose:  Inpatient / outpatient / ED encounters from Epic Caboodle
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop, @department_name, @encounter_type
-- ================================================================================================

use cdw_cache_staging;

declare @date_start       date           = '2023-06-03';     -- Epic go-live floor; adjust forward if needed
declare @date_stop        date           = '{date_stop}';    -- e.g., '2025-12-31'
-- Optionally filter to specific departments or encounter types (semicolon-delimited):
declare @department_name  varchar(500)   = null;             -- e.g., 'OU CHILDREN''S PHYSICIANS;SOONER PEDIATRICS'
declare @encounter_type   varchar(500)   = null;             -- e.g., 'Office Visit;Telemedicine'


DROP TABLE IF EXISTS [cdw_cache_staging].[{project_schema}].[encounter_epic];
CREATE TABLE [cdw_cache_staging].[{project_schema}].[encounter_epic] (
    encounter_key               int             not null primary key,
    mrn_mpi                     int             not null,
    mrn_epic_durable            int             not null,
    encounter_epic_csn          bigint          null,
    -- Encounter details
    encounter_start_date        date            not null,
    encounter_end_date          date            null,
    length_of_stay              smallint        null,
    encounter_type              varchar(300)    null,
    patient_class               varchar(50)     null,         -- 'Inpatient' | 'Outpatient' | 'Emergency'
    department_name             varchar(100)    null,
    facility_name               varchar(100)    null,
    visit_provider_name         varchar(200)    null,
    discharge_disposition       varchar(300)    null,
    primary_benefit_payor_class varchar(100)    null,
);

WITH insurance AS (
    SELECT
        ba.primary_encounter_key
        ,ba.primary_benefit_payor_class
    FROM cdw_epic.caboodle.billing_account ba
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
  inner join cdw_mpi_1.groomed.node_assigned na    on e.mrn_epic_durable = na.mrn_epic_durable
  inner join {project_schema}.pt_pool pp           on na.mrn_mpi = pp.mrn_mpi
  left  join insurance i                           on e.encounter_key = i.primary_encounter_key
WHERE
    cast(e.encounter_start_date as date) between @date_start and @date_stop
    -- Uncomment to filter by department:
    -- and (@department_name is null or e.department_name in (SELECT [value] FROM string_split(@department_name, ';')))
    -- Uncomment to filter by encounter type:
    -- and (@encounter_type is null or e.encounter_type in (SELECT [value] FROM string_split(@encounter_type, ';')))
ORDER BY na.mrn_mpi, e.encounter_start_date;

-- (N rows affected) HH:MM:SS
