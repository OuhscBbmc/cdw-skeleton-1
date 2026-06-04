-- ================================================================================================
-- TEMPLATE: encounter-meditech.sql
-- Source:   cdw_meditech.meditech.visit
-- Applies:  service dates < 2023-06-03  (legacy Meditech system)
-- Purpose:  Inpatient / outpatient / ED encounters from Meditech
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop, @campus, @patient_status
-- ================================================================================================

use cdw_cache_staging;

declare @date_start      date           = '{date_start}';    -- e.g., '2015-01-01'
declare @date_stop       date           = '2023-06-02';      -- hard ceiling for Meditech; adjust earlier if needed
declare @campus          varchar(500)   = '{campus}';        -- semicolon-delimited; e.g., 'the children''s hospital;ou medical center (adult)'
declare @patient_status  varchar(50)    = 'inpatient';       -- 'inpatient' | 'outpatient' | 'emergency' | remove filter for all


DROP TABLE IF EXISTS [cdw_cache_staging].[{project_schema}].[encounter_meditech];
CREATE TABLE [cdw_cache_staging].[{project_schema}].[encounter_meditech] (
    account_number              char(12)        not null primary key,
    mrn_mpi                     int             not null,
    mrn_meditech_internal       varchar(10)     not null,
    -- Encounter details
    facility                    varchar(50),
    campus                      varchar(50),
    visit_start_date            date            not null,
    visit_stop_date             date,
    length_of_stay              smallint,
    patient_status              varchar(50),
    location_first_description  varchar(50),
    location_last_description   varchar(50),
    location_last_room          varchar(50),
    provider_discharge          varchar(50),
    admit_source                varchar(50),
    discharge_disposition       varchar(50),
);

INSERT INTO {project_schema}.encounter_meditech
SELECT
    v.account_number,na.mrn_mpi,v.mrn_meditech_internal,v.facility,v.campus,v.visit_start_date,v.visit_stop_date,v.length_of_stay,v.patient_status,v.location_first_description,v.location_last_description,v.location_last_room,v.provider_discharge,v.admit_source,v.discharge_disposition
FROM cdw_meditech.meditech.visit v
  inner join cdw_mpi_1.groomed.node_assigned na          on v.mrn_meditech_internal = na.mrn_meditech_internal
  inner join {project_schema}.pt_pool pp                 on na.mrn_mpi = pp.mrn_mpi
WHERE
    v.visit_start_date between @date_start and @date_stop
    and v.campus in (SELECT [value] FROM string_split(@campus, ';'))
    and v.patient_status = @patient_status   -- remove this line for all visit types
ORDER BY na.mrn_mpi, v.visit_start_date;

-- (N rows affected) HH:MM:SS
