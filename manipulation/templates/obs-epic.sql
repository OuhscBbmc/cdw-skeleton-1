-- ================================================================================================
-- TEMPLATE: obs-epic.sql
-- Source:   cdw_epic.caboodle.flowsheet + cdw_epic_waystation.caboodle.flowsheet_row_dim
-- Applies:  Epic flowsheet observations (vitals, nursing assessments, intake/output, scores)
--           service dates >= 2023-06-03
-- Purpose:  Structured observation values recorded in Epic flowsheets.
--           Each row is one value for one flowsheet row key for one encounter.
--           Uses a temp table for the MPI join to avoid repeated lookups at scale.
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop
-- !! CUSTOMIZE the ss_obs_epic join - build this lookup by running DISTINCT on
--    flowsheet_row_key + fd.name for your observations of interest
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start_epic date          = '2023-06-03';
DECLARE @date_stop       date          = '{date_stop}';

DROP TABLE if exists {project_schema}.obs_epic;
--exec dbo.generate_create_table_sp '{project_schema}.obs_epic'
CREATE TABLE {project_schema}.obs_epic (
  obs_epic_index             int           identity primary key,
  mrn_mpi                    int           not null,
  mrn_epic_durable           int           not null,
  encounter_key              int           not null,
  flowsheet_row_key          int           not null,
  name                       varchar(250)  not null,
  display_name               varchar(300)  not null,
  value_numeric              numeric(18,2),
  value_string               varchar(2500),
  value_date                 date,
  value_time                 time,
  obs_index_within_patient   bigint,
  obs_index_within_encounter bigint,
  -- Study classification:
  obs_category               varchar(100),
);

-- Temp table to resolve mrn_epic_durable -> mrn_mpi once, then reuse in the main join
DROP TABLE if exists #pt_epic;
CREATE TABLE #pt_epic (
  mrn_epic_durable int primary key,
  mrn_mpi          int not null,
);

INSERT #pt_epic
SELECT
  na.mrn_epic_durable
  ,pp.mrn_mpi
FROM {project_schema}.pt_pool pp
  inner join cdw_mpi_1.groomed.node_assigned na on
    pp.mrn_mpi = na.mrn_mpi
    and
    na.type = 'epic14';

INSERT {project_schema}.obs_epic
SELECT
  p.mrn_mpi
  ,p.mrn_epic_durable
  ,f.encounter_key
  ,f.flowsheet_row_key
  ,fd.name
  ,fd.display_name
  ,f.value_numeric
  ,f.value_string
  ,f.value_date
  ,f.value_time
  ,row_number() over (
    partition by p.mrn_mpi
    order by f.encounter_key, f.flowsheet_row_key, f.value_date, f.value_time, f.value_string
  )                                            as obs_index_within_patient
  ,row_number() over (
    partition by f.encounter_key
    order by f.flowsheet_row_key, f.value_date, f.value_time, f.value_string
  )                                            as obs_index_within_encounter
  ,ss.obs_category
FROM cdw_epic.caboodle.flowsheet f
  inner join cdw_epic_waystation.caboodle.flowsheet_row_dim fd    on f.flowsheet_row_key = fd.flowsheet_row_key
  inner join {project_schema}.ss_obs_epic ss                      on f.flowsheet_row_key = ss.flowsheet_row_key
  inner join #pt_epic p                                           on f.mrn_epic_durable = p.mrn_epic_durable
-- Optionally filter by date (flowsheet doesn't always have a direct date; filter via encounter):
-- inner join cdw_epic.caboodle.encounter e on f.encounter_key = e.encounter_key
--   and cast(e.encounter_start_date as date) between @date_start_epic and @date_stop
ORDER BY p.mrn_mpi, f.encounter_key;
DROP TABLE if exists #pt_epic;

-- (N rows affected) HH:MM:SS
