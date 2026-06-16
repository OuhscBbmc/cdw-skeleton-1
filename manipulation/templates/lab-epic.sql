-- ================================================================================================
-- TEMPLATE: lab-epic.sql
-- Source:   cdw_epic.caboodle.lab_component_result
--           + cdw_epic_waystation.caboodle.lab_component_dim
-- Applies:  result dates >= 2023-06-03  (Epic go-live)
-- Purpose:  Lab component results from Epic
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop
-- !! CUSTOMIZE the ss_lab join or lab_component_key / loinc_code filter
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start_epic date          = '2023-06-03';    -- Epic go-live floor
DECLARE @date_stop       date          = '{date_stop}';

DROP TABLE if exists {project_schema}.lab_epic;
--exec dbo.generate_create_table_sp '{project_schema}.lab_epic'
CREATE TABLE {project_schema}.lab_epic (
  lab_component_result_key int          primary key,
  encounter_key            int          not null,
  mrn_mpi                  int          not null,
  mrn_epic_durable         int          not null,
  lab_component_key        int          not null,
  loinc_code               varchar(30),
  component_name           varchar(150),
  common_name              varchar(500),
  result_datetime          datetime     not null,
  result_value             varchar(200),
  result_numeric           float,
  unit                     varchar(50),
  reference_values         varchar(300),
  flag                     varchar(50),
  result_status            varchar(50),
  -- Study classification:
  lab_category             varchar(100),
);

INSERT {project_schema}.lab_epic
SELECT
  lr.lab_component_result_key
  ,lr.encounter_key
  ,na.mrn_mpi
  ,lr.mrn_epic_durable
  ,lr.lab_component_key
  ,lcd.loinc_code
  ,lcd.name                                        as component_name
  ,lcd.common_name
  ,lr.result_datetime
  ,lr.value                                        as result_value
  ,lr.numeric_value                                as result_numeric
  ,lr.unit
  ,lr.reference_values
  ,lr.flag
  ,lr.result_status
  ,ss.lab_category
FROM cdw_epic.caboodle.lab_component_result lr
  inner join cdw_epic_waystation.caboodle.lab_component_dim lcd   on lr.lab_component_key = lcd.lab_component_key
  inner join cdw_mpi_1.groomed.node_assigned na                   on lr.mrn_epic_durable = na.mrn_epic_durable
  inner join {project_schema}.pt_pool pp                          on na.mrn_mpi = pp.mrn_mpi
  -- !! Join to study-specific lab lookup; remove/swap as needed:
  inner join {project_schema}.ss_lab ss                           on lr.lab_component_key = ss.lab_component_key
WHERE
  lr.result_datetime between @date_start_epic and @date_stop
  and lr.is_final = 1
ORDER BY na.mrn_mpi, lr.result_datetime;

-- (N rows affected) HH:MM:SS
