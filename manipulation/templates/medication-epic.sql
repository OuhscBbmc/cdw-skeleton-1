-- ================================================================================================
-- TEMPLATE: medication-epic.sql
-- Source:   cdw_epic.caboodle.medication_event + cdw_epic_waystation.caboodle.medication_dim
-- Applies:  start dates >= 2023-06-03  (Epic go-live)
-- Purpose:  Medication events (administrations + orders) from Epic Caboodle
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop
-- !! CUSTOMIZE medication filter: join to ss_med OR filter by therapeutic_class / pharmaceutical_class
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start_epic date = '2023-06-03';   -- Epic go-live floor
DECLARE @date_stop       date = '{date_stop}';

DROP TABLE if exists {project_schema}.medication_epic;
--exec dbo.generate_create_table_sp '{project_schema}.medication_epic'
CREATE TABLE {project_schema}.medication_epic (
  med_epic_index          int           identity primary key,
  mrn_mpi                 int           not null,
  mrn_epic_durable        int           not null,
  medication_event_key    int           not null,
  medication_key          int           not null,
  index_within_patient    bigint,
  name                    varchar(300),
  simple_generic_name     varchar(300),
  pharmaceutical_class    varchar(300),
  pharmaceutical_subclass varchar(300),
  therapeutic_class       varchar(300),
  start_datetime          datetime2     not null,
  administered            int           not null,
  route                   varchar(300),
  minimum_dose            numeric(18,2),
  maximum_dose            numeric(18,2),
  dose_unit               varchar(300),
  -- Study classification:
  med_category            varchar(100),
);

INSERT {project_schema}.medication_epic
SELECT
  na.mrn_mpi
  ,na.mrn_epic_durable
  ,m.medication_event_key
  ,m.medication_key
  ,row_number() over (
    partition by na.mrn_mpi
    order by m.start_date, md.name, m.medication_event_key
    )                                          as index_within_patient
  ,md.name
  ,md.simple_generic_name
  ,md.pharmaceutical_class
  ,md.pharmaceutical_subclass
  ,md.therapeutic_class
  ,m.start_datetime
  ,m.administered
  ,m.route
  ,m.minimum_dose
  ,m.maximum_dose
  ,m.dose_unit
  ,ss.med_category
FROM cdw_epic.caboodle.medication_event m
  inner join cdw_epic_waystation.caboodle.medication_dim md   on m.medication_key = md.medication_key
  inner join cdw_mpi_1.groomed.node_assigned na               on m.mrn_epic_durable = na.mrn_epic_durable
  inner join {project_schema}.pt_pool pp                      on na.mrn_mpi = pp.mrn_mpi
  -- !! Join to your study-specific medication lookup; remove/swap filter as needed:
  inner join {project_schema}.ss_med ss                       on m.medication_key = ss.medication_key
  -- Alternative filter options (comment out ss_med join and use one of these):
  -- where md.therapeutic_class in ('ANTICOAGULANTS', 'ANTIDIABETICS')
  -- where md.pharmaceutical_class in (SELECT [value] FROM string_split(@classes, ';'))
WHERE
  m.start_date between @date_start_epic and @date_stop
ORDER BY na.mrn_mpi, m.start_date;

-- (N rows affected) HH:MM:SS
