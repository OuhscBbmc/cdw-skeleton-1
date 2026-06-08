-- ================================================================================================
-- TEMPLATE: medication-harmonized.sql
-- Source:   UNION of Meditech (< 2023-06-03) and Epic (>= 2023-06-03)
-- Purpose:  Single medication table spanning the full study window across both systems.
--           Columns are harmonized; system-specific fields nulled where absent.
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop, medication filters
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start       date         = '{date_start}';
DECLARE @date_stop        date         = '{date_stop}';
DECLARE @mnemonics        varchar(500) = '{mnemonics}';   -- for Meditech arm; e.g., 'METFORMIN;LISINOPRIL'
DECLARE @date_stop_legacy date         = '2023-06-02';
DECLARE @date_start_epic  date         = '2023-06-03';

DROP TABLE if exists {project_schema}.medication;
--exec dbo.generate_create_table_sp '{project_schema}.medication'
CREATE TABLE {project_schema}.medication (
  med_index            int           identity primary key,
  source_system        varchar(10)   not null,   -- 'meditech' | 'epic'
  -- System-specific encounter/event keys:
  account_number       char(12),   -- Meditech
  medication_event_key int,   -- Epic
  mrn_mpi              int           not null,
  -- Medication
  medication_name      varchar(300),
  simple_generic_name  varchar(300),   -- Epic only; null for Meditech
  pharmaceutical_class varchar(300),   -- Epic only
  therapeutic_class    varchar(300),   -- Epic only
  admin_datetime       datetime2     not null,
  administered         bit,   -- Epic: 1=administered; Meditech: always 1
  route                varchar(300),
  minimum_dose         numeric(18,2),   -- Epic only; no per-admin dose in Caboodle; always null for Meditech
  maximum_dose         numeric(18,2),   -- Epic only
  dose_unit            varchar(300),
  -- Study classification:
  med_category         varchar(100),
);

-- ---- Meditech arm (< 2023-06-03) -------------------------------------------------------------
INSERT {project_schema}.medication
SELECT
  'meditech'
  ,m.account_number
  ,null
  ,na.mrn_mpi
  ,m.medication_name
  ,null                                               as simple_generic_name
  ,null                                               as pharmaceutical_class
  ,null                                               as therapeutic_class
  ,cast(m.admin_datetime as datetime2)
  ,1                                                  as administered
  ,m.route
  ,null                                               as minimum_dose  -- Meditech has no numeric dose column
  ,null                                               as maximum_dose
  ,m.dose_unit
  ,ss.med_category
FROM cdw_meditech.meditech.medication m
  inner join cdw_mpi_1.groomed.node_assigned na   on m.mrn_meditech_internal = na.mrn_meditech_internal
  inner join {project_schema}.pt_pool pp           on na.mrn_mpi = pp.mrn_mpi
  -- !! Use EITHER mnemonic filter OR ss_med join, not both:
  inner join {project_schema}.ss_med ss            on m.medication_name = ss.medication_name_meditech
  -- Mnemonic alternative: replace the above line with:
  -- WHERE m.medication_name in (SELECT [value] FROM string_split(@mnemonics, ';'))
WHERE m.admin_datetime between @date_start and @date_stop_legacy

UNION ALL

-- ---- Epic arm (>= 2023-06-03) ---------------------------------------------------------------
SELECT
  'epic'
  ,null
  ,m.medication_event_key
  ,na.mrn_mpi
  ,md.name
  ,md.simple_generic_name
  ,md.pharmaceutical_class
  ,md.therapeutic_class
  ,m.start_datetime
  ,cast(m.administered as bit)
  ,m.route
  ,m.minimum_dose
  ,m.maximum_dose
  ,m.dose_unit
  ,ss.med_category
FROM cdw_epic.caboodle.medication_event m
  inner join cdw_epic_waystation.caboodle.medication_dim md   on m.medication_key = md.medication_key
  inner join cdw_mpi_1.groomed.node_assigned na               on m.mrn_epic_durable = na.mrn_epic_durable
  inner join {project_schema}.pt_pool pp                      on na.mrn_mpi = pp.mrn_mpi
  inner join {project_schema}.ss_med ss                       on m.medication_key = ss.medication_key
WHERE m.start_date between @date_start_epic and @date_stop
;

-- (N rows affected) HH:MM:SS
