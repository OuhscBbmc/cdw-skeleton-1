-- ================================================================================================
-- TEMPLATE: image-epic.sql
-- Source:   cdw_epic_waystation.caboodle.imaging_fact + cdw_epic.caboodle.procedure_event
-- Applies:  exam dates >= 2023-06-03  (Epic)
-- Purpose:  Radiology/imaging studies from Epic.
--           NOTE: exam_start_date_key is an integer date key (YYYYMMDD format), not a date type.
--                 Use the CONVERT pattern below to decode it.
--           Filter by procedure_durable_key (via procedure_event) to target specific imaging types.
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop, @procedure_keys
-- !! BUILD @procedure_keys by running DISTINCT on procedure_durable_key + procedure_name
--    filtered by cpt_code for your modality (e.g., MRI CPTs: 70553, 72148, 73721, etc.)
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start_epic date          = '2023-06-03';
DECLARE @date_stop       date          = '{date_stop}';
-- Semicolon-delimited procedure_durable_key list (or swap for cpt_code filter):
DECLARE @procedure_keys  varchar(2000) = '{procedure_keys}';

DROP TABLE if exists {project_schema}.image_epic;
--exec dbo.generate_create_table_sp '{project_schema}.image_epic'
CREATE TABLE {project_schema}.image_epic (
  image_epic_index    int          identity primary key,
  imaging_key         int          not null,
  mrn_mpi             int          not null,
  mrn_epic_durable    int          not null,
  procedure_name      varchar(300),
  exam_date           date,                                -- decoded FROM exam_start_date_key
  exam_start_date_key int          not null,               -- raw integer key; keep for diagnostics
  study_status        varchar(30),
  is_abnormal         bit,
);

INSERT {project_schema}.image_epic
SELECT
  i.imaging_key
  ,na.mrn_mpi
  ,i.mrn_epic_durable
  ,pe.procedure_name
  -- Decode integer date key (format YYYYMMDD); negative = missing/unknown:
  ,case
    when i.exam_start_date_key < 0 then null
    else convert(date, convert(varchar(8), i.exam_start_date_key, 112))
    end                                            as exam_date
  ,i.exam_start_date_key
  ,i.study_status
  ,i.is_abnormal
FROM cdw_epic_waystation.caboodle.imaging_fact i
  inner join cdw_epic.caboodle.procedure_event pe     on i.mrn_epic_durable = pe.mrn_epic_durable
  inner join cdw_mpi_1.groomed.node_assigned na       on i.mrn_epic_durable = na.mrn_epic_durable
  inner join {project_schema}.pt_pool pp              on na.mrn_mpi = pp.mrn_mpi
WHERE
  pe.procedure_durable_key in (SELECT [value] FROM string_split(@procedure_keys, ';'))
  -- Swap for CPT-based filter:
  -- pe.cpt_code in (SELECT [value] FROM string_split(@cpts, ';'))
  and case
      when i.exam_start_date_key < 0 then null
      else convert(date, convert(varchar(8), i.exam_start_date_key, 112))
    end between @date_start_epic and @date_stop
ORDER BY na.mrn_mpi, exam_date;

-- (N rows affected) HH:MM:SS
