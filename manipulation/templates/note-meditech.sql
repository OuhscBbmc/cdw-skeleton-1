-- ================================================================================================
-- TEMPLATE: note-meditech.sql
-- Source:   cdw_meditech.meditech.report  (clinical notes/reports from Meditech)
-- Applies:  entered_datetime < 2023-06-03  (Meditech)
-- Purpose:  Free-text clinical notes from Meditech (discharge summaries, H&Ps, progress notes, etc.)
--           Filter by report_name pattern to target specific note types.
--
--           For keyword-based NLP extraction from note text, see stash/report-note-keyword-extraction.sql.
--           For Epic notes, see the saleh-velez pattern in note-epic.sql.
--
-- !! CUSTOMIZE {project_schema}, @date_start, @report_name_pattern
-- ================================================================================================

use cdw_cache_staging;

declare @date_start           date           = '{date_start}';
declare @date_stop            date           = '2023-06-02';
declare @report_name_pattern  varchar(100)   = '%{report_name_pattern}%';
-- Common patterns: '%discharge%', '%pallia%', '%history%physical%', '%operative%', '%progress%'


DROP TABLE IF EXISTS [cdw_cache_staging].[{project_schema}].[note_meditech];
CREATE TABLE [cdw_cache_staging].[{project_schema}].[note_meditech] (
    note_meditech_index     int             identity(1,1) primary key,
    report_urn              varchar(8)      not null unique,
    source_meditech         varchar(15)     not null,
    account_number          varchar(12)     not null,
    mrn_mpi                 int             not null,
    mrn_meditech_internal   varchar(8)      not null,
    entered_datetime        smalldatetime   not null,
    report_name             varchar(100)    null,
    note_text               varchar(max)    null,
);

INSERT INTO {project_schema}.note_meditech
SELECT
    r.report_urn
    ,r.source_meditech
    ,r.account_number
    ,na.mrn_mpi
    ,r.mrn_meditech_internal
    ,r.entered_datetime
    ,r.report_name
    ,r.note
FROM cdw_meditech.meditech.report r
inner join cdw_mpi_1.groomed.node_assigned na   on r.mrn_meditech_internal = na.mrn_meditech_internal
inner join {project_schema}.pt_pool pp          on na.mrn_mpi = pp.mrn_mpi
WHERE
    r.entered_datetime between @date_start and @date_stop
    and r.report_name like @report_name_pattern
ORDER BY na.mrn_mpi, r.entered_datetime;

-- (N rows affected) HH:MM:SS
