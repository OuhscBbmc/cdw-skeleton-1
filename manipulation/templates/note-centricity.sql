-- ================================================================================================
-- TEMPLATE: note-centricity.sql
-- Source:   cdw_centricity.centricity.document + document_note + usr
-- Applies:  clinical_dates < 2023-06-03  (Centricity — outpatient/ambulatory)
-- Purpose:  Free-text clinical notes from Centricity (office visits, op reports, etc.)
--           Uses a two-step pattern: first resolve eligible documents into a temp table,
--           then pull full note text only for those — avoids loading all note text upfront.
--
-- Common doctype_description values:
--   'Office Visit', 'Telemedicine Encounter', 'Progress Notes', 'Operative Report',
--   'Office Procedure', 'Visit Summary', 'Nurse Only Visit', 'Phone Note'
--
-- !! CUSTOMIZE {project_schema}, @date_start, @location_name, @doctype_description list
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start   date           = '{date_start}';
DECLARE @date_stop    date           = '2023-06-02';
DECLARE @location     varchar(500)   = '{location_name}';   -- e.g., 'OU Cancer Institute;Stephenson Cancer Center'
-- Set @location = null to pull all locations


DROP TABLE IF EXISTS [cdw_cache_staging].[{project_schema}].[note_centricity];
CREATE TABLE [cdw_cache_staging].[{project_schema}].[note_centricity] (
    sdid                    bigint          not null primary key,   -- Centricity document ID
    mrn_centricity          bigint          not null,
    mrn_mpi                 int             null,
    clinical_date           date            not null,
    location_name           varchar(70)     null,
    doctype_description     varchar(32)     null,
    summary                 varchar(64)     null,
    note_text               varchar(max)    not null,
    physician_name_full     varchar(50)     null,
    physician_specialty     varchar(32)     null,
);

-- Step 1: resolve eligible document IDs (avoids full note text scan)
DROP TABLE IF EXISTS #docs;
CREATE TABLE #docs (sdid bigint primary key);

INSERT INTO #docs
SELECT d.sdid
FROM cdw_centricity.centricity.document d
inner join cdw_mpi_1.groomed.node_assigned na   on d.mrn_centricity = na.mrn_centricity
inner join {project_schema}.pt_pool pp          on na.mrn_mpi = pp.mrn_mpi
WHERE
    d.clinical_date between @date_start and @date_stop
    and (@location is null or d.location_name in (SELECT [value] FROM string_split(@location, ';')))
    and d.doctype != 3;   -- exclude phone notes; remove if you want all types

-- Step 2: pull note text only for resolved documents
INSERT INTO {project_schema}.note_centricity
SELECT
    d.sdid
    ,d.mrn_centricity
    ,na.mrn_mpi
    ,d.clinical_date
    ,d.location_name
    ,d.doctype_description
    ,d.summary
    ,n.note                                     as note_text
    ,u.name_full                                as physician_name_full
    ,u.specialty                                as physician_specialty
FROM cdw_centricity.centricity.document d
inner join cdw_centricity.centricity.document_note n    on d.sdid  = n.sdid
inner join cdw_centricity.centricity.usr u              on d.usrid = u.pvid
left  join cdw_mpi_1.groomed.node_assigned na           on d.mrn_centricity = na.mrn_centricity
WHERE
    d.sdid in (SELECT td.sdid FROM #docs td)
    -- !! Filter to desired note types; remove for all types:
    and d.doctype_description in (
        'Office Visit'
        ,'Telemedicine Encounter'
        -- ,'Progress Notes'
        -- ,'Operative Report'
        -- ,'Office Procedure'
    );

DROP TABLE IF EXISTS #docs;

-- (N rows affected) HH:MM:SS
