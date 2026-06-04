-- ================================================================================================
-- TEMPLATE: note-epic.sql
-- Source:   cdw_epic_waystation.clarity.note_clinical_summary
--           cdw_epic_waystation.clarity.note_clinical_text_line
--           cdw_epic.caboodle.encounter
-- Applies:  Notes created >= 2023-06-03  (Epic)
-- Purpose:  Free-text clinical notes from Epic Clarity.
--           Each row is one line of one note — join on note_clinical_key to reassemble.
--           Filter by note_type to target specific note categories.
--
-- Common note_type values:
--   'Discharge Summary', 'H&P', 'Interdisciplinary', 'Op Note', 'Post-Procedure Note',
--   'Progress Notes', 'Significant Event', 'Transfer of Care', 'Consult'
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop, @note_types
-- !! CUSTOMIZE the visit_pool join — default uses encounter_key from an existing encounter table
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start   date           = '2023-06-03';
DECLARE @date_stop    date           = '{date_stop}';
DECLARE @note_types   varchar(500)   = 'Discharge Summary;H&P;Op Note;Progress Notes;Consult';


DROP TABLE IF EXISTS [cdw_cache_staging].[{project_schema}].[note_epic];
CREATE TABLE [cdw_cache_staging].[{project_schema}].[note_epic] (
    note_epic_index         int             identity(1,1) primary key,
    note_clinical_key       int             not null,
    encounter_key           int             not null,
    mrn_mpi                 int             not null,
    mrn_epic_durable        int             not null,
    creation_instant        datetime,
    note_type               varchar(80)     not null,
    note_service            varchar(80),
    note_line               smallint        not null,
    note_text               varchar(2000)   not null,
);

INSERT INTO {project_schema}.note_epic
SELECT
    ns.note_clinical_key,e.encounter_key,na.mrn_mpi,e.mrn_epic_durable,ns.creation_instant,ns.note_type,ns.note_service,nt.note_line,nt.text                                    as note_text
FROM cdw_epic_waystation.clarity.note_clinical_summary ns
inner join cdw_epic_waystation.clarity.note_clinical_text_line nt  on ns.note_clinical_key = nt.note_clinical_key
inner join cdw_epic.caboodle.encounter e                           on ns.encounter_epic_csn = e.encounter_epic_csn
inner join cdw_mpi_1.groomed.node_assigned na                      on e.mrn_epic_durable = na.mrn_epic_durable
inner join {project_schema}.pt_pool pp                             on na.mrn_mpi = pp.mrn_mpi
WHERE
    ns.creation_instant between @date_start and @date_stop
    and ns.note_type in (SELECT [value] FROM string_split(@note_types, ';'))
ORDER BY ns.note_clinical_key, nt.note_line;

-- (N rows affected) HH:MM:SS
