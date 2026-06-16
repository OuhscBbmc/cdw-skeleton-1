-- ================================================================================================
-- TEMPLATE: medication-meditech.sql
-- Source:   cdw_meditech.meditech.medication
-- Applies:  admin dates < 2023-06-03  (Meditech)
-- Purpose:  Medication administrations and orders from Meditech
--
-- !! CUSTOMIZE {project_schema}, @date_start, @mnemonics
-- !! CUSTOMIZE the WHERE filter: join to {project_schema}.ss_med OR filter by @mnemonics string
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start       date          = '{date_start}';
DECLARE @date_stop_legacy date          = '2023-06-02';
-- Option A - semicolon-delimited mnemonic list (simple name filter):
DECLARE @mnemonics        varchar(500)  = '{mnemonics}';    -- e.g., 'METFORMIN;LISINOPRIL;ATORVASTATIN'
-- Option B - use a study-specific medication lookup table (ss_med) for concept-based filtering;
--            comment out @mnemonics filter and uncomment ss_med join below.
DROP TABLE if exists {project_schema}.medication_meditech;
--exec dbo.generate_create_table_sp '{project_schema}.medication_meditech'
CREATE TABLE {project_schema}.medication_meditech (
  med_meditech_index    int           identity primary key,
  account_number        char(12)      not null,
  mrn_mpi               int           not null,
  mrn_meditech_internal varchar(10)   not null,
  source_meditech       varchar(10)   not null,
  admin_datetime        smalldatetime not null,
  medication_name       varchar(100),
  dose_unit             varchar(50),
  route                 varchar(50),
  sig                   varchar(50),
  -- Study classification (from ss_med lookup):
  med_category          varchar(100),
);

INSERT {project_schema}.medication_meditech
SELECT
  m.account_number,na.mrn_mpi,m.mrn_meditech_internal,m.source_meditech,m.admin_datetime,m.medication_name,m.dose_unit,m.route,m.sig,ss.med_category
FROM cdw_meditech.meditech.medication m
  inner join cdw_mpi_1.groomed.node_assigned na   on m.mrn_meditech_internal = na.mrn_meditech_internal
  inner join {project_schema}.pt_pool pp           on na.mrn_mpi = pp.mrn_mpi
-- Option A - filter by mnemonic name list:
WHERE
  m.admin_datetime between @date_start and @date_stop_legacy
  and m.medication_name in (SELECT [value] FROM string_split(@mnemonics, ';'))
-- Option B - join to study-specific lookup (comment out Option A and uncomment below):
-- inner join {project_schema}.ss_med ss on m.medication_concept_id = ss.concept_id
-- WHERE m.admin_datetime between @date_start and @date_stop
ORDER BY na.mrn_mpi, m.admin_datetime;

-- (N rows affected) HH:MM:SS
