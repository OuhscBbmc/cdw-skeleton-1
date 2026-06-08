-- ================================================================================================
-- TEMPLATE: lab-meditech.sql
-- Source:   cdw_meditech.meditech.lab
-- Applies:  collection dates < 2023-06-03  (Meditech)
-- Purpose:  Lab results from Meditech
--
-- !! CUSTOMIZE {project_schema}, @date_start
-- !! CUSTOMIZE ss_lab join - join to your study-specific lookup to filter to tests of interest
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start   date = '{date_start}';
DECLARE @date_stop_legacy date = '2023-06-02';

drop table if exists {project_schema}.lab_meditech;
--exec dbo.generate_create_table_sp '{project_schema}.lab_meditech'
create table {project_schema}.lab_meditech (
  lab_meditech_index      int             identity primary key,
  account_number          char(12)        not null,
  mrn_mpi                 int             not null,
  mrn_meditech_internal   varchar(10)     not null,
  mnemonic                varchar(15)     not null,
  loinc                   varchar(7),
  collection_datetime     smalldatetime   not null,
  result_value            varchar(75),
  result_numeric          float,
  unit                    varchar(10),
  normal_range            varchar(25),
  status                  varchar(5),   -- lab status code; see lexis.dim_meditech_lab_status
  -- Study classification:
  lab_category            varchar(100),
);

insert {project_schema}.lab_meditech
SELECT
  l.account_number
  ,na.mrn_mpi
  ,l.mrn_meditech_internal
  ,l.mnemonic
  ,l.loinc
  ,l.collection_datetime
  ,l.result                                    as result_value
  ,try_convert(float, l.result)               as result_numeric
  ,l.units                                    as unit
  ,l.normal_range
  ,l.status
  ,ss.lab_category
FROM cdw_meditech.meditech.lab l
  inner join cdw_mpi_1.groomed.node_assigned na   on l.mrn_meditech_internal = na.mrn_meditech_internal
  inner join {project_schema}.pt_pool pp           on na.mrn_mpi = pp.mrn_mpi
  -- !! Join to study-specific lookup to filter to tests of interest:
  inner join {project_schema}.ss_lab ss            on l.mnemonic = ss.mnemonic
WHERE l.collection_datetime between @date_start and @date_stop_legacy
ORDER BY na.mrn_mpi, l.collection_datetime;

-- (N rows affected) HH:MM:SS
