-- ================================================================================================
-- TEMPLATE: birth-meditech.sql
-- Source:   cdw_outpost.snowflake_2.birth  (unified birth table spanning legacy systems)
-- Applies:  birth_dates < 2023-06-03  (legacy / Meditech)
--           snowflake_2.birth covers births from all pre-Epic source systems.
-- Purpose:  Birth events from legacy systems - fewer obstetric fields than Epic but
--           includes key fields: birth date, gestational age, birth weight, account numbers
--           for both baby and mother.
--
-- !! CUSTOMIZE {project_schema}, @date_start
-- !! Join logic: default joins via mother's mrn_meditech_internal.
--    Swap to mrn_mpi_mother or mrn_mpi_baby depending on your cohort anchor.
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start   date = '{date_start}';
DECLARE @date_stop_legacy date = '2023-06-02';

drop table if exists {project_schema}.birth_meditech;
--exec dbo.generate_create_table_sp '{project_schema}.birth_meditech'
create table {project_schema}.birth_meditech (
  birth_meditech_index                int             identity primary key,
  mrn_mpi_mother                      int             not null,
  mrn_mpi_baby                        int             not null,
  birth_index_within_mother           smallint,
  birth_date                          date,
  mother_age_years                    numeric(18,0),
  gestational_age_weeks_standardized  varchar(25),
  birth_weight_g                      varchar(25),
  account_number_baby                 char(12)        not null,
  account_number_mother               char(12),
  mrn_meditech_internal_mother        varchar(10),
);

insert {project_schema}.birth_meditech
SELECT distinct
  na.mrn_mpi                              as mrn_mpi_mother
  ,b.mrn_mpi_baby
  ,row_number() over (
    partition by na.mrn_mpi
    ORDER BY b.birth_date
    )                                      as birth_index_within_mother
  ,b.birth_date
  ,floor(datediff(day, p.birth_date, b.birth_date) / 365.25) as mother_age_years
  ,b.gestational_age_weeks_standardized
  ,b.birth_weight_g
  ,b.account_number_baby
  ,b.account_number_mother
  ,b.mrn_meditech_internal_mother
FROM cdw_outpost.snowflake_2.birth b
  inner join cdw_mpi_1.groomed.node_assigned na   on b.mrn_meditech_internal_mother = na.mrn_meditech_internal
  inner join {project_schema}.pt_pool pp          on na.mrn_mpi = pp.mrn_mpi
  inner join cdw_outpost.snowflake_2.person p     on na.mrn_mpi = p.mrn_mpi
WHERE b.birth_date between @date_start and @date_stop_legacy
ORDER BY na.mrn_mpi, b.birth_date;

-- (N rows affected) HH:MM:SS
