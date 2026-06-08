-- ================================================================================================
-- TEMPLATE: patient.sql
-- Source:   cdw_outpost.snowflake_2 (unified across all time periods; no date-based routing needed)
-- Purpose:  Build pt_pool (inclusion criteria) and patient (demographics) tables
--
-- !! CUSTOMIZE before running -- find-replace {project_schema} with your schema (e.g., benbrook_diabetes_1)
-- !! CUSTOMIZE @date_start, @date_stop, @age_min, @age_max to match your IRB
-- !! CUSTOMIZE the WHERE clause in pt_pool to match your inclusion criteria
-- !! CUSTOMIZE columns in CREATE TABLE / SELECT to match what your project needs
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start date = '{date_start}';   -- e.g., '2019-01-01'
DECLARE @date_stop  date = '{date_stop}';   -- e.g., '2024-12-31'
DECLARE @age_min    int  = 18;   -- inclusive lower bound (years)
DECLARE @age_max    int  = 89;   -- inclusive upper bound (years); remove if not needed

-- ------------------------------------------------------------------------------------------------
-- Step 1: pt_pool  -- one row per eligible patient; holds mrn_mpi for downstream joins
-- ------------------------------------------------------------------------------------------------
DROP TABLE if exists {project_schema}.pt_pool;
--exec dbo.generate_create_table_sp '{project_schema}.pt_pool'
CREATE TABLE {project_schema}.pt_pool (
  mrn_mpi  int primary key,
  pt_index int not null unique,   -- stable surrogate key used across all project tables
);

-- ------------------------------------------------------------------------------------------------
-- Step 2: patient  -- one row per patient; demographics + cross-system MRNs
-- ------------------------------------------------------------------------------------------------
DROP TABLE if exists {project_schema}.patient;
--exec dbo.generate_create_table_sp '{project_schema}.patient'
CREATE TABLE {project_schema}.patient (
  pt_index               int          primary key,
  mrn_mpi                int          not null unique,
  birth_date             date,
  birth_year             smallint,
  death_date             date,
  age_years              smallint,
  gender_male            bit,
  race                   varchar(50),
  ethnicity              varchar(50),
  zipcode                varchar(10),
  -- Cross-system MRN externals (pipe-delimited)
  mrn_meditech_externals varchar(150),
  mrn_gecbs              varchar(120),
  mrn_epic_externals     varchar(150),
);

INSERT {project_schema}.pt_pool
SELECT
  p.mrn_mpi
  ,cast(row_number() over (order by p.mrn_mpi) as int) as pt_index
FROM cdw_outpost.snowflake_2.person p
-- !! Add inclusion joins here, e.g.:
-- inner join {project_schema}.ss_dx sd on ...   (diagnosis-based inclusion)
-- inner join cdw_outpost.snowflake_2.problem pr on p.mrn_mpi = pr.mrn_mpi and pr.problem_concept_id in (...)
WHERE
  @age_min <= p.age_years
  and p.age_years <= @age_max
  -- and p.death_date is null   -- uncomment to exclude deceased patients
;
INSERT {project_schema}.patient
SELECT
  pp.pt_index
  ,p.mrn_mpi
  ,p.birth_date
  ,year(p.birth_date)                         as birth_year
  ,p.death_date
  ,p.age_years
  ,p.gender_male
  ,p.race
  ,p.ethnicity
  ,p.zipcode
  ,mx.mrn_meditech_externals
  ,mx.mrn_gecbs
  ,mx.mrn_epic_externals
FROM {project_schema}.pt_pool pp
  inner join cdw_outpost.snowflake_2.person p        on pp.mrn_mpi = p.mrn_mpi
  left  join cdw_mpi_1.cluster.mrn_mpi_collapsed mx  on pp.mrn_mpi = mx.mrn_mpi
ORDER BY pp.pt_index;

-- (N rows affected) HH:MM:SS
