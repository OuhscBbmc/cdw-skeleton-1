-- ================================================================================================
-- TEMPLATE: pt-identity.sql
-- Target:   cdw_transaction.{project_schema}.pt_identity  (NOT cdw_cache_staging)
-- Purpose:  Persistent identity table that maps mrn_mpi -> stable auto-increment record_id.
--           Stored in cdw_transaction so it survives cache refreshes.
--           Used for REDCap linking and longitudinal study IDs.
--
-- !! CRITICAL: Do NOT drop and recreate this table once populated - it holds the
--    stable study IDs. Only INSERT new patients as the cohort grows.
-- !! CUSTOMIZE {project_schema}
-- !! Run patient.sql first - this inserts from the patient table.
-- ================================================================================================

use cdw_cache_staging;

-- ------------------------------------------------------------------------------------------------
-- One-time setup: create the table (only run once per project; then comment out)
-- ------------------------------------------------------------------------------------------------
-- !! Don't drop & recreate once populated - this table has to be preserved!!
/*
drop table if exists cdw_transaction.{project_schema}.pt_identity;
--exec dbo.generate_create_table_sp '{project_schema}.pt_identity'
create table cdw_transaction.{project_schema}.pt_identity (
  mrn_mpi     int primary key,
  record_id   int     identity not null unique,   -- stable, auto-incrementing study ID,
);
*/

-- ------------------------------------------------------------------------------------------------
-- Incremental insert: add any patients in the patient table who don't yet have a record_id
-- Run this whenever the cohort is refreshed.
-- ------------------------------------------------------------------------------------------------
with mrn_to_add as (
  SELECT p.mrn_mpi
  FROM cdw_cache_staging.{project_schema}.patient p
  except
  SELECT pi.mrn_mpi
  FROM cdw_transaction.{project_schema}.pt_identity pi
)
insert cdw_transaction.{project_schema}.pt_identity
SELECT mrn_mpi
FROM mrn_to_add;

-- !! Don't drop & recreate this table once populated - this table has to be preserved!!

-- (N rows affected) HH:MM:SS
