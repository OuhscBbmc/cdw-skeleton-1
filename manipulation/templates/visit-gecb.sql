-- ================================================================================================
-- TEMPLATE: visit-gecb.sql
-- Source:   cdw_outpost.gecb.fact_sched  (scheduled/appointment visits)
-- Applies:  appt_dates < 2023-06-03  (GECB - outpatient scheduling system)
-- Purpose:  Outpatient appointments and scheduled visits from GECB.
--           fact_sched = appointment/scheduling data (not billing claims).
--           For billing claims/CPT codes use invoice-gecb.sql instead.
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop
-- !! CUSTOMIZE ss_location_gecb join - build this lookup by running a DISTINCT on
--    sched_location_id to identify desired clinic locations.
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start       date        = '{date_start}';
DECLARE @date_stop_legacy date        = '2023-06-02';
DECLARE @visit_status     varchar(50) = 'arrived';   -- 'arrived' | 'no show' | 'cancelled' | remove for all

DROP TABLE if exists {project_schema}.visit_gecb;
--exec dbo.generate_create_table_sp '{project_schema}.visit_gecb'
CREATE TABLE {project_schema}.visit_gecb (
  visit_gecb_index           int           identity primary key,
  visit_number               int           not null unique,
  mrn_mpi                    int           not null,
  mrn_gecb                   int           not null,
  visit_index_within_patient bigint,
  appt_date                  date          not null,
  appt_datetime              smalldatetime not null,
  appt_duration              smallint      not null,
  provider_name              varchar(75)   not null,
  visit_type                 varchar(160)  not null,
  sched_location             varchar(40)   not null,
  clinic_name                varchar(40),
  sched_location_id          int           not null,
  billing_loc_name           varchar(100),
  visit_status               varchar(30)   not null,
);

INSERT {project_schema}.visit_gecb
SELECT
  fs.visit_number
  ,na.mrn_mpi
  ,fs.mrn_gecb
  ,row_number() over (
    partition by fs.mrn_gecb
    order by fs.visit_number
    )                                          as visit_index_within_patient
  ,fs.appt_date
  ,fs.appt_datetime
  ,fs.appt_duration
  ,fs.provider_name
  ,fs.visit_type
  ,fs.sched_location
  ,fs.clinic_name
  ,fs.sched_location_id
  ,fs.billing_loc_name
  ,fs.visit_status
FROM cdw_outpost.gecb.fact_sched fs
-- !! Join to study-specific location lookup; remove to pull all locations:
  inner join {project_schema}.ss_location_gecb ge  on fs.sched_location_id = ge.sched_location_id
  inner join cdw_mpi_1.groomed.node_assigned na     on fs.mrn_gecb = na.mrn_gecb
  inner join {project_schema}.pt_pool pp            on na.mrn_mpi = pp.mrn_mpi
WHERE
  fs.appt_date between @date_start and @date_stop_legacy
  and fs.visit_status = @visit_status   -- remove this line to include all statuses
ORDER BY na.mrn_gecb, fs.visit_number;

-- (N rows affected) HH:MM:SS
