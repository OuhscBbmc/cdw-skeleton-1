-- ================================================================================================
-- TEMPLATE: medication-centricity.sql
-- Source:   cdw_centricity.centricity.medicate
-- Applies:  startdate < 2023-06-03  (Centricity EHR - outpatient/ambulatory)
-- Purpose:  Medication orders from Centricity (prescription/order history).
--           Has richer pharmacy fields than Meditech: NDC, GPI, RxNorm, start/stop dates.
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop, medication filter
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start   date = '{date_start}';
DECLARE @date_stop_legacy date = '2023-06-02';

drop table if exists {project_schema}.medication_centricity;
--exec dbo.generate_create_table_sp '{project_schema}.medication_centricity'
create table {project_schema}.medication_centricity (
  med_centricity_index    int             identity primary key,
  mrn_mpi                 int             not null,
  index_within_patient    bigint,
  mid                     bigint          not null,   -- Centricity medication record ID
  sdid                    bigint          not null,   -- Centricity document ID (encounter link)
  description             varchar(80)     not null,   -- brand/trade name
  genericmed              varchar(60),
  instructions            varchar(1400),
  startdate               date,
  stopdate                date,
  stop_date_calc          date,
  stopreason              varchar(1),
  route                   varchar(30),
  dose                    numeric(19,5),
  ndc_11                  varchar(11),
  gpi                     varchar(14),
  rxnorm                  int,
  age_years               int,
  -- Study classification:
  med_category            varchar(100),
);

insert {project_schema}.medication_centricity
SELECT
  na.mrn_mpi
  ,row_number() over (
    partition by na.mrn_mpi
    ORDER BY m.startdate, m.description, m.mid
    )                                          as index_within_patient
  ,m.mid
  ,m.sdid
  ,m.description
  ,m.genericmed
  ,m.instructions
  ,m.startdate
  ,m.stopdate
  ,m.stop_date_calc
  ,m.stopreason
  ,m.route
  ,m.dose
  ,m.ndc_11
  ,m.gpi
  ,m.rxnorm
  ,m.age_years
  ,ss.med_category
FROM cdw_centricity.centricity.medicate m
  inner join cdw_mpi_1.groomed.node_assigned na   on m.mrn_centricity = na.mrn_centricity
  inner join {project_schema}.pt_pool pp           on na.mrn_mpi = pp.mrn_mpi
-- !! Join to study-specific lookup; remove/swap for name-based filter:
  inner join {project_schema}.ss_med ss            on m.rxnorm = ss.rxnorm
-- Name-based alternative: and m.description like '%metformin%'
-- GPI-based alternative: and left(m.gpi, 4) in ('2710', '2720')  -- antidiabetics
WHERE
  @date_start <= m.startdate
  and m.startdate <= @date_stop_legacy
ORDER BY na.mrn_mpi, m.startdate;

-- (N rows affected) HH:MM:SS
