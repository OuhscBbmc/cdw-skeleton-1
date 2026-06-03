-- ================================================================================================
-- TEMPLATE: medication-centricity.sql
-- Source:   cdw_centricity.centricity.medicate
-- Applies:  startdate < 2023-06-03  (Centricity EHR — outpatient/ambulatory)
-- Purpose:  Medication orders from Centricity (prescription/order history).
--           Has richer pharmacy fields than Meditech: NDC, GPI, RxNorm, start/stop dates.
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop, medication filter
-- ================================================================================================

use cdw_cache_staging;

declare @date_start   date = '{date_start}';
declare @date_stop    date = '2023-06-02';


DROP TABLE IF EXISTS [cdw_cache_staging].[{project_schema}].[medication_centricity];
CREATE TABLE [cdw_cache_staging].[{project_schema}].[medication_centricity] (
    med_centricity_index    int             identity(1,1) primary key,
    mrn_mpi                 int             not null,
    index_within_patient    bigint          null,
    mid                     bigint          not null,   -- Centricity medication record ID
    sdid                    bigint          not null,   -- Centricity document ID (encounter link)
    description             varchar(80)     not null,   -- brand/trade name
    genericmed              varchar(60)     null,
    instructions            varchar(1400)   null,
    startdate               date            null,
    stopdate                date            null,
    stop_date_calc          date            null,
    stopreason              varchar(1)      null,
    route                   varchar(30)     null,
    dose                    numeric(19,5)   null,
    ndc_11                  varchar(11)     null,
    gpi                     varchar(14)     null,
    rxnorm                  int             null,
    age_years               int             null,
    -- Study classification:
    med_category            varchar(100)    null,
);

INSERT INTO {project_schema}.medication_centricity
SELECT
    na.mrn_mpi
    ,row_number() over (
        partition by na.mrn_mpi
        order by m.startdate, m.description, m.mid
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
    and m.startdate <= @date_stop
ORDER BY na.mrn_mpi, m.startdate;

-- (N rows affected) HH:MM:SS
