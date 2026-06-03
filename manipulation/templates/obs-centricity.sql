-- ================================================================================================
-- TEMPLATE: obs-centricity.sql
-- Source:   cdw_centricity.centricity.obs + cdw_centricity.dictionary.obs_head
-- Applies:  clinical_dates < 2023-06-03  (Centricity EHR — outpatient/ambulatory)
-- Purpose:  Observations (vitals, flowsheet values, structured visit data) from Centricity.
--           Centricity is the legacy ambulatory system; use obs-meditech for inpatient obs.
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop
-- !! CUSTOMIZE the hdid filter in WHERE — run a DISTINCT on oh.hdid/oh.name first to find IDs
-- ================================================================================================

use cdw_cache_staging;

declare @date_start   date = '{date_start}';
declare @date_stop    date = '2023-06-02';
-- Semicolon-delimited hdid list for your observations of interest:
declare @hdids        varchar(500) = '{hdids}';   -- e.g., '61;3095;5347;29139'
-- Alternatively filter by name/description/keyword patterns (see commented WHERE clause below)


DROP TABLE IF EXISTS [cdw_cache_staging].[{project_schema}].[obs_centricity];
CREATE TABLE [cdw_cache_staging].[{project_schema}].[obs_centricity] (
    obs_centricity_index    int             identity(1,1) primary key,
    mrn_mpi                 int             not null,
    index_within_patient    bigint          null,
    hdid                    int             not null,
    name                    varchar(15)     not null,
    description             varchar(220)    not null,
    keyword                 varchar(255)    null,
    obs_date                date            not null,
    obs_value               varchar(2000)   not null,
    obs_value_numeric       float           null,
);

INSERT INTO {project_schema}.obs_centricity
SELECT
    na.mrn_mpi
    ,row_number() over (
        partition by na.mrn_mpi
        order by o.obs_date, oh.name, o.obs_value
     )                                          as index_within_patient
    ,oh.hdid
    ,oh.name
    ,oh.description
    ,oh.keyword
    ,o.obs_date
    ,o.obs_value
    ,try_convert(float, o.obs_value)            as obs_value_numeric
FROM cdw_centricity.centricity.obs o
inner join cdw_centricity.dictionary.obs_head oh    on o.hdid = oh.hdid
inner join cdw_centricity.centricity.document d     on o.sdid = d.sdid
inner join cdw_mpi_1.groomed.node_assigned na       on o.mrn_centricity = na.mrn_centricity
inner join {project_schema}.pt_pool pp              on na.mrn_mpi = pp.mrn_mpi
WHERE
    @date_start <= d.clinical_date
    and d.clinical_date <= @date_stop
    -- Option A — filter by hdid list:
    and o.hdid in (SELECT cast([value] as int) FROM string_split(@hdids, ';'))
    -- Option B — filter by name/description pattern (comment out Option A, uncomment below):
    -- and (oh.name like '%weight%' or oh.description like '%weight%' or oh.keyword like '%weight%')
ORDER BY na.mrn_mpi, o.obs_date, oh.name;

-- (N rows affected) HH:MM:SS
