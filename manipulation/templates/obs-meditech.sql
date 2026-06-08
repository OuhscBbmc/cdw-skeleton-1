-- ================================================================================================
-- TEMPLATE: obs-meditech.sql
-- Source:   cdw_meditech.meditech.obs  (observations: vitals, scores, flowsheet values)
-- Applies:  observation dates < 2023-06-03  (Meditech)
-- Purpose:  Vitals, clinical scores, and other flowsheet observations from Meditech
--           Requires a study-specific obs lookup (ss_obs) mapping obs_key_ids to test names.
--
-- !! CUSTOMIZE {project_schema}, @date_start, @hdids (health data item IDs)
-- !! NOTE: Meditech obs are identified by hdid (health data item ID), not a mnemonic.
--          Build ss_obs by running a DISTINCT query on obs_key_id for your tests of interest.
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start   date           = '{date_start}';
DECLARE @date_stop_legacy date = '2023-06-02';
-- Semicolon-delimited hdid (obs_key_id) list for your tests of interest:
DECLARE @hdids        varchar(500)   = '{hdids}';   -- e.g., '15745;25468;132931' (asthma control, etc.)

drop table if exists {project_schema}.obs_meditech;
--exec dbo.generate_create_table_sp '{project_schema}.obs_meditech'
create table {project_schema}.obs_meditech (
  obs_meditech_index      int             identity primary key,
  account_number          char(12)        not null,
  mrn_mpi                 int             not null,
  mrn_meditech_internal   varchar(10)     not null,
  obs_key_id              int             not null,   -- hdid; identifies the measurement type
  obs_datetime            smalldatetime   not null,
  obs_name                varchar(100),       -- human-readable name FROM ss_obs
  obs_mnemonic            varchar(15),       -- meditech obs mnemonic
  obs_value               varchar(200),
  obs_value_numeric       float,
  obs_category            varchar(100),       -- study classification FROM ss_obs,
);

insert {project_schema}.obs_meditech
SELECT
  o.account_number,na.mrn_mpi,o.mrn_meditech_internal,o.obs_key_id,o.obs_datetime,ss.obs_name,o.obs_mnemonic,o.obs_value,try_convert(float, o.obs_value)        as obs_value_numeric,ss.obs_category
FROM cdw_meditech.meditech.obs o
  inner join cdw_mpi_1.groomed.node_assigned na   on o.mrn_meditech_internal = na.mrn_meditech_internal
  inner join {project_schema}.pt_pool pp           on na.mrn_mpi = pp.mrn_mpi
-- Option A - join to study-specific lookup:
  inner join {project_schema}.ss_obs ss            on o.obs_key_id = ss.obs_key_id
-- Option B - filter by hdid list (replace ss_obs join with this):
-- WHERE o.obs_key_id in (SELECT cast([value] as int) FROM string_split(@hdids, ';'))
WHERE
  o.obs_datetime between @date_start and @date_stop_legacy
ORDER BY na.mrn_mpi, o.obs_datetime;

-- (N rows affected) HH:MM:SS
