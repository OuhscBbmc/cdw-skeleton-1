-- ================================================================================================
-- TEMPLATE: procedure-harmonized.sql
-- Source:   UNION of cdw_gecb.gecb.fact_invoice+fact_transac (< 2023-06-03)
--                 and cdw_epic.caboodle.procedure_event      (>= 2023-06-03)
-- Purpose:  CPT-coded procedures spanning both legacy GECB and Epic systems.
--           Both systems expose CPT codes so harmonization is straightforward.
--
-- !! CUSTOMIZE {project_schema}, @cpts (semicolon-delimited CPT code list), date params
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start       date          = '{date_start}';
DECLARE @date_stop        date          = '{date_stop}';
DECLARE @cpts             varchar(2000) = '{cpts}';   -- e.g., '27447;27446;27445' (knee replacement CPTs)
DECLARE @date_stop_legacy date          = '2023-06-02';
DECLARE @date_start_epic  date          = '2023-06-03';

DROP TABLE if exists {project_schema}.procedure_event;
--exec dbo.generate_create_table_sp '{project_schema}.procedure_event'
CREATE TABLE {project_schema}.procedure_event (
  proc_index          int          identity primary key,
  source_system       varchar(10)  not null,   -- 'gecb' | 'epic'
  mrn_mpi             int          not null,
  proc_date           date         not null,
  cpt_code            varchar(10)  not null,
  procedure_name      varchar(254),
  procedure_category  varchar(100),   -- Epic: procedure_category; GECB: billing_description
  provider_name       varchar(200),
  -- System-specific keys:
  invpk               int,   -- GECB invoice PK
  procedure_event_key int,   -- Epic procedure event key
);

-- ---- GECB arm (< 2023-06-03) -----------------------------------------------------------------
INSERT {project_schema}.procedure_event
SELECT distinct
  'gecb'
  ,na.mrn_mpi
  ,cast(i.inv_service_date as date)
  ,t.billing_code
  ,t.billing_description
  ,t.billing_category
  ,i.provider_attending
  ,i.invpk
  ,null
FROM cdw_gecb.gecb.fact_invoice i
  inner join cdw_gecb.gecb.fact_transac t         on i.invpk = t.trnfkinvpk
  inner join cdw_mpi_1.groomed.node_assigned na   on i.mrn_gecb = na.mrn_gecb
  inner join {project_schema}.pt_pool pp          on na.mrn_mpi = pp.mrn_mpi
WHERE
  i.inv_service_date between @date_start and @date_stop_legacy
  and t.billing_code in (SELECT [value] FROM string_split(@cpts, ';'))

UNION ALL

-- ---- Epic arm (>= 2023-06-03) ---------------------------------------------------------------
SELECT distinct
  'epic'
  ,na.mrn_mpi
  ,cast(pe.procedure_start_date as date)
  ,pe.cpt_code
  ,pe.procedure_name
  ,pe.procedure_category
  ,pe.performing_provider_name
  ,null
  ,pe.procedure_event_key
FROM cdw_epic.caboodle.procedure_event pe
  inner join cdw_mpi_1.groomed.node_assigned na   on pe.mrn_epic_durable = na.mrn_epic_durable
  inner join {project_schema}.pt_pool pp          on na.mrn_mpi = pp.mrn_mpi
WHERE
  pe.procedure_start_date between @date_start_epic and @date_stop
  and pe.cpt_code in (SELECT [value] FROM string_split(@cpts, ';'))
;

-- (N rows affected) HH:MM:SS
