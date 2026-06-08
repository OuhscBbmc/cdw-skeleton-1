-- ================================================================================================
-- TEMPLATE: invoice-gecb.sql
-- Source:   cdw_gecb.gecb.fact_invoice + cdw_gecb.waystation dims
-- Applies:  service dates < 2023-06-03  (legacy GECB/IDX system)
-- Purpose:  Billing claims, procedures (CPTs), and diagnoses from GECB/IDX
--           Includes a CPT arm via fact_transac + dim_cpt for procedure codes.
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop
-- !! CUSTOMIZE the ss_dx join for diagnosis filtering if needed
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start       date = '{date_start}';
DECLARE @date_stop_legacy date = '2023-06-02';

-- ---- Invoice / Claim header ------------------------------------------------------------------
DROP TABLE if exists {project_schema}.invoice_gecb;
--exec dbo.generate_create_table_sp '{project_schema}.invoice_gecb'
CREATE TABLE {project_schema}.invoice_gecb (
  invoice_gecb_index int          identity primary key,
  mrn_gecb           int          not null,
  mrn_mpi            int          not null,
  invpk              int          not null,   -- GECB invoice PK; join key for CPTs/diagnoses
  invnum             int          not null,
  inv_service_date   date         not null,
  location_name      varchar(100),
  provider_name      varchar(100),
  icd_code           varchar(40),
  icd_description    varchar(256),
  visit_number       int,
);

-- ---- CPT codes per invoice -------------------------------------------------------------------
DROP TABLE if exists {project_schema}.cpt_gecb;
--exec dbo.generate_create_table_sp '{project_schema}.cpt_gecb'
CREATE TABLE {project_schema}.cpt_gecb (
  cpt_gecb_index      int          identity primary key,
  mrn_mpi             int          not null,
  invpk               int          not null,
  inv_service_date    date         not null,
  visit_number        int,
  billing_code        varchar(10),
  billing_description varchar(100),
  billing_category    varchar(100),
  vocabulary_id       varchar(10),
  provider_name       varchar(100),
);

WITH pt as (
  SELECT
    pp.mrn_mpi
    ,na.mrn_gecb
  FROM {project_schema}.pt_pool pp
  inner join cdw_mpi_1.groomed.node_assigned na on pp.mrn_mpi = na.mrn_mpi and na.type = 'gecb'
)
INSERT {project_schema}.invoice_gecb
SELECT
  i.mrn_gecb
  ,pt.mrn_mpi
  ,i.invpk
  ,i.invoice_number
  ,i.inv_service_date
  ,l.locname                  as location_name
  ,p.providername             as provider_name
  ,d.diagcd                   as icd_code
  ,d.diagdesc                 as icd_description
  ,i.visit_number
FROM cdw_gecb.gecb.fact_invoice i
  inner join pt                                        on i.mrn_gecb = pt.mrn_gecb
  left  join cdw_gecb.waystation.diag_help_inv dhi  on i.invpk = dhi.dhifkinvpk
  left  join cdw_gecb.waystation.dim_diagnosis d     on dhi.dhifkdiagpk = d.diagpk
  left  join cdw_gecb.waystation.dim_location l      on i.inv_location_id = l.locpk
  left  join cdw_gecb.waystation.dim_provider p      on i.provider_attending_id = p.providerpk
WHERE
  i.inv_service_date between @date_start and @date_stop_legacy
ORDER BY pt.mrn_mpi, i.inv_service_date;
INSERT {project_schema}.cpt_gecb
SELECT
  ig.mrn_mpi
  ,ig.invpk
  ,ig.inv_service_date
  ,ig.visit_number
  ,c.billing_code
  ,c.billing_description
  ,c.billing_category
  ,c.vocabulary_id
  ,ig.provider_name
FROM {project_schema}.invoice_gecb ig
  inner join cdw_gecb.gecb.fact_transac t       on ig.invpk = t.trnfkinvpk
  inner join cdw_gecb.waystation.dim_cpt c      on t.trncptid = c.cpt_pk
WHERE c.billing_code is not null
ORDER BY ig.mrn_mpi, ig.inv_service_date;

-- (N rows affected) HH:MM:SS
