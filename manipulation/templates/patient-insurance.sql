-- ================================================================================================
-- TEMPLATE: patient-insurance.sql
-- Source:   cdw_epic.caboodle.billing_account + cdw_gecb.gecb.fact_invoice
-- Purpose:  Most recent insurance category per patient, from each system.
--           Run after patient.sql.
--
-- !! CUSTOMIZE {project_schema}
-- ================================================================================================

use cdw_cache_staging;

DROP TABLE if exists {project_schema}.patient_insurance;
--exec dbo.generate_create_table_sp '{project_schema}.patient_insurance'
CREATE TABLE {project_schema}.patient_insurance (
  mrn_mpi                 int          primary key,
  last_insurance_cat_epic varchar(100),
  last_insurance_cat_gecb varchar(100),
);

WITH insurance_epic as (
  SELECT
    na.mrn_mpi
    ,ba.primary_benefit_payor_class
    ,row_number() over (partition by na.mrn_mpi order by e.encounter_start_date desc) as rn
  FROM cdw_epic.caboodle.encounter e
    inner join cdw_epic.caboodle.billing_account ba   on e.encounter_key = ba.primary_encounter_key
    inner join cdw_mpi_1.groomed.node_assigned na     on e.mrn_epic_durable = na.mrn_epic_durable
    inner join {project_schema}.pt_pool pp            on na.mrn_mpi = pp.mrn_mpi
)
,insurance_gecb as (
  SELECT
    na.mrn_mpi
    ,i.fsc_category_1
    ,row_number() over (partition by na.mrn_mpi order by i.inv_service_date desc) as rn
  FROM {project_schema}.pt_pool pp
    inner join cdw_mpi_1.groomed.node_assigned na on pp.mrn_mpi = na.mrn_mpi and na.type = 'gecb'
    inner join cdw_gecb.gecb.fact_invoice i       on na.mrn_gecb = i.mrn_gecb
)
INSERT {project_schema}.patient_insurance
SELECT
  pp.mrn_mpi
  ,ie.primary_benefit_payor_class     as last_insurance_cat_epic
  ,ig.fsc_category_1                  as last_insurance_cat_gecb
FROM {project_schema}.pt_pool pp
  left  join insurance_epic ie  on pp.mrn_mpi = ie.mrn_mpi and ie.rn = 1
  left  join insurance_gecb ig  on pp.mrn_mpi = ig.mrn_mpi and ig.rn = 1
ORDER BY pp.mrn_mpi;

-- (N rows affected) HH:MM:SS
