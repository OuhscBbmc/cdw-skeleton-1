-- ================================================================================================
-- TEMPLATE: ss-cpt.sql
-- Sources:  cdw_gecb.waystation.dim_cpt                    (GECB, historical)
--           cdw_epic_waystation.caboodle.procedure_snapshot_dim  (Epic, >= 2023-06-03)
-- Purpose:  Discovery query - find CPT codes matching study procedure keywords from both systems.
--           Run this query to build the @cpts variable for procedure-harmonized.sql, or to load
--           a full ss_cpt lookup table if you need procedure-level category classification.
--
--           Usage in procedure-harmonized.sql (simple list approach):
--               DECLARE @cpts varchar(2000) = '27447;27446;...'
--               and t.billing_code in (SELECT [value] FROM string_split(@cpts, ';'))
--
--           Usage with ss_cpt lookup table (if category classification is needed):
--               inner join {project_schema}.ss_cpt sc on pe.cpt_code = sc.cpt_code
--
-- !! CUSTOMIZE keyword patterns below
-- ================================================================================================

use cdw_cache_staging;

-- ---- STEP 1: Discovery -----------------------------------------------------------------------

-- ---- GECB arm -------------------------------------------------------------------------------
SELECT
  'gecb'                                     as source_system
  ,cp.cpt_hcpc_code                          as cpt_code
  ,cp.billing_description                    as procedure_name
  ,null                                      as procedure_category
  ,'TRUE'                                    as desired
  ,''                                        as category
FROM cdw_gecb.waystation.dim_cpt cp
WHERE
  cp.billing_description like '%{keyword}%'

UNION ALL

-- ---- Epic arm -------------------------------------------------------------------------------
SELECT
  'epic'
  ,pd.cpt_code
  ,pd.name                                   as procedure_name
  ,pd.category                               as procedure_category
  ,'TRUE'                                    as desired
  ,''                                        as category
FROM cdw_epic_waystation.caboodle.procedure_snapshot_dim pd
WHERE
  pd.is_current = 1
  and pd.cpt_code is not null
  and (
    pd.name like '%{keyword}%'
    or pd.category like '%{keyword}%'
  )

ORDER BY source_system, cpt_code;

-- ---- STEP 2: Load ss_cpt after PI review (optional) -----------------------------------------
-- If you only need a CPT code list, just pull the distinct cpt_code values from Step 1
-- and paste them into the @cpts variable in procedure-harmonized.sql.
-- Only create the table below if you need procedure-level category classification.

/*
DROP TABLE if exists {project_schema}.ss_cpt;
--exec dbo.generate_create_table_sp '{project_schema}.ss_cpt'
CREATE TABLE {project_schema}.ss_cpt (
  cpt_code       varchar(10)  primary key,
  procedure_name varchar(254),
  category       varchar(100),
);

INSERT {project_schema}.ss_cpt
VALUES
  ('{cpt_code}', '{procedure_name}', '{category}'),
  -- ...
*/
