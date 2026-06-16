-- ================================================================================================
-- TEMPLATE: ss-location-epic.sql
-- Source:   cdw_epic_waystation.caboodle.department_dim
-- Purpose:  Discovery query - find Epic departments matching study clinic keywords.
--           Run this query, review in SSMS or Excel, set desired = 'TRUE' for departments to include.
--           The final ss_location_epic table is used as a filter join in encounter queries.
--
--           Typical join pattern:
--               inner join {project_schema}.ss_location_epic sl on e.department_key = sl.department_key
--
-- !! CUSTOMIZE keyword patterns in the WHERE / CASE block
-- ================================================================================================

use cdw_cache_staging;

-- ---- STEP 1: Discovery -----------------------------------------------------------------------

WITH active_depts as (
  SELECT distinct department_key
  FROM cdw_epic.caboodle.encounter
  WHERE 1 <= department_key
)

SELECT
  d.department_key
  ,d.department_name
  ,d.department_external_name
  ,d.department_specialty
  ,d.location_name
  ,d.department_type
  ,d.state_or_province_abbreviation
  ,case
  when d.department_external_name like '%{keyword}%'   then 'TRUE'
  when d.department_name like '%{keyword}%'             then 'TRUE'
  when d.department_specialty like '%{keyword}%'        then 'TRUE'
  else 'FALSE'
  end                                                   as desired
  ,''                                                    as category
FROM cdw_epic_waystation.caboodle.department_dim d
  inner join active_depts ad   on d.department_key = ad.department_key
WHERE
  d.state_or_province_abbreviation = 'OK'
  and
  (
  -- !! Replace with keywords relevant to your study:
  d.department_external_name like '%{keyword}%'
  or d.department_specialty like '%{keyword}%'
  )
ORDER BY desired desc, d.department_external_name;

-- ---- STEP 2: Load ss_location_epic after PI review ------------------------------------------

/*
DROP TABLE if exists {project_schema}.ss_location_epic;
--exec dbo.generate_create_table_sp '{project_schema}.ss_location_epic'
CREATE TABLE {project_schema}.ss_location_epic (
  department_key           int          primary key,
  department_name          varchar(300),
  department_external_name varchar(300),
  category                 varchar(100),
);

INSERT {project_schema}.ss_location_epic
VALUES
  ({department_key}, '{department_name}', '{department_external_name}', '{category}'),
  -- ...
*/
