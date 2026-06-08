-- ================================================================================================
-- TEMPLATE: ss-location-gecb.sql
-- Source:   cdw_gecb.gecb.fact_sched
-- Purpose:  Discovery query - find GECB scheduling locations matching study clinic keywords.
--           Run this query, review in SSMS or Excel, set desired = 'TRUE' for locations to include.
--           The final ss_location_gecb table is used as a filter join in visit/encounter queries.
--
--           Typical join pattern:
--               inner join {project_schema}.ss_location_gecb sl on s.sched_location_id = sl.sched_location_id
--
-- !! CUSTOMIZE keyword patterns in the WHERE / CASE block
-- !! CUSTOMIZE @date_start / @date_stop to scope to the relevant scheduling period
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start       date = '2016-01-01';
DECLARE @date_stop_legacy date = '2023-06-02';

-- ---- STEP 1: Discovery -----------------------------------------------------------------------

SELECT
  distinct
  s.sched_location_id
  ,s.sched_location
  ,s.clinic_name
  ,s.billing_loc_name
  ,case
  when s.clinic_name like '%{keyword}%'         then 'TRUE'
  when s.billing_loc_name like '%{keyword}%'    then 'TRUE'
  when s.sched_location like '%{keyword}%'      then 'TRUE'
  else 'FALSE'
  end                                            as desired
  ,''                                             as category
FROM cdw_gecb.gecb.fact_sched s
WHERE
  s.visit_status = 'arrived'
  and s.appt_date between @date_start and @date_stop_legacy
ORDER BY desired desc, s.clinic_name;

-- ---- STEP 2: Load ss_location_gecb after PI review ------------------------------------------

/*
DROP TABLE if exists {project_schema}.ss_location_gecb;
--exec dbo.generate_create_table_sp '{project_schema}.ss_location_gecb'
CREATE TABLE {project_schema}.ss_location_gecb (
  sched_location_id int          primary key,
  sched_location    varchar(100),
  clinic_name       varchar(100),
  category          varchar(100),
);

INSERT {project_schema}.ss_location_gecb
VALUES
  ({sched_location_id}, '{sched_location}', '{clinic_name}', '{category}'),
  -- ...
*/
