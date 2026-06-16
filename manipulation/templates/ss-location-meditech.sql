-- ================================================================================================
-- TEMPLATE: ss-location-meditech.sql
-- Source:   cdw_meditech.dictionary.location
-- Purpose:  Discovery query - find Meditech locations matching study clinic keywords.
--           Run this query, review in SSMS or Excel, set desired = 'TRUE' for locations to include.
--           The final ss_location_meditech table is used as a filter join in encounter queries.
--
--           Typical join pattern:
--               inner join {project_schema}.ss_location_meditech sl on v.location_mnemonic = sl.location_mnemonic
--
-- !! CUSTOMIZE keyword patterns in the WHERE / CASE block
-- ================================================================================================

use cdw_cache_staging;

-- ---- STEP 1: Discovery -----------------------------------------------------------------------

SELECT
  d.location_mnemonic
  ,d.location_description
  ,d.facility_name
  ,d.campus_name
  ,d.location_type
  ,d.location_subtype
  ,case
  when d.location_description like '%{keyword}%'  then 'TRUE'
  when d.campus_name like '%{keyword}%'            then 'TRUE'
  else 'FALSE'
  end                                              as desired
  ,''                                               as category
FROM cdw_meditech.dictionary.location d
WHERE
  -- !! Replace with keywords relevant to your study:
  d.location_description like '%{keyword}%'
  or d.campus_name like '%{keyword}%'
ORDER BY desired desc, d.location_description;

-- ---- STEP 2: Load ss_location_meditech after PI review --------------------------------------

/*
DROP TABLE if exists {project_schema}.ss_location_meditech;
--exec dbo.generate_create_table_sp '{project_schema}.ss_location_meditech'
CREATE TABLE {project_schema}.ss_location_meditech (
  location_mnemonic    varchar(10)  primary key,
  location_description varchar(30),
  facility_name        varchar(30),
  campus_name          varchar(30),
  category             varchar(100),
);

INSERT {project_schema}.ss_location_meditech
VALUES
  ('{location_mnemonic}', '{location_description}', '{facility_name}', '{campus_name}', '{category}'),
  -- ...
*/
