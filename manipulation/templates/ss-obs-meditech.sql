-- ================================================================================================
-- TEMPLATE: ss-obs-meditech.sql
-- Source:   cdw_meditech.dictionary.obs
-- Purpose:  Discovery query — find Meditech observation types (vitals, scores, flowsheet values)
--           matching study keywords. Set desired = 'TRUE', fill obs_name and obs_category.
--
--           Joins in obs-meditech.sql:
--               inner join {project_schema}.ss_obs ss on o.obs_key_id = ss.obs_key_id
--           Columns selected: ss.obs_name, ss.obs_category
--
-- !! CUSTOMIZE keyword patterns in the WHERE clause
-- ================================================================================================


-- ---- STEP 1: Discovery -----------------------------------------------------------------------

SELECT
  d.obs_key_id
  ,d.obs_mnemonic
  ,d.obs_nurse_name
  ,d.description
  ,d.type
  ,case
    when d.obs_nurse_name like '%weight%'       then 'weight'
    when d.obs_nurse_name like '%height%'       then 'height'
    when d.obs_nurse_name like '%bmi%'          then 'bmi'
    when d.obs_nurse_name like '%blood%press%'  then 'blood_pressure'
    when d.obs_nurse_name like '%systolic%'     then 'blood_pressure'
    when d.obs_nurse_name like '%diastolic%'    then 'blood_pressure'
    when d.obs_nurse_name like '%pulse%'        then 'heart_rate'
    when d.obs_nurse_name like '%heart%rate%'   then 'heart_rate'
    when d.obs_nurse_name like '%temp%'         then 'temperature'
    when d.obs_nurse_name like '%spo2%'         then 'oxygen_saturation'
    when d.obs_nurse_name like '%o2%sat%'       then 'oxygen_saturation'
    when d.obs_nurse_name like '%respir%'       then 'respiratory_rate'
    when d.description like '%weight%'          then 'weight'
    when d.description like '%height%'          then 'height'
    else ''
   end                                          as category_suggested
  ,''                                           as obs_name       -- readable name for final table
  ,''                                           as obs_category   -- fill in after review
  ,'TRUE'                                       as desired
FROM cdw_meditech.dictionary.obs d
WHERE
  d.active = 1
  and
  (
    -- !! Replace with keywords relevant to your study:
    d.obs_nurse_name like '%{keyword}%'
    or d.description like '%{keyword}%'
  )
ORDER BY d.obs_nurse_name;


-- ---- STEP 2: Load ss_obs after PI review ----------------------------------------------------

/*
DROP TABLE IF EXISTS [cdw_cache_staging].[{project_schema}].[ss_obs];
CREATE TABLE [cdw_cache_staging].[{project_schema}].[ss_obs] (
    obs_key_id      int             not null primary key,
    obs_mnemonic    varchar(50)     null,
    obs_name        varchar(100)    not null,
    obs_category    varchar(100)    not null,
);

INSERT INTO {project_schema}.ss_obs
VALUES
    ({obs_key_id}, '{obs_mnemonic}', '{obs_name}', '{obs_category}'),
    -- ...
*/
