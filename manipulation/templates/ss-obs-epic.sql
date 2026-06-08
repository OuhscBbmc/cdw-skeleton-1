-- ================================================================================================
-- TEMPLATE: ss-obs-epic.sql
-- Source:   cdw_epic_waystation.caboodle.flowsheet_row_dim + cdw_epic.caboodle.flowsheet
-- Purpose:  Discovery query -- find Epic flowsheet rows (vitals, scores, assessments) matching
--           study keywords. Includes usage stats (patient count, obs count, value range) to help
--           the PI decide which rows to include. Set desired = 'TRUE' and fill obs_category.
--
--           Joins in obs-epic.sql:
--               inner join {project_schema}.ss_obs_epic ss on f.flowsheet_row_key = ss.flowsheet_row_key
--           Column selected: ss.obs_category
--
-- !! CUSTOMIZE @date_start -- the discovery query scopes to observed rows after this date
-- !! CUSTOMIZE keyword patterns in the WHERE clause
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start_epic date = '2023-06-03';

-- ---- STEP 1: Discovery -- scoped to observed flowsheet rows ----------------------------------

SELECT
  frd.flowsheet_row_key
  ,frd.name
  ,frd.display_name
  ,frd.description
  ,frd.value_type
  ,frd.unit
  ,case
    when frd.name like '%weight%'              then 'weight'
    when frd.name like '%height%'              then 'height'
    when frd.name like '%bmi%'                 then 'bmi'
    when frd.name like '%blood%pressure%'      then 'blood_pressure'
    when frd.name like '%systolic%'            then 'blood_pressure'
    when frd.name like '%diastolic%'           then 'blood_pressure'
    when frd.name like '%pulse%'               then 'heart_rate'
    when frd.name like '%heart%rate%'          then 'heart_rate'
    when frd.name like '%temperature%'         then 'temperature'
    when frd.name like '%spo2%'                then 'oxygen_saturation'
    when frd.name like '%oxygen%sat%'          then 'oxygen_saturation'
    when frd.name like '%respir%'              then 'respiratory_rate'
    when frd.description like '%weight%'       then 'weight'
    when frd.description like '%height%'       then 'height'
    else ''
    end                                         as category_suggested
  ,''                                          as obs_category   -- fill in after review
  ,'TRUE'                                      as desired
  ,count(distinct f.mrn_epic_durable)          as pt_count
  ,count(f.flowsheet_row_key)                  as obs_count
  ,count(distinct f.encounter_key)             as encounter_count
  ,min(f.taken_date)                           as obs_date_min
  ,max(f.taken_date)                           as obs_date_max
FROM cdw_epic_waystation.caboodle.flowsheet_row_dim frd
  inner join cdw_epic.caboodle.flowsheet f   on frd.flowsheet_row_key = f.flowsheet_row_key
WHERE
  @date_start_epic <= f.taken_date
  and
  (
    -- !! Replace with keywords relevant to your study:
    frd.name like '%{keyword}%'
    or frd.description like '%{keyword}%'
  )
GROUP BY
  frd.flowsheet_row_key
  ,frd.name
  ,frd.display_name
  ,frd.description
  ,frd.value_type
  ,frd.unit
ORDER BY pt_count desc, frd.name;

-- ---- STEP 2: Load ss_obs_epic after PI review -----------------------------------------------

/*
DROP TABLE if exists {project_schema}.ss_obs_epic;
--exec dbo.generate_create_table_sp '{project_schema}.ss_obs_epic'
CREATE TABLE {project_schema}.ss_obs_epic (
  flowsheet_row_key int          primary key,
  name              varchar(250) not null,
  obs_category      varchar(100) not null,
);

INSERT {project_schema}.ss_obs_epic
VALUES
  ({flowsheet_row_key}, '{name}', '{obs_category}'),
  -- ...
*/
