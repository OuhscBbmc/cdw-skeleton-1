-- ================================================================================================
-- TEMPLATE: ss-lab.sql  (harmonized: Epic + Meditech)
-- Sources:  cdw_epic_waystation.caboodle.lab_component_dim  (Epic, >= 2023-06-03)
--           cdw_meditech.dictionary.lab                      (Meditech, < 2023-06-03)
-- Purpose:  Discovery query - find lab components matching study keywords from both systems.
--           Run this query, review in SSMS or Excel, set desired = 'TRUE' and fill lab_category.
--           The final ss_lab table is loaded from the approved rows.
--
--           Joins in lab-epic.sql:
--               inner join {project_schema}.ss_lab ss on lr.lab_component_key = ss.lab_component_key
--           Joins in lab-meditech.sql:
--               inner join {project_schema}.ss_lab ss on l.mnemonic = ss.mnemonic
--           Column selected: ss.lab_category
--
-- !! CUSTOMIZE @keywords - semicolon-delimited name fragments
-- ================================================================================================

use cdw_cache_staging;

DECLARE @keywords varchar(1000) = '%{keyword_1}%;%{keyword_2}%';   -- semicolon-delimited like patterns

-- ---- STEP 1: Discovery -----------------------------------------------------------------------

-- ---- Epic arm -------------------------------------------------------------------------------
SELECT
  'epic'                                     as source_system
  ,lcd.lab_component_key
  ,null                                      as mnemonic
  ,lcd.name
  ,lcd.common_name
  ,nullif(lcd.loinc_code, '*Unspecified')    as loinc_code
  ,nullif(lcd.loinc_name, '*Unspecified')    as loinc_name
  ,lcd.default_unit
  ,s.value                                   as matched_pattern
  ,'TRUE'                                    as desired
  ,''                                        as lab_category
FROM cdw_epic_waystation.caboodle.lab_component_dim lcd
  cross apply string_split(@keywords, ';') s
WHERE
  (lcd.name like s.value or lcd.common_name like s.value)
  and lcd.lab_component_key in (SELECT distinct lab_component_key FROM cdw_epic.caboodle.lab_component_result)

UNION ALL

-- ---- Meditech arm ---------------------------------------------------------------------------
SELECT
  'meditech'
  ,null
  ,d.mnemonic
  ,d.name
  ,null                                      as common_name
  ,d.loinc                                   as loinc_code
  ,null                                      as loinc_name
  ,null                                      as default_unit
  ,s.value                                   as matched_pattern
  ,'TRUE'                                    as desired
  ,''                                        as lab_category
FROM cdw_meditech.dictionary.lab d
  cross apply string_split(@keywords, ';') s
WHERE
  d.name like s.value

ORDER BY source_system, lab_category, name;

-- ---- STEP 2: Load ss_lab after PI review ----------------------------------------------------

/*
DROP TABLE if exists {project_schema}.ss_lab;
--exec dbo.generate_create_table_sp '{project_schema}.ss_lab'
CREATE TABLE {project_schema}.ss_lab (
  ss_lab_index      int          identity primary key,
  source_system     varchar(10)  not null,   -- 'epic' | 'meditech'
  lab_component_key int,   -- Epic join key; null for Meditech rows
  mnemonic          varchar(15),   -- Meditech join key; null for Epic rows
  name              varchar(150),
  loinc_code        varchar(30),
  lab_category      varchar(100) not null,
);

INSERT {project_schema}.ss_lab
-- paste approved rows from Step 1, or re-run with desired = 'TRUE' filter
*/
