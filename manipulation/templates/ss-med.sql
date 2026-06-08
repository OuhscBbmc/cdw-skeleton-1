-- ================================================================================================
-- TEMPLATE: ss-med.sql  (harmonized: Epic + Meditech)
-- Sources:  cdw_epic_waystation.caboodle.medication_dim   (Epic, >= 2023-06-03)
--           cdw_meditech.dictionary.medication             (Meditech, < 2023-06-03)
-- Purpose:  Discovery query - find medications matching study keywords from both systems.
--           Run this query, review in SSMS or Excel, set desired = 'TRUE' and fill med_category.
--           The final ss_med table is loaded from the approved rows.
--
--           Joins in medication-epic.sql:
--               inner join {project_schema}.ss_med ss on m.medication_key = ss.medication_key
--           Joins in medication-meditech.sql:
--               inner join {project_schema}.ss_med ss on m.medication_name = ss.medication_name_meditech
--           Column selected: ss.med_category
--
-- !! CUSTOMIZE @keywords - semicolon-delimited name fragments, e.g. '%metformin%;%lisinopril%'
-- ================================================================================================

use cdw_cache_staging;

DECLARE @keywords varchar(2000) = '%{keyword_1}%;%{keyword_2}%';   -- semicolon-delimited like patterns

-- ---- STEP 1: Discovery -----------------------------------------------------------------------

-- ---- Epic arm -------------------------------------------------------------------------------
SELECT
  'epic'                                     as source_system
  ,md.medication_key
  ,null                                      as medication_name_meditech
  ,md.name                                   as medication_name
  ,md.simple_generic_name                    as generic_name
  ,md.pharmaceutical_class
  ,md.therapeutic_class
  ,s.value                                   as matched_pattern
  ,'TRUE'                                    as desired
  ,''                                        as med_category
FROM cdw_epic_waystation.caboodle.medication_dim md
  cross apply string_split(@keywords, ';') s
WHERE
  (md.name like s.value or md.simple_generic_name like s.value)
  and md.medication_key in (SELECT distinct medication_key FROM cdw_epic.caboodle.medication_event)

UNION ALL

-- ---- Meditech arm ---------------------------------------------------------------------------
SELECT
  'meditech'
  ,null
  ,m.medication_name
  ,m.medication_name                         as medication_name
  ,m.generic                                 as generic_name
  ,null                                      as pharmaceutical_class
  ,null                                      as therapeutic_class
  ,s.value                                   as matched_pattern
  ,'TRUE'                                    as desired
  ,''                                        as med_category
FROM cdw_meditech.dictionary.medication m
  cross apply string_split(@keywords, ';') s
WHERE
  m.medication_name like s.value
  or m.generic like s.value

ORDER BY source_system, med_category, medication_name;

-- ---- STEP 2: Load ss_med after PI review ----------------------------------------------------

/*
DROP TABLE if exists {project_schema}.ss_med;
--exec dbo.generate_create_table_sp '{project_schema}.ss_med'
CREATE TABLE {project_schema}.ss_med (
  ss_med_index             int          identity primary key,
  source_system            varchar(10)  not null,               -- 'epic' | 'meditech'
  medication_key           int,                                 -- Epic join key; null for Meditech rows
  medication_name_meditech varchar(50),                         -- Meditech join key; null for Epic rows
  medication_name          varchar(300),
  generic_name             varchar(300),
  med_category             varchar(100) not null,
);

INSERT {project_schema}.ss_med
-- paste approved rows from Step 1, or re-run with desired = 'TRUE' filter
*/
