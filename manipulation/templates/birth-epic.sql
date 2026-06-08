-- ================================================================================================
-- TEMPLATE: birth-epic.sql
-- Source:   cdw_epic.caboodle.birth
-- Applies:  birth_datetime >= 2023-06-03  (Epic)
-- Purpose:  Birth events with obstetric detail from Epic Caboodle.
--           One row per birth (baby). Join is via mother's mrn_epic_durable.
--           Includes gestational age, delivery method, APGAR scores, birth weight/length,
--           and pregnancy timeline fields.
--
-- !! CUSTOMIZE {project_schema}, @date_start, @date_stop
-- !! Join logic: default joins via mother's mrn_mpi in pt_pool.
--    For baby-centric studies, join via baby_mrn_epic_durable instead.
-- ================================================================================================

use cdw_cache_staging;

DECLARE @date_start_epic date          = '2023-06-03';
DECLARE @date_stop       date          = '{date_stop}';

DROP TABLE if exists {project_schema}.birth_epic;
--exec dbo.generate_create_table_sp '{project_schema}.birth_epic'
CREATE TABLE {project_schema}.birth_epic (
  birth_epic_index                   int           identity primary key,
  mrn_mpi_mother                     int           not null,
  baby_mrn_epic_durable              int           not null,
  birth_index_within_mother          smallint,                             -- row_number per mother
  birth_datetime                     datetime,
  -- Pregnancy timeline:
  pregnancy_estimated_start_date     date,
  pregnancy_estimated_end_date       date,
  pregravid_weight                   numeric(18,1),
  pregravid_bmi                      numeric(18,1),
  labor_start_datetime               datetime,
  mother_age_years                   int,
  -- Delivery:
  delivery_method                    varchar(80),
  labor_type                         varchar(80),
  presentation_type                  varchar(80),
  placenta_method                    varchar(50),
  antenatal_steroids                 varchar(80),
  -- Gestational age:
  gestational_age_days               int,
  gestational_age_weeks              tinyint,
  gestational_age_days_remainder     tinyint,
  -- Multiple births:
  multiple_delivery_count            tinyint,
  multiple_delivery_order            tinyint,
  -- Infant measurements:
  birth_weight_grams                 numeric(10,3),
  birth_length                       numeric(8,3),
  head_circumference                 numeric(8,3),
  baby_discharge_weight              numeric(10,3),
  baby_inpatient_length_of_stay_days smallint,
  -- APGAR scores:
  total_apgar_one_minute             tinyint,
  total_apgar_five_minute            tinyint,
  total_apgar_ten_minute             tinyint,
  -- Flags:
  breast_milk_given                  bit,
  spontaneous_vaginal_delivery       bit,
  labor_attempted                    bit,
);

INSERT {project_schema}.birth_epic
SELECT distinct
  na.mrn_mpi                              as mrn_mpi_mother
  ,b.baby_mrn_epic_durable
  ,row_number() over (
    partition by na.mrn_mpi
    order by b.birth_datetime
    )                                      as birth_index_within_mother
  ,b.birth_datetime
  ,b.pregnancy_estimated_start_date
  ,b.pregnancy_estimated_end_date
  ,b.pregravid_weight
  ,b.pregravid_bmi
  ,b.labor_start_datetime
  ,b.mother_age_years
  ,b.delivery_method
  ,b.labor_type
  ,b.presentation_type
  ,b.placenta_method
  ,b.antenatal_steroids
  ,b.gestational_age_days
  ,b.gestational_age_weeks
  ,b.gestational_age_days_remainder
  ,b.multiple_delivery_count
  ,b.multiple_delivery_order
  ,b.birth_weight_grams
  ,b.birth_length
  ,b.head_circumference
  ,b.baby_discharge_weight
  ,b.baby_inpatient_length_of_stay_in_days
  ,b.total_apgar_one_minute
  ,b.total_apgar_five_minute
  ,b.total_apgar_ten_minute
  ,b.breast_milk_given
  ,b.spontaneous_vaginal_delivery
  ,b.labor_attempted
FROM cdw_epic.caboodle.birth b
  inner join cdw_mpi_1.groomed.node_assigned na   on b.mother_mrn_epic_durable = na.mrn_epic_durable
  inner join {project_schema}.pt_pool pp          on na.mrn_mpi = pp.mrn_mpi
WHERE b.birth_datetime between @date_start_epic and @date_stop
ORDER BY na.mrn_mpi, b.birth_datetime;

-- (N rows affected) HH:MM:SS
