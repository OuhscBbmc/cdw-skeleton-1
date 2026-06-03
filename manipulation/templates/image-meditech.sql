-- ================================================================================================
-- TEMPLATE: image-meditech.sql
-- Source:   cdw_meditech.meditech.image  (radiology/imaging reports from Meditech)
-- Applies:  exam dates < 2023-06-03  (Meditech)
-- Purpose:  Radiology exam names and impression text from Meditech.
--           Join is via account_number from a prior visit pool.
--
-- !! CUSTOMIZE {project_schema}
-- !! CUSTOMIZE the visit pool join — default joins via account_number from visit_pool
-- ================================================================================================

use cdw_cache_staging;

declare @date_start   date = '{date_start}';
declare @date_stop    date = '2023-06-02';


DROP TABLE IF EXISTS [cdw_cache_staging].[{project_schema}].[image_meditech];
CREATE TABLE [cdw_cache_staging].[{project_schema}].[image_meditech] (
    image_meditech_index    int             identity(1,1) primary key,
    account_number          char(12)        not null,
    mrn_mpi                 int             not null,
    exam_date               date            not null,
    exam_name               varchar(30)     null,
    impression              varchar(max)    null,
);

INSERT INTO {project_schema}.image_meditech
SELECT
    i.account_number
    ,na.mrn_mpi
    ,i.exam_date
    ,i.exam_name
    ,i.impression
FROM cdw_meditech.meditech.image i
inner join cdw_mpi_1.groomed.node_assigned na   on i.mrn_meditech_internal = na.mrn_meditech_internal
inner join {project_schema}.pt_pool pp          on na.mrn_mpi = pp.mrn_mpi
WHERE i.exam_date between @date_start and @date_stop
-- !! Optionally limit to accounts from a visit pool:
-- and i.account_number in (SELECT v.account_number FROM {project_schema}.visit_pool v)
-- !! Or filter by exam name:
-- and i.exam_name like '%mri%'
ORDER BY na.mrn_mpi, i.exam_date;

-- (N rows affected) HH:MM:SS
