rm(list = ls(all = TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
source("manipulation/sweep-and-specify/sweep-specify-common.R")
source("manipulation/sweep-and-specify/get-sweep-and-specify.R")
source("manipulation/sweep-and-specify/verify-ds-ferry.R")

# ---- load-packages -----------------------------------------------------------
library("magrittr")
requireNamespace("dplyr")
requireNamespace("readr")
requireNamespace("checkmate")
requireNamespace("tidyr")
requireNamespace("DBI")
requireNamespace("OuhscMunge")

ds <- tibble::tribble(

    ~in_path,                              ~col_types,                    ~sql_path,                       ~dsn,          ~review_required,      ~ss_type,                ~out_path,                 ~archive_path,

    in_rw_req_centricity_diagnosis,     col_types_review_required,   sql_rw_req_centricity_diagnosis,   dsn_centricity,       TRUE,         "centricity_diagnosis",     rw_centricity_diagnosis,          archive_centricity,
    in_rw_req_centricity_location,      col_types_review_required,   sql_rw_req_centricity_location,    dsn_centricity,       TRUE,         "centricity_location",      rw_centricity_location,           archive_centricity,
    in_rw_req_centricity_medication,    col_types_review_required,   sql_rw_req_centricity_medication,  dsn_centricity,       TRUE,         "centricity_medication",    rw_centricity_medication,         archive_centricity,
    in_rw_req_centricity_obs,           col_types_review_required,   sql_rw_req_centricity_obs,         dsn_centricity,       TRUE,         "centricity_obs",           rw_centricity_obs,                archive_centricity,
    in_rw_req_idx_cpt,                  col_types_review_required,   sql_rw_req_idx_cpt,                dsn_idx,              TRUE,         "idx_cpt",                  rw_idx_cpt,                       archive_idx,
    in_rw_req_idx_diagnosis,            col_types_review_required,   sql_rw_req_idx_diagnosis,          dsn_idx,              TRUE,         "idx_diagnosis",            rw_idx_diagnosis,                 archive_idx,
    in_rw_req_idx_location,             col_types_review_required,   sql_rw_req_idx_location,           dsn_idx,              TRUE,         "idx_location",             rw_idx_location,                  archive_idx,
    in_rw_req_idx_sched_location,       col_types_review_required,   sql_rw_req_idx_sched_location,     dsn_idx,              TRUE,         "idx_sched_location",       rw_idx_sched_location,            archive_idx,
    in_rw_skip_centricity_diagnosis,    col_types_review_skip,       sql_rw_skip_centricity_diagnosis,  dsn_centricity,       FALSE,        "centricity_diagnosis",     rw_org_centricity_diagnosis,      archive_centricity,
    in_rw_skip_centricity_location,     col_types_review_skip,       sql_rw_skip_centricity_location,   dsn_centricity,       FALSE,        "centricity_location",      rw_org_centricity_location,       archive_centricity,
    in_rw_skip_centricity_medication,   col_types_review_skip,       sql_rw_skip_centricity_medication, dsn_centricity,       FALSE,        "centricity_medication",    rw_org_centricity_medication,     archive_centricity,
    in_rw_skip_centricity_obs,          col_types_review_skip,       sql_rw_skip_centricity_obs,        dsn_centricity,       FALSE,        "centricity_obs",           rw_org_centricity_obs,            archive_centricity,
    in_rw_skip_idx_cpt,                 col_types_review_skip,       sql_rw_skip_idx_cpt,               dsn_idx,              FALSE,        "idx_cpt",                  rw_org_idx_cpt,                   archive_idx,
    in_rw_skip_idx_diagnosis,           col_types_review_skip,       sql_rw_skip_idx_diagnosis,         dsn_idx,              FALSE,        "idx_diagnosis",            rw_org_idx_diagnosis,             archive_idx,
    in_rw_skip_idx_location,            col_types_review_skip,       sql_rw_skip_idx_location,          dsn_idx,              FALSE,        "idx_location",             rw_org_idx_location,              archive_idx,
    in_rw_skip_idx_sched_location,      col_types_review_skip,       sql_rw_skip_idx_sched_location,    dsn_idx,              FALSE,        "idx_sched_location",       rw_org_idx_sched_location,        archive_idx
  )



ds <- ds %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    check_file_count =   length(list.files(in_path,pattern = "*.csv"))
  )%>%
  dplyr::filter(as.logical(check_file_count)) %>%
  dplyr::mutate(
    project = project_name
  )


if(any(ds$check_file_count > 1 )){
  stop("There is more than one file in one of the input sweep and specify directory's")
}

if(!any(ds$check_file_count == 1))
{
  stop("There is no file in any of the input sweep and specify directory's")
}

ds %>%
  dplyr::select(-check_file_count) %>%
  purrr::pwalk(get_sweep_and_specify)
