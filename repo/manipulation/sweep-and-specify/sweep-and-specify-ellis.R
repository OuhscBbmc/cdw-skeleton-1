rm(list = ls(all = TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
source("manipulation/sweep-and-specify/sweep-specify-common.R")
source("manipulation/sweep-and-specify/post-sweep-and-specify.R")
source("manipulation/sweep-and-specify/verify-ds-ellis.R")


# ---- load-packages -----------------------------------------------------------
# Attach these package(s) so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr            , quietly                   =TRUE)

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"        )
requireNamespace("tidyr"        )
requireNamespace("dplyr"        ) # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns. # devtools::install_github("mllg/checkmate")
requireNamespace("OuhscMunge"   ) # devtools::install_github(repo ="OuhscBbmc/OuhscMunge")
requireNamespace("DBI")


ds <- tibble::tribble(
  ~in_path,                         ~col_types,                        ~dsn,            ~ss_type,               ~schema_name,                ~table_name,              ~columns_to_write,                          ~archive_path,

  to_db_centricity_diagnosis,      col_types_centricity_diagnosis,    dsn_staging,    "centricity_diagnosis",   "sweep_and_specify",         "centricity_diagnosis",      columns_to_write_centricity_diagnosis,      archive_centricity,
  to_db_centricity_location,       col_types_centricity_location,     dsn_staging,    "centricity_location",    "sweep_and_specify",         "centricity_location",       columns_to_write_centricity_location,       archive_centricity,
  to_db_centricity_medication,     col_type_centricity_medication,    dsn_staging,    "centricity_medication",  "sweep_and_specify",         "centricity_medication",     columns_to_write_centricity_medication,     archive_centricity,
  to_db_centricity_obs,            col_type_centricity_obs,           dsn_staging,    "centricity_obs",         "sweep_and_specify",         "centricity_obs",            columns_to_write_centricity_obs,            archive_centricity,
  to_db_idx_cpt,                   col_types_idx_cpt,                 dsn_staging,    "idx_cpt",                "sweep_and_specify",         "idx_dim_cpt",               columns_to_write_idx_cpt,                   archive_idx,
  to_db_idx_diagnosis,             col_types_idx_diagnosis,           dsn_staging,    "idx_diagnosis",          "sweep_and_specify",         "idx_dim_diagnosis",         columns_to_write_idx_diagnosis,             archive_idx,
  to_db_idx_location,              col_types_idx_location,            dsn_staging,    "idx_location",           "sweep_and_specify",         "idx_dim_location",          columns_to_write_idx_location,              archive_idx,
  to_db_idx_sched_location,        col_types_idx_sched_location,      dsn_staging,    "idx_sched_location",     "sweep_and_specify",         "idx_sched_location",        columns_to_write_idx_sched_location,        archive_idx
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
  stop("There is more than one file in one of the reviewed-to-database sweep and specify directory's")
}

if(!any(ds$check_file_count == 1))
{
  stop("There is no file in any of the reviewed-to-database sweep and specify directory's")
}

ds %>%
  dplyr::select(-check_file_count) %>%
  purrr::pwalk(post_sweep_and_specify)
