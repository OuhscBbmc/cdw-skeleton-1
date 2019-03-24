
# ---- load-packages -----------------------------------------------------------
requireNamespace("config")


# ---- declare-globals ---------------------------------------------------------
config <- config::get(file = "config.yml")

project_name     <- basename(getwd())

dsn_centricity  <- config$dsn_centricity
dsn_idx         <- config$dsn_idx
dsn_staging     <- config$dsn_staging


in_rw_req_centricity_diagnosis          <-  glue::glue(config$fs_input_review_required_centricity_diagnosis,  project_name = project_name)
in_rw_req_centricity_medication         <-  glue::glue(config$fs_input_review_required_centricity_medication, project_name = project_name)
in_rw_req_centricity_obs                <-  glue::glue(config$fs_input_review_required_centricity_obs,        project_name = project_name)
in_rw_req_centricity_location           <-  glue::glue(config$fs_input_review_required_centricity_location,   project_name = project_name)
in_rw_req_idx_location                  <-  glue::glue(config$fs_input_review_required_idx_location,          project_name = project_name)
in_rw_req_idx_sched_location            <-  glue::glue(config$fs_input_review_required_idx_sched_location,    project_name = project_name)
in_rw_req_idx_diagnosis                 <-  glue::glue(config$fs_input_review_required_idx_diagnosis,         project_name = project_name)
in_rw_req_idx_cpt                       <-  glue::glue(config$fs_input_review_required_idx_cpt,               project_name = project_name)
in_rw_skip_centricity_diagnosis         <-  glue::glue(config$fs_input_review_skip_centricity_diagnosis,      project_name = project_name)
in_rw_skip_centricity_medication        <-  glue::glue(config$fs_input_review_skip_centricity_medication,     project_name = project_name)
in_rw_skip_centricity_obs               <-  glue::glue(config$fs_input_review_skip_centricity_obs,            project_name = project_name)
in_rw_skip_centricity_location          <-  glue::glue(config$fs_input_review_skip_centricity_location,       project_name = project_name)
in_rw_skip_idx_location                 <-  glue::glue(config$fs_input_review_skip_idx_location,              project_name = project_name)
in_rw_skip_idx_sched_location           <-  glue::glue(config$fs_input_review_skip_idx_sched_location,        project_name = project_name)
in_rw_skip_idx_diagnosis                <-  glue::glue(config$fs_input_review_skip_idx_diagnosis,             project_name = project_name)
in_rw_skip_idx_cpt                      <-  glue::glue(config$fs_input_review_skip_idx_cpt,                   project_name = project_name)
rw_centricity_diagnosis                 <-  glue::glue(config$fs_review_centricity_diagnosis,                 project_name = project_name)
rw_centricity_medication                <-  glue::glue(config$fs_review_centricity_medication,                project_name = project_name)
rw_centricity_obs                       <-  glue::glue(config$fs_review_centricity_obs,                       project_name = project_name)
rw_centricity_location                  <-  glue::glue(config$fs_review_centricity_location,                  project_name = project_name)
rw_idx_location                         <-  glue::glue(config$fs_review_idx_location,                         project_name = project_name)
rw_idx_sched_location                   <-  glue::glue(config$fs_review_idx_sched_location,                   project_name = project_name)
rw_idx_diagnosis                        <-  glue::glue(config$fs_review_idx_diagnosis,                        project_name = project_name)
rw_idx_cpt                              <-  glue::glue(config$fs_review_idx_cpt,                              project_name = project_name)

rw_org_centricity_diagnosis             <-  glue::glue(config$fs_review_original_centricity_diagnosis,        project_name = project_name)
rw_org_centricity_medication            <-  glue::glue(config$fs_review_original_centricity_medication,       project_name = project_name)
rw_org_centricity_obs                   <-  glue::glue(config$fs_review_original_centricity_obs,              project_name = project_name)
rw_org_centricity_location              <-  glue::glue(config$fs_review_original_centricity_location,         project_name = project_name)
rw_org_idx_location                     <-  glue::glue(config$fs_review_original_idx_location,                project_name = project_name)
rw_org_idx_sched_location               <-  glue::glue(config$fs_review_original_idx_sched_location,          project_name = project_name)
rw_org_idx_diagnosis                    <-  glue::glue(config$fs_review_original_idx_diagnosis,               project_name = project_name)
rw_org_idx_cpt                          <-  glue::glue(config$fs_review_original_idx_cpt,                     project_name = project_name)

to_db_centricity_diagnosis              <-  glue::glue(config$fs_to_database_centricity_diagnosis,            project_name = project_name)
to_db_centricity_medication             <-  glue::glue(config$fs_to_database_centricity_medication,           project_name = project_name)
to_db_centricity_obs                    <-  glue::glue(config$fs_to_database_centricity_obs,                  project_name = project_name)
to_db_centricity_location               <-  glue::glue(config$fs_to_database_centricity_location,             project_name = project_name)
to_db_idx_location                      <-  glue::glue(config$fs_to_database_idx_location,                    project_name = project_name)
to_db_idx_sched_location                <-  glue::glue(config$fs_to_database_idx_sched_location,              project_name = project_name)
to_db_idx_diagnosis                     <-  glue::glue(config$fs_to_database_idx_diagnosis,                   project_name = project_name)
to_db_idx_cpt                           <-  glue::glue(config$fs_to_database_idx_cpt,                         project_name = project_name)

archive_centricity                      <-  glue::glue(config$fs_archive_centricity,                          project_name = project_name)
archive_idx                             <-  glue::glue(config$fs_archive_idx,                                 project_name = project_name)

sql_rw_req_centricity_diagnosis         <-  config$gt_sql_review_required_centricity_diagnosis
sql_rw_req_centricity_medication        <-  config$gt_sql_review_required_centricity_medication
sql_rw_req_centricity_obs               <-  config$gt_sql_review_required_centricity_obs
sql_rw_req_centricity_location          <-  config$gt_sql_review_required_centricity_location
sql_rw_req_idx_location                 <-  config$gt_sql_review_required_idx_location
sql_rw_req_idx_sched_location           <-  config$gt_sql_review_required_idx_sched_location
sql_rw_req_idx_diagnosis                <-  config$gt_sql_review_required_idx_diagnosis
sql_rw_req_idx_cpt                      <-  config$gt_sql_review_required_idx_cpt
sql_rw_skip_centricity_diagnosis        <-  config$gt_sql_review_skip_centricity_diagnosis
sql_rw_skip_centricity_medication       <-  config$gt_sql_review_skip_centricity_medication
sql_rw_skip_centricity_obs              <-  config$gt_sql_review_skip_centricity_obs
sql_rw_skip_centricity_location         <-  config$gt_sql_review_skip_centricity_location
sql_rw_skip_idx_location                <-  config$gt_sql_review_skip_idx_location
sql_rw_skip_idx_sched_location          <-  config$gt_sql_review_skip_idx_sched_location
sql_rw_skip_idx_diagnosis               <-  config$gt_sql_review_skip_idx_diagnosis
sql_rw_skip_idx_cpt                     <-  config$gt_sql_review_skip_idx_cpt

read_file <- function(input_file_path,col_types){
  return(readr::read_csv(input_file_path,col_types = col_types))
}

read_sql <- function(input_sql_path){
  return(readr::read_file(input_sql_path))
}

run_sql <- function(sql,dsn){
  system.time({
    channel     <- DBI::dbConnect(odbc::odbc(),dsn)
    ds          <- DBI::dbGetQuery(conn = channel, statement = sql)
    DBI::dbDisconnect(channel); rm(channel)
  })
  return(ds)
}

write_file <- function(ds,path_to_write){

  # ---- save-to-disk ------------------------------------------------------------

  # Temporarily using utils::write.csv() because readr::write_csv() doesn't currently force quotes,
  #    but it soon will: https://github.com/tidyverse/readr/issues/653
  #    Check in occasionally, and switch back when the readr feature is available.

  #readr::write_csv(ds, path_to_write)

  utils::write.csv(ds, path_to_write,quote=T, row.names=F)
}

archive_file <- function(input_file_path,path_to_archive){

  file_copy <- file.copy(
    from      = input_file_path,
    to        = path_to_archive,
    overwrite = FALSE,
    recursive = FALSE
  )

  if (file_copy){
    file.remove(input_file_path)
  }
}

write_db <- function(ds_slim,schema_name,table_name,dsn){

  OuhscMunge::upload_sqls_odbc(
    d             = ds_slim,
    schema_name   = schema_name,
    table_name    = table_name,
    dsn_name      = dsn,
    clear_table   = TRUE,
    create_table  = FALSE,
    convert_logical_to_integer = T
  )

}
