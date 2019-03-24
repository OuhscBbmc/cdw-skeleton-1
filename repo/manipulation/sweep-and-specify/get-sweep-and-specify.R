

get_sweep_and_specify <- function(in_path,col_types,sql_path,dsn,review_required,ss_type,out_path,archive_path,project){

  file_name                        <- list.files(in_path,pattern = "*.csv")
  in_path                          <- file.path(in_path,file_name)

  file_name_out                    <- gsub ("\\.csv$","",file_name )
  file_name_out_path               <- paste(file_name_out,                             strftime(Sys.time(), "{project}-%Y%m%d%H%M%S.csv"),sep = '-')
  file_name_unmatched_keywords     <- paste("unmatched-keywords",file_name_out_path,   strftime(Sys.time(), "{project}-%Y%m%d%H%M%S.csv"),sep = '-')
  file_name_archive                <- paste("input"             ,file_name_out_path,   strftime(Sys.time(), "{project}-%Y%m%d%H%M%S.csv"),sep = '-')
  file_to_write                    <- glue::glue(file_name_out_path,               cdw_project = project_name)
  file_to_write_unmatched_keywords <- glue::glue(file_name_unmatched_keywords,cdw_project = project_name)
  file_to_archive                  <- glue::glue(file_name_archive,           cdw_project = project_name)
  path_to_write                    <- file.path(out_path,                     file_to_write)
  path_to_write_unmatched_keywords <- file.path(out_path,                     file_to_write_unmatched_keywords)
  path_to_archive                  <- file.path(archive_path,                 file_to_archive)


  # ---- load-data ---------------------------------------------------------------
  ds_entry  <- read_file(in_path,col_types)
  sql       <- read_sql(sql_path)

  # ---- flatten-phylum ----------------------------------------------------------
  ds_keywords       <- ds_entry %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      project = project
    )
  rm(ds_entry)

  ds_sql  <-   ds_keywords %>%
    dplyr::mutate(
      sql_similar = glue::glue(sql)
    )

  rm(ds_keywords)
  rm(sql)

  #------ verify keywords ---------------------------------
  verify_keyword(ds_sql,review_required)

  ds <- run_sql(paste(ds_sql$sql_similar, collapse="\nUNION\n"),dsn)

  ds <- ds %>%
    dplyr::mutate(
      desired = as.logical(desired)
    ) %>%
    dplyr::distinct() %>%
    tibble::as_tibble()

  # ---- verify-ds -----------------------------------------------------------
  verify_ds(ds,ss_type)

  if(review_required)
  {
    unmatched_keyword <-  setdiff(unique(ds_sql$broad_match_keyword),unique(ds$match_keyword))
  }
  else
  {
    unmatched_keyword <-  setdiff(unique(ds_sql$exact_match_keyword),unique(ds$match_keyword))
  }

  # ---- save-to-disk ----------------------------------------------------------------
  write_file(ds,path_to_write)
  write_file(unmatched_keyword,path_to_write_unmatched_keywords)
  archive_file(in_path,path_to_archive)

}
