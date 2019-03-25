post_sweep_and_specify <- function(in_path, col_types, dsn, ss_type, schema_name, table_name, columns_to_write, archive_path, project){

    # ---- load-data ---------------------------------------------------------------
    file_name <- list.files(in_path,pattern = "*.csv")
    in_path   <- file.path(in_path,file_name)
    ds_entry  <- read_file(in_path,col_types)

    # ---- tweak-data --------------------------------------------------------------
    # OuhscMunge::column_rename_headstart(ds) #Spit out columns to help write call at `dplyr::rename()`.
    ds <- ds_entry %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        project = project
      ) %>%
      dplyr::filter(desired)

    # ---- verify-ds -----------------------------------------------------------
    verify_ds(ds,ss_type)

    # ---- specify-columns-to-upload -----------------------------------------------
    ds_slim <- ds %>%
      dplyr::select(!!columns_to_write) %>%
      tibble::rowid_to_column("id")

    # ---- upload-to-db ----------------------------------------------------------
    write_db(ds_slim,schema_name,table_name,dsn)

    file_name <- gsub("-\\d+\\.csv$","",file_name )

    file_to_archive <- paste("to-database",file_name,strftime(Sys.time(),"%Y%m%d%H%M%S.csv"),sep = '-')

    # file_to_archive <- glue::glue(file_name_archive,cdw_project = project_name)

    path_to_archive <- file.path(archive_path,file_to_archive)

    archive_file(in_path,path_to_archive)
}
