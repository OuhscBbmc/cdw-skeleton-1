rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
# ---- load-sources ------------------------------------------------------------
# source("./manipulation/osdh/ellis/common-ellis.R")

# ---- load-packages ----------------------------------------------------------------
requireNamespace("DBI")
library(magrittr, quietly=TRUE)
requireNamespace("dplyr") #devtools::install_github("hadley/dplyr")
requireNamespace("lubridate")
requireNamespace("config")
requireNamespace("OuhscMunge") #devtools::install_github(repo="OuhscBbmc/OuhscMunge")

# ---- declare-globals --------------------------------------------------------------
config           <- config::get()
l_data           <- list()
project_name     <- basename(getwd())
pattern_sql      <- "^(.+)\\.sql$"
pattern_sql_date <- "^(.+)(-\\d{4}-\\d{2}-\\d{2}-\\d{4}\\.sql)$"
pattern_csv      <- "^(.+)(-\\d{4}-\\d{2}-\\d{2}-\\d{4}\\.csv)$"
fs_path_out         <- glue::glue(config$fs_csv_report,project_name = project_name)
fs_path_archive     <- file.path(fs_path_out,"archive")
fs_files_to_archive <- c()
gt_files_to_archive <- c()
gt_path_archive     <- file.path(config$gt_sql_csv_report_generated,"archive")

sql_files_to_read   <- list.files(path = config$gt_sql_queries_to_csv, full.names=T, pattern=".*.sql")
#testit::assert("There shouldbe at least 20 SQL files to execute.", 20 < length(sql_files_to_read))

ds_file <- tibble::tibble(
  path_sql     = sql_files_to_read
)

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

for(input_file in basename(ds_file$path_sql)){
  for(output_file in list.files(fs_path_out,pattern = "*.csv")){
    if (sub(pattern_sql,"\\1",input_file) == sub(pattern_csv,"\\1",output_file))
    {
      fs_files_to_archive <- append(fs_files_to_archive,output_file)
    }
  }
}

for(input_file in basename(ds_file$path_sql)){
  for(output_file in list.files(config$gt_sql_csv_report_generated,pattern = "*.sql")){
    if (sub(pattern_sql,"\\1",input_file) == sub(pattern_sql_date,"\\1",output_file))
    {
      gt_files_to_archive <- append(gt_files_to_archive,output_file)
    }
  }
}

# ---- load-data ----------------------------------------------------------------
ds_file <- ds_file %>%
  dplyr::mutate(
    file_name_in      = basename(path_sql),
    file_name_out     = paste(sub("^(.+)(\\.sql)","\\1",file_name_in),strftime(Sys.time(), "%Y-%m-%d-%H%M.csv"),sep = '-'),
    sql               = purrr::map_chr(path_sql, readr::read_file),
    path_output       = file.path(fs_path_out, file_name_out)
  )

# system.time(gcFirst=T, {
channel               <- DBI::dbConnect(odbc::odbc(),config$dsn_staging)
for( i in seq_len(nrow(ds_file)) ) {
  message("Reading ", ds_file$file_name_in[[i]])
  l_data[[i]] <- DBI::dbGetQuery(channel, ds_file$sql[[i]])

  OuhscMunge::verify_data_frame(l_data[[i]], minimum_row_count=1)

  l_data[[i]] <- tibble::as_tibble(l_data[[i]])
}
DBI::dbDisconnect(channel); rm(channel)
# })



# ---- tweak-data ----------------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds_county_month_indicators)
# ds_lu_program <- ds_lu_program %>%
#   tibble::as_tibble() %>%
#   dplyr::mutate(
#     model_name  = classify_interbred(model_name)
#   )

# ---- verify-values -----------------------------------------------------------


# ---- specify-columns-to-upload -----------------------------------------------
# dput(colnames(ds_program_month))

# ---- archive-file -----------------------------------------------------------
if(!is.null(fs_files_to_archive)){

  for(file_name in fs_files_to_archive)
  {
    archive_file(file.path(fs_path_out,file_name),fs_path_archive)

  }
}


for(file_name in basename(sql_files_to_read))
{
  input_file_path  <- file.path(config$gt_sql_queries_to_csv,file_name)
  output_file_path <- file.path(config$gt_sql_csv_report_generated,file_name)
  archive_file(input_file_path,output_file_path)
}


# ---- save-to-disk ----------------------------------------------------------------
#This gets written to the disk, but not the repository
# readr::write_csv(ds_model             , path=file.path(path_cache_local, "eto-model.csv"))
# readr::write_csv(ds_program           , path=file.path(path_cache_local, "eto-program.csv"))


list(
  x      = l_data,
  path   = ds_file$path_output
) %>%
  purrr::pwalk(readr::write_csv)

#readr::write_csv(ds_slim     , path=file.path(path_cache_local, "eto-program-month.csv"))
