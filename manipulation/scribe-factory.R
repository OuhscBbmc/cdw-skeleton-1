# knitr::stitch_rmd(script="manipulation/mlm-scribe.R", output="stitched-output/manipulation/mlm-scribe.md")
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.


# ---- load-sources ------------------------------------------------------------
# source("manipulation/osdh/ellis/common-ellis.R")
# base::source(file="dal/osdh/arch/benchmark-client-program-arch.R") #Load retrieve_benchmark_client_program

# ---- load-packages -----------------------------------------------------------
library(magrittr, quietly=TRUE)
library(rlang, quietly=TRUE)
requireNamespace("DBI")
requireNamespace("odbc")
requireNamespace("dplyr")
requireNamespace("testit")
requireNamespace("lubridate")
requireNamespace("RcppRoll")
requireNamespace("OuhscMunge") # devtools::install_github(repo="OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config                         <- config::get()#file="./repo/config.yml")
path_db                        <- config$path_database

# is_test                        <- (config$schema_name == "{project_name}")
if( config$schema_name == "{project_name}" ) {
  # `main` is the default schema in the (test) SQLite database
  config$schema_name  <- list("project_name" = "main") %>%
    glue::glue_data(config$schema_name) %>%
    as.character()
}

# glue::glue_data(list("schema_name" = config$schema_name), config$tables_to_scribe[[1]]$sql)
#
# class(config$tables_to_scribe[[1]]$sql)
# config$tables %>%
#   purrr::map_chr("path_output")

ds_table <-
  config$tables %>%
  purrr::map_df(tibble::as_tibble) %>%
  dplyr::mutate(
    project_name     = config$schema_name
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    # sql             = gsub("\\{project_name\\}", .data$schema_name, sql)
    sql             = as.character(glue::glue_data(list("project_name" = .data$project_name), .data$sql))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    sql_constructed = as.character(glue::glue_data(., "SELECT {columns_include} FROM {project_name}.{name}")),
    sql             = dplyr::na_if(sql, ""),
    sql             = dplyr::na_if(sql, "NA"),
    sql             = dplyr::coalesce(.data$sql, .data$sql_constructed)

    # sql             = dplyr::if_else(is_test, gsub("\\{project_name\\}.", "", sql))
  ) %>%
  dplyr::select(sql,  path_output)

checkmate::assert_character(ds_table$sql          , min.chars=10, any.missing=F, unique=T)
checkmate::assert_character(ds_table$path_output  , min.chars=10, any.missing=F, unique=T)


# ---- load-data ---------------------------------------------------------------
# ds_lu_program   <- retrieve_program()
cnn <- DBI::dbConnect(drv=RSQLite::SQLite(), dbname=path_db)
# DBI::dbListTables(cnn)
ds_table$d <- ds_table$sql %>%
  purrr::map(., function(s) DBI::dbGetQuery(conn=cnn, statement = s))
# d           <- DBI::dbGetQuery(cnn, ds_table$sql[1])
DBI::dbDisconnect(cnn); rm(cnn)


# ds_table$d[[1]] %>%
#   purrr::map(~checkmate::assert_data_frame(., min.rows = 1))


# ---- tweak-data --------------------------------------------------------------

ds_table$message_check <- ds_table$d %>%
  purrr::map_chr(., ~checkmate::check_data_frame(., min.rows = 1))
ds_table$row_count <- ds_table$d %>%
  purrr::map_int(nrow)
ds_table$col_count <- ds_table$d %>%
  purrr::map_int(ncol)
ds_table$table_size <- ds_table$d %>%
  purrr::map_chr(~format(object.size(.), units="KiB"))

ds_table <-
  ds_table %>%
  dplyr::mutate(
    # check_message =  purrr::map_chr(.data$d, ~checkmate::check_data_frame(.data$d, min.rows = 5)),
    pass                = (message_check == "TRUE"),
    message_check       = dplyr::if_else(message_check == "TRUE", "Pass", message_check),
    message_dimensions  = paste("Table dim (c x r):", .data$col_count,  "x", .data$row_count, "-", .data$table_size)
  )

# ---- inspect -----------------------------------------------------------------
ds_table %>%
  dplyr::select(sql, message_check, message_dimensions) %>%
  dplyr::transmute(
    message_augmented = paste("\n--------", sql, message_check, message_dimensions, sep="\n")
  ) %>%
  # dplyr::pull()
  # ds_table$check %>%
    purrr::walk(~message(.))

if( !purrr::every(ds_table$pass, isTRUE) ) {
  stop(sum(!ds_table$pass), " out of ", nrow(ds_table), " tables failed.")
}


# ---- verify-values -----------------------------------------------------------
# OuhscMunge::verify_value_headstart(ds_county)

# ---- specify-columns-to-upload -----------------------------------------------

# ---- save-to-disk ------------------------------------------------------------
ds_table %>%
  dplyr::select(d, path_output) %>%
  purrr::pwalk(.f=~readr::write_csv(x = .x, path=.y))


  # purrr::walk2(.x=.data$d, .data$path_output, .f=~readr::write_csv(x = .x, path=.y))
