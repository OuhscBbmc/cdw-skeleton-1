# knitr::stitch_rmd(script="manipulation/mlm-scribe.R", output="stitched-output/manipulation/mlm-scribe.md")
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.


# ---- load-sources ------------------------------------------------------------
# source("manipulation/osdh/ellis/common-ellis.R")
# base::source(file="dal/osdh/arch/benchmark-client-program-arch.R") #Load retrieve_benchmark_client_program

# ---- load-packages -----------------------------------------------------------
library(magrittr, quietly=TRUE)
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

ds_table$check_message <- ds_table$d %>%
  purrr::map_chr(., ~checkmate::check_data_frame(., min.rows = 1))

ds_table <-
  ds_table %>%
  dplyr::mutate(
    # check_message =  purrr::map_chr(.data$d, ~checkmate::check_data_frame(.data$d, min.rows = 5)),
    pass  = (check_message == "TRUE"),
    check_message = dplyr::if_else(check_message == "TRUE", "Pass", check_message)
  )

# ---- inspect -----------------------------------------------------------------
ds_table %>%
  dplyr::select(sql, check_message) %>%
  dplyr::transmute(
    message_augmented = paste("\n--------", sql, check_message, sep="\n")
  ) %>%
  # dplyr::pull()
  # ds_table$check %>%
    purrr::walk(~message(.))

if( !purrr::every(ds_table$pass, isTRUE) ) {
  stop(sum(!ds_table$pass), " out of ", nrow(ds_table), " tables failed.")
}


cat(
  "Unique counties    : ", scales::comma(dplyr::n_distinct(ds_county_month$county_id)), "\n",
  "Unique months      : ", scales::comma(dplyr::n_distinct(ds_county_month$month    )), "\n",
  "Month range        : ", strftime(range(ds_county_month$month), "%Y-%m-%d  "), "\n",
  sep=""
)
ds_county_month %>%
  dplyr::count(county_id) %>%
  dplyr::mutate(n = scales::comma(n)) %>%
  tidyr::spread(county_id, n)

ds_county_month %>%
  # dplyr::filter(visit_all_completed_count > 0L) %>%
  # purrr::map(., ~mean(is.na(.)) ) %>%
  purrr::map(., ~mean(is.na(.) | as.character(.)=="Unknown")) %>%
  purrr::map(., ~round(., 3)) %>%
  tibble::as_tibble() %>%
  t()

# ---- verify-values -----------------------------------------------------------
# OuhscMunge::verify_value_headstart(ds_county)
checkmate::assert_integer(  ds_county$county_id , any.missing=F , lower=1, upper=77   , unique=T)
checkmate::assert_character(ds_county$county    , any.missing=F , pattern="^.{3,12}$" , unique=T)
checkmate::assert_numeric(  ds_county$fte       , any.missing=F , lower=0, upper=22   )
checkmate::assert_numeric(  ds_county$cog_1       , any.missing=T , lower=4, upper=6    )
checkmate::assert_numeric(  ds_county$cog_2       , any.missing=T , lower=5, upper=7    )
checkmate::assert_numeric(  ds_county$cog_3       , any.missing=T , lower=6, upper=9    )
checkmate::assert_numeric(  ds_county$phys_1      , any.missing=T , lower=2, upper=4    )
checkmate::assert_numeric(  ds_county$phys_2      , any.missing=T , lower=3, upper=5    )
checkmate::assert_numeric(  ds_county$phys_3      , any.missing=T , lower=1, upper=2    )


checkmate::assert_integer(  ds_county_month$county_id                   , any.missing=F , lower=1, upper=77                                        )
checkmate::assert_character(ds_county_month$county                      , any.missing=F , pattern="^.{3,12}$"                                      )
checkmate::assert_date(     ds_county_month$month                       , any.missing=F , lower=as.Date("2012-06-15"), upper=as.Date("2015-09-15") )
checkmate::assert_numeric(  ds_county_month$fte                         , any.missing=F , lower=0, upper=27                                        )
checkmate::assert_logical(  ds_county_month$fte_approximated            , any.missing=F                                                            )
checkmate::assert_logical(  ds_county_month$month_missing               , any.missing=F                                                            )
checkmate::assert_numeric(  ds_county_month$fte_rolling_median_11_month , any.missing=T , lower=0, upper=24                                        )

county_month_combo   <- paste(ds_county_month$county_id, ds_county_month$month)
checkmate::assert_character(county_month_combo, pattern  ="^\\d{1,2} \\d{4}-\\d{2}-\\d{2}$"            , any.missing=F, unique=T)

# ---- specify-columns-to-upload -----------------------------------------------
# dput(colnames(ds_county_month)) # Print colnames for line below.
columns_to_write_county_month <- c(
  "county_id", "county", "month", "fte", "fte_approximated",
  "month_missing", "fte_rolling_median_11_month"
)
ds_slim_county_month <-
  ds_county_month %>%
  # dplyr::slice(1:100) %>%
  dplyr::select(!!columns_to_write_county_month)
ds_slim_county_month

rm(columns_to_write_county_month)

# dput(colnames(ds_county)) # Print colnames for line below.
columns_to_write_county <- c(
  "county_id", "county", "fte",
  "cog_1_count",
  "cog_1", "cog_2", "cog_3",
  "phys_1", "phys_2", "phys_3"
)
ds_slim_county <-
  ds_county %>%
  # dplyr::slice(1:100) %>%
  dplyr::select(!!columns_to_write_county)
ds_slim_county

rm(columns_to_write_county)

# ---- save-to-disk ------------------------------------------------------------
readr::write_rds(ds_slim_county        , config$path_te_county           , compress="gz")
readr::write_rds(ds_slim_county_month  , config$path_te_county_month     , compress="gz")
