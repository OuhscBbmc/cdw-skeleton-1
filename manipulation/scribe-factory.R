# knitr::stitch_rmd(script="manipulation/scribe-factory.R", output="stitched-output/manipulation/scribe-factory.md")
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
library(magrittr, quietly=TRUE)
library(rlang, quietly=TRUE)
requireNamespace("DBI")
requireNamespace("odbc")
requireNamespace("dplyr")
requireNamespace("testit")
requireNamespace("checkmate")
requireNamespace("OuhscMunge") # devtools::install_github(repo="OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config                         <- config::get()

# config %>%
#   purrr::map(~gsub("\\{project_name\\}", config$project_name, .))

if( config$project_name == "cdw-skeleton-1" ) {
  is_test                         <- TRUE
  config$schema_name              <- "main"               # `main` is the default schema in the (test) SQLite database
  config$path_directory_output    <- "data-public/derived"
  # config$schema_name  <- list("project_name" = "main") %>%
  #   glue::glue_data(config$schema_name) %>%
  #   as.character()
} else {
  is_test             <- FALSE
  config$schema_name  <- config$project_name
}

config <- config %>%
  rapply(object=., function(s) gsub("\\{project_name\\}", config$project_name, s), how="replace") %>%
  rapply(object=., function(s) gsub("\\{schema_name\\}", config$schema_name, s), how="replace") %>%
  rapply(object=., function(s) gsub("\\{path_directory_output\\}", config$path_directory_output, s), how="replace")


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
    sql             = dplyr::coalesce(.data$sql, .data$sql_constructed),


    sql_pretty      = gsub("^SELECT\\b"   , "<br/>SELECT<br/>  "    , sql),
    sql_pretty      = gsub("\\bFROM\\b"   , "<br/>FROM"           , sql_pretty),
    sql_pretty      = gsub("\\bWHERE\\b"  , "<br/>WHERE"          , sql_pretty),
    sql_pretty      = paste0("<pre><code>", sql_pretty, "</code></pre>"),

    path_output     = strftime(Sys.Date(), path_output)
  ) %>%
  dplyr::select(sql, path_output, row_unit, sql_pretty)

checkmate::assert_character(ds_table$sql          , min.chars=10, any.missing=F, unique=T)
checkmate::assert_character(ds_table$sql_pretty   , min.chars=10, any.missing=F, unique=T)
checkmate::assert_character(ds_table$path_output  , min.chars=10, any.missing=F, unique=T)

# ---- load-data ---------------------------------------------------------------
cnn <- if( is_test ) {
  DBI::dbConnect(drv=RSQLite::SQLite(), dbname=config$path_database)
} else {
  DBI::dbConnect(odbc::odbc(), config$dsn_staging)
}
# DBI::dbListTables(cnn)
ds_table$d <- ds_table$sql %>%
  purrr::map(., function(s) DBI::dbGetQuery(conn=cnn, statement = s))
# d           <- DBI::dbGetQuery(cnn, ds_table$sql[1])
DBI::dbDisconnect(cnn); rm(cnn)

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
  # dplyr::rowwise() %>%
  # dplyr::mutate(
  #   check_message     =  purrr::pmap_chr(.data$d, function(dd) checkmate::check_data_frame(dd, min.rows = 5))
  # ) %>%
  # dplyr::ungroup() %>%
  dplyr::mutate(
    pass                = (message_check == "TRUE"),
    message_check       = dplyr::if_else(message_check == "TRUE", "Pass", message_check),
    message_dimensions  = paste("Table dim (col x row):", .data$col_count,  "x", .data$row_count, " -", .data$table_size)
  )
# ds_table$check_message

# ---- inspect -----------------------------------------------------------------
ds_table %>%
  dplyr::select(message_check, sql, path_output, message_dimensions) %>%
  dplyr::mutate(
    path_output         = gsub("/", "/<br/>", path_output),
    sql                 = gsub("^SELECT\\b", "SELECT<br/>  ", sql),
    sql                 = gsub("\\bFROM\\b", "<br/>FROM", sql),
    sql                 = paste0("<br/>", sql, "<br/>"),
    message_dimensions  = sub("Table dim \\(c x r\\): ", "", message_dimensions)
  ) %>%
  knitr::kable(
    col.names = gsub("_", "<br/>", colnames(.)),
    format    = "markdown"
  )

ds_table %>%
  dplyr::select(sql, message_check, message_dimensions) %>%
  dplyr::transmute(
    message_augmented = paste("\n--------", sql, message_check, message_dimensions, sep="\n")
  ) %>%
  purrr::walk(~message(.))

if( !purrr::every(ds_table$pass, isTRUE) ) {
  stop(sum(!ds_table$pass), " out of ", nrow(ds_table), " tables failed.")
}


# ---- details -----------------------------------------------------------------

table_detail <-
  ds_table %>%
  dplyr::select(
    path_output,
    row_unit,
    dimensions    = message_dimensions,
    check         = message_check,
    sql           = sql_pretty
  ) %>%
  purrr::transpose() %>%
  # purrr::map_df(tibble::as_tibble) %>%
  yaml::as.yaml() %>%
  gsub("\\b(path_output|row_unit|dimensions|check|sql)\\b", "<br/><b>\\1</b>", .)

# table_detail %>%
#   cat()

# ---- slim-table --------------------------------------------------------------
ds_table_slim <-
  ds_table %>%
  dplyr::select(pass, path_output, sql, message_check, message_dimensions)

# ---- message -----------------------------------------------------------------
description_template <- paste0(
  "Project: `%s`\n",
  "============================\n\n",
  "Data Extracts from the BBMC CDW\n\n",
  "%i datasets were derived from the CDW and saved as separate csvs.\n",
  "The collection of datasets is described in the file `%s`\n",
  "which can be opened in Excel, Notepad++, or any program that can read plain text.\n\n",
  "The datasets were saved by %s at %s.\n\n",
  "%s\n\n",
  "%s\n\n"
)

description <- sprintf(
  description_template,
  config$project_name,
  nrow(ds_table),
  basename(config$path_output_summary),
  whoami::fullname(),
  # whoami::email_address(),
  Sys.time(),
  ds_table_slim %>%
    dplyr::select(pass, path_output) %>%
    knitr::kable() %>%
    paste(collapse="\n"),
  table_detail
)


# ---- verify-values -----------------------------------------------------------
# OuhscMunge::verify_value_headstart(ds_county)

# ---- specify-columns-to-upload -----------------------------------------------

# ---- save-to-disk ------------------------------------------------------------
# Create directories (typically just one directory).
directories <- ds_table$path_output %>%
  dirname() %>%
  unique() %>%
  sort()

directories %>%
  purrr::discard(dir.exists) %>%
  purrr::walk(., ~dir.create(., recursive = T))

# Save the real datasets.
ds_table %>%
  dplyr::select(d, path_output) %>%
  purrr::pwalk(.f=~readr::write_csv(x = .x, path=.y))

# Save the CSV summarizing the datasets.
ds_table_slim %>%
  readr::write_csv(config$path_output_summary)

# Save the description file.
description %>%
  readr::write_file(config$path_output_description)

rmarkdown::render(config$path_output_description)
