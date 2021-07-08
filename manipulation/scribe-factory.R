# knitr::stitch_rmd(script="manipulation/scribe-factory.R", output="stitched-output/manipulation/scribe-factory.md")
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
import::from("magrittr", "%>%")
requireNamespace("rlang")
requireNamespace("dplyr")
requireNamespace("odbc")
requireNamespace("checkmate")
requireNamespace("OuhscMunge") # remotes::install_github(repo="OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config                          <- config::get()

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
  # config$schema_name  <- config$project_name
}

read_file_sql <- function (path) {
  if (!is.na(path)) {
    readr::read_file(path)
  } else {
    NA_character_
  }
}
required_columns <- c(
  sql             = NA_character_,
  path_sql        = NA_character_,
  columns_include = NA_character_
)

if (is.null(config$tables_to_scribe))
  stop("The `tables_to_scribe` entry is not found in the config file.")

ds_table <-
  config$tables_to_scribe %>%
  purrr::map_df(tibble::as_tibble) %>%
  dplyr::mutate(
    project_name     = config$schema_name
  ) %>%
  tibble::add_column( # https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist
    .data = .,
    !!!required_columns[setdiff(names(required_columns), colnames(.))]
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    # sql             = gsub("\\{project_name\\}", .data$schema_name, sql)
    sql             = as.character(glue::glue_data(list("project_name" = .data$project_name), .data$sql))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    file_name       = fs::path_file(path_output),
    # file_name       = paste0("**", fs::path_file(path_output), "**"),
    sql_file        = purrr::map_chr(.data$path_sql, read_file_sql),
    sql_constructed = as.character(glue::glue_data(., "SELECT {columns_include} FROM {project_name}.{name}")),
    sql             = dplyr::na_if(sql, ""),
    sql             = dplyr::na_if(sql, "NA"),
    sql             = dplyr::coalesce(.data$sql, .data$sql_file, .data$sql_constructed),

    sql_pretty      = gsub("^SELECT\\b"   , "SELECT<br/>  "       , sql),
    sql_pretty      = gsub("\\bFROM\\b"   , "<br/>FROM"           , sql_pretty),
    sql_pretty      = gsub("\\bWHERE\\b"  , "<br/>WHERE"          , sql_pretty),
    sql_pretty      = paste0("<pre><code>", sql_pretty, "</code></pre>"),

    path_output     = strftime(Sys.Date(), path_output),
  ) %>%
  dplyr::select(file_name, sql, path_output, row_unit, sql_pretty)

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
ds_table$column_names <- ds_table$d %>%
  purrr::map(~paste(colnames(.), collapse = ", "))

# paste(colnames(ds_table$d[[2]]), collapse = ", ")

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
    file_name,
    row_unit,
    path_output,
    dimensions    = message_dimensions,
    check         = message_check,
    column_names,
    sql           = sql_pretty,
  ) %>%
  dplyr::mutate(
    path_output   = paste0("<code>", path_output, "</code>")
  ) %>%
  purrr::transpose() %>%
  # purrr::map_df(tibble::as_tibble) %>%
  yaml::as.yaml() %>%
  gsub("\\bfile_name: (.+?)\\n", "<br/><h3>\\1</h3>", .) %>%
  gsub("\\b(path_output|row_unit|dimensions|check|sql|column_names)\\b", "<br/><b>\\1</b>", .)

# table_detail %>%
#   cat()

# ---- slim-table --------------------------------------------------------------
ds_table_slim <-
  ds_table %>%
  dplyr::select(pass, path_output, sql, message_check, message_dimensions)

# ---- message -----------------------------------------------------------------
description_template <- paste0(
description_template <- paste0(
  "---\ntitle: %s Extracts\n\n---\n\n",
  "Project: `%s`\n",
  "============================\n\n",
  "Data Extracts from the BBMC CRDW\n\n",
  "%i datasets were derived from the CRDW and saved as separate csvs.\n",
  "The collection of datasets is described in the file `%s`\n",
  "which can be opened in Excel, Notepad++, or any program that can read plain text.\n\n",
  "%s\n\n",
  "The datasets were saved by %s at %s.\n\n",
  "%s\n\n",
  "%s\n\n",
  "This work was made possible by the NIH grant U54GM104938 -[ (Oklahoma Shared Clinical and Translational Resource)](http://osctr.ouhsc.edu). Because our continued existence depends partly on productivity in research dissemination, when producing articles and presentations that utilize these data, please include this grant number in your acknowledgements.\n\n
   Our suggested acknowledgement:  'Data for this research were provided by the University of Oklahoma Health Sciences Center Clinical Research Data Warehouse (http://ouhsc.edu/bbmc/crdw), whose work is made possible by NIH grant U54GM104938.'\n"
)

security_warning <-
  paste(
    "<b>Data Security</b>:",
    "These files have been delivered to you (the researchers) in a secure environment.",
    "You are responsible for maintaining security of the PHI.",
    "By receiving this dataset, you agree to follow all OU regulations to protect the data security.",
    "If you have questions regarding best practices, please contact the CRDW team or Campus IT."
  )

description <- sprintf(
  description_template,
  config$project_name,
  config$project_name,
  nrow(ds_table),
  basename(config$path_output_summary),
  security_warning,
  whoami::fullname(),
  # whoami::email_address(),
  Sys.time(),
  ds_table_slim %>%
    dplyr::mutate(
      file = paste0("**", fs::path_file(path_output), "**")
    ) %>%
    dplyr::select(file, pass) %>%
    knitr::kable() %>%
    paste(collapse = "\n"),
  table_detail
)

# ---- verify-values -----------------------------------------------------------
# OuhscMunge::verify_value_headstart(ds_county)

# ---- specify-columns-to-upload -----------------------------------------------

# ---- save-to-disk ------------------------------------------------------------
# Create directories (typically just one directory).
directories <-
  ds_table$path_output %>%
  c(
    .,                                # All the paths for the exported/scribed datasets
    config$path_output_summary,       # The summary file
    config$path_output_description    # The description file
  ) %>%
  dirname() %>%
  unique() %>%
  sort()

directories %>%
  purrr::discard(dir.exists) %>%
  purrr::walk(., ~dir.create(., recursive = T))

# Save the real datasets.
# ds_table %>%
#   dplyr::select(d, path_output) %>%
#   purrr::pwalk(.f=~readr::write_csv(x = .x, file=.y))

ds_table %>%
  dplyr::select(d, path_output) %>%
  dplyr::filter(!(fs:path_ext(path_output) %in% c("sas7bdat", "sav"))) %>%
  purrr::pwalk(.f = ~readr::write_csv(.x, .y, na=''))

ds_table %>%
  dplyr::select(d, path_output) %>%
  dplyr::filter(fs:path_ext(path_output) == "sav") %>%
  purrr::pwalk(.f = ~haven::write_spss(.x, .y))

ds_table %>%
  dplyr::select(d, path_output) %>%
  dplyr::filter(fs:path_ext(path_output) == "sas7bdat") %>%
  purrr::pwalk(.f = ~haven::write_sas(.x, .y))

# Save datasets as .sav for SPSS (note: file extensions in config file must end in '.sav')
#ds_table %>%
#  dplyr::select(d, path_output) %>%
#  purrr::pwalk(.f=~haven::write_sav(.x, .y))

# Save datasets as .sas7bdat for SAS (note: file extensions in config file must end in 'sas7bdat')
# ds_table %>%
#  dplyr::select(d, path_output) %>%
#  purrr::pwalk(.f=~haven::write_sas(.x, .y))

# Save the CSV summarizing the datasets.
ds_table_slim %>%
  readr::write_csv(config$path_output_summary)

# Save the description file.
description %>%
  readr::write_file(config$path_output_description)

# Render markdown summary as html
#    Takes extra steps to avoid using network drive.  https://community.rstudio.com/t/fail-to-generate-file-in-rmarkdwon-openbinaryfile-does-not-exist-no-such-file-or-directory/34913/3
path_temp_md    <- fs::file_temp(ext = "md")
path_temp_html  <- fs::file_temp(ext = "html")

fs::file_copy(
  path        = config$path_output_description,
  new_path    = path_temp_md,
  overwrite   = TRUE
)

rmarkdown::render(
  input       = path_temp_md,
  output_file = path_temp_html
)
fs::file_delete(path_temp_md)

fs::file_copy(
  path      = path_temp_html,
  new_path  = fs::path_ext_set(config$path_output_description, "html"),
  overwrite = TRUE
)
fs::file_delete(path_temp_html)
