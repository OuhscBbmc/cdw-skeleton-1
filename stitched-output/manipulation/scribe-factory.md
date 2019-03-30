



This report was automatically generated with the R package **knitr**
(version 1.22).


```r
# knitr::stitch_rmd(script="manipulation/scribe-factory.R", output="stitched-output/manipulation/scribe-factory.md")
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
```


```r
library(magrittr, quietly=TRUE)
library(rlang, quietly=TRUE)
requireNamespace("DBI")
requireNamespace("odbc")
requireNamespace("dplyr")
requireNamespace("testit")
requireNamespace("checkmate")
requireNamespace("OuhscMunge") # devtools::install_github(repo="OuhscBbmc/OuhscMunge")
```

```r
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

    path_output     = strftime(Sys.Date(), path_output)
  ) %>%
  dplyr::select(sql, path_output)

checkmate::assert_character(ds_table$sql          , min.chars=10, any.missing=F, unique=T)
checkmate::assert_character(ds_table$path_output  , min.chars=10, any.missing=F, unique=T)
```

```r
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
```

```r
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
    message_dimensions  = paste("Table dim (c x r):", .data$col_count,  "x", .data$row_count, "-", .data$table_size)
  )
# ds_table$check_message
```

```r
ds_table %>%
  dplyr::select(message_check, sql, path_output, message_dimensions) %>%
  dplyr::mutate(
    path_output         = gsub("/", "/<br/>", path_output),
    sql                 = gsub("^SELECT\\b", "SELECT<br/>  ", sql),
    sql                 = gsub("\\bFROM\\b", "<br/>FROM", sql),
    message_dimensions  = sub("Table dim \\(c x r\\): ", "", message_dimensions)
  ) %>%
  knitr::kable(
    col.names = gsub("_", "<br/>", colnames(.)),
    format    = "markdown"
  )
```



|message<br/>check |sql                                                                          |path<br/>output                            |message<br/>dimensions |
|:-----------------|:----------------------------------------------------------------------------|:------------------------------------------|:----------------------|
|Pass              |SELECT<br/>   * <br/>FROM main.person                                        |data-public/<br/>derived/<br/>person.csv   |4 x 6 - 2.3 KiB        |
|Pass              |SELECT<br/>   mrn_flowcast, obs_date, obs_term, obs_value <br/>FROM main.obs |data-public/<br/>derived/<br/>obs.csv      |4 x 4 - 1.8 KiB        |
|Pass              |SELECT<br/>   * <br/>FROM main.medicate                                      |data-public/<br/>derived/<br/>medicate.csv |5 x 2 - 1.8 KiB        |

```r
ds_table %>%
  dplyr::select(sql, message_check, message_dimensions) %>%
  dplyr::transmute(
    message_augmented = paste("\n--------", sql, message_check, message_dimensions, sep="\n")
  ) %>%
  purrr::walk(~message(.))
```

```
## 
## --------
## SELECT * FROM main.person
## Pass
## Table dim (c x r): 4 x 6 - 2.3 KiB
## --------
## SELECT mrn_flowcast, obs_date, obs_term, obs_value FROM main.obs
## Pass
## Table dim (c x r): 4 x 4 - 1.8 KiB
## --------
## SELECT * FROM main.medicate
## Pass
## Table dim (c x r): 5 x 2 - 1.8 KiB
```

```r
if( !purrr::every(ds_table$pass, isTRUE) ) {
  stop(sum(!ds_table$pass), " out of ", nrow(ds_table), " tables failed.")
}
```

```r
ds_table_slim <-
  ds_table %>%
  dplyr::select(pass, path_output, sql, message_check, message_dimensions)
```

```r
message("TODO: write an automated text file every time that provides context to the researcher about what the files are.")
```

```
## TODO: write an automated text file every time that provides context to the researcher about what the files are.
```

```r
description_template <- paste0(
  "Project: `%s`\n",
  "============================\n\n",
  "Data Extracts from the BBMC CDW\n\n",
  "%i datasets were derived from the CDW and saved as separate csvs.\n",
  "The collection of datasets is described in the file `%s`\n",
  "which can be opened in Excel, Notepad++, or any program that can read plain text.\n\n",
  "The datasets were saved by %s at %s.\n\n",
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
  paste(knitr::kable(ds_table_slim), collapse="\n")
)
```

```r
# OuhscMunge::verify_value_headstart(ds_county)
```


```r
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
```

```
## /usr/lib/rstudio/bin/pandoc/pandoc +RTS -K512m -RTS _description.utf8.md --to html4 --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash+smart --output _description.html --email-obfuscation none --self-contained --standalone --section-divs --template /home/wibeasley/R/x86_64-pc-linux-gnu-library/3.5/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable 'theme:bootstrap' --include-in-header /tmp/Rtmp1VDLxe/rmarkdown-str59717f73483d.html --mathjax --variable 'mathjax-url:https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --metadata pagetitle=_description.utf8.md
```

```
## 
## Output created: _description.html
```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 3.5.3 (2019-03-11)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 18.04.2 LTS
## 
## Matrix products: default
## BLAS: /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] rlang_0.3.1  magrittr_1.5
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.0            pillar_1.3.1          compiler_3.5.3       
##  [4] highr_0.7             tools_3.5.3           odbc_1.1.6           
##  [7] digest_0.6.18         packrat_0.5.0         bit_1.1-14           
## [10] jsonlite_1.6          evaluate_0.13         RSQLite_2.1.1        
## [13] memoise_1.1.0         tibble_2.0.1          checkmate_1.9.1      
## [16] pkgconfig_2.0.2       whoami_1.2.0          cli_1.0.1            
## [19] DBI_1.0.0             curl_3.3              yaml_2.2.0           
## [22] xfun_0.5              httr_1.4.0            dplyr_0.8.0.1        
## [25] stringr_1.4.0         knitr_1.22            hms_0.4.2.9001       
## [28] bit64_0.9-7           tidyselect_0.2.5      glue_1.3.0           
## [31] OuhscMunge_0.1.9.9010 R6_2.4.0              fansi_0.4.0          
## [34] rmarkdown_1.11        purrr_0.3.1           readr_1.3.1          
## [37] blob_1.1.1            htmltools_0.3.6       ps_1.3.0             
## [40] backports_1.1.3       assertthat_0.2.0      testit_0.9.1         
## [43] config_0.3            utf8_1.1.4            stringi_1.3.1        
## [46] markdown_0.9          crayon_1.3.4
```

```r
Sys.time()
```

```
## [1] "2019-03-30 11:45:51 CDT"
```

