



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
message("TODO: write an automated text file every time that provides context to the researcher about what the files are.")
```

```
## TODO: write an automated text file every time that provides context to the researcher about what the files are.
```

```r
# OuhscMunge::verify_value_headstart(ds_county)
```


```r
directories <- ds_table$path_output %>%
  dirname() %>%
  unique() %>%
  sort()

directories %>%
  purrr::discard(dir.exists) %>%
  purrr::walk(., ~dir.create(., recursive = F))

ds_table %>%
  dplyr::select(d, path_output) %>%
  purrr::pwalk(.f=~readr::write_csv(x = .x, path=.y))

ds_table %>%
  dplyr::select(pass, path_output, sql, message_check, message_dimensions) %>%
  readr::write_csv(config$path_output_summary)
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
## [10] evaluate_0.13         RSQLite_2.1.1         memoise_1.1.0        
## [13] tibble_2.0.1          checkmate_1.9.1       pkgconfig_2.0.2      
## [16] cli_1.0.1             DBI_1.0.0             yaml_2.2.0           
## [19] xfun_0.5              dplyr_0.8.0.1         stringr_1.4.0        
## [22] knitr_1.22            hms_0.4.2.9001        bit64_0.9-7          
## [25] tidyselect_0.2.5      glue_1.3.0            OuhscMunge_0.1.9.9010
## [28] R6_2.4.0              fansi_0.4.0           purrr_0.3.1          
## [31] readr_1.3.1           blob_1.1.1            backports_1.1.3      
## [34] assertthat_0.2.0      testit_0.9.1          config_0.3           
## [37] utf8_1.1.4            stringi_1.3.1         markdown_0.9         
## [40] crayon_1.3.4
```

```r
Sys.time()
```

```
## [1] "2019-03-28 09:07:27 CDT"
```
