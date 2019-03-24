rm(list =ls(all = TRUE)) #Clear the variables from previous runs.

# ---- load-sources ------------------------------------------------------------
# source("./manipulation/osdh/ellis/common-ellis.R")

requireNamespace("DBI")
library(magrittr,quietly = TRUE)
requireNamespace("dplyr")
requireNamespace("OuhscMunge")
requireNamespace("config")

# ---- declare-globals --------------------------------------------------------------
config           <- config::get()



