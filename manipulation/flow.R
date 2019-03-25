rm(list = ls(all = TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
library("magrittr")
requireNamespace("dplyr")
requireNamespace("readr")
requireNamespace("checkmate")
requireNamespace("config")
requireNamespace("tidyr")

# ---- declare-globals ---------------------------------------------------------
config <- config::get(file = "config.yml")

