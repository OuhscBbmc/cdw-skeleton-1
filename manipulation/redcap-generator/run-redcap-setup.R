rm(list = ls(all.names = TRUE))

# ---- source-functions --------------------------------------------------------
script_dir <- if (interactive()) {
  "manipulation/redcap-generator"
} else {
  dirname(sub("--file=", "", commandArgs()[grep("--file=", commandArgs())]))
}

source(file.path(script_dir, "generate-data-dictionary.R"))
source(file.path(script_dir, "generate-redcap-ferry.R"))

# ---- run-setup ---------------------------------------------------------------
config <- config::get()


# Step 1: Generate data dictionary
message("STEP 1: Generate Data Dictionary")

schema_data <- generate_data_dictionary(
  output_path    = "./data-public/metadata/redcap-data-dictionary.csv",
  link_column    = "mrn_mpi",
  preserve_edits = TRUE
)

# Step 2: Generate ferry (SQL scripts + R ferry script)

generate_redcap_ferry(
  schema_data = schema_data,
  output_dir  = "manipulation/redcap-ferry"
)

# ---- complete ----------------------------------------------------------------

message("REDCap setup complete!")
message("\nGenerated files:")
message("  - data-public/metadata/redcap-data-dictionary.csv")
message("  - manipulation/redcap-ferry/*.sql")
message("  - manipulation/redcap-ferry/redcap-ferry.R")
message("\nNext steps:")
message("  1. Review/edit the data dictionary CSV")
message("  2. Import data dictionary into REDCap")
message("  3. Run: source('manipulation/redcap-ferry/redcap-ferry.R')")
