# REDCap Generator & Ferry System

## Overview
  
1. Generator phase (manipulation/redcap-generator/) — inspects the database schema and auto-generates a REDCap data
dictionary CSV, SQL query scripts, and an R upload script.
2. Ferry phase (manipulation/redcap-ferry/) — the generated scripts that move data into REDCap.

*Note: All fields are marked @READONLY in REDCap.*
  
---
## Phase 1: Running the Generator

Open R with the working directory at the project root, then:
  
  `source("manipulation/redcap-generator/run-redcap-setup.R")`

This runs two steps sequentially:
  
## Step 1 — `generate_data_dictionary()`

Connects to the database, queries INFORMATION_SCHEMA.COLUMNS for all tables in the schema, and produces
data-public/metadata/redcap-data-dictionary.csv.

### What it auto-detects:
- __Feature: 'Root table'__
How: Matches table names against patterns: pt, pt_*, patient, patient_*, etc. The root table is the primary
(non-repeating) instrument.
- __Feature: Repeat instruments__
How: Queries each table for duplicate IDs (GROUP BY link_column HAVING COUNT(*) > 1). Tables with multiple rows per
patient become repeating instruments. Note: this is a good way to catch accidental duplicate rows (e.g., 'birth' with 1+ row per baby)
- __Feature: Order-by column__
How: Scans column names for date, time, instant, _dt, _dts, _datetime patterns. Used to assign redcap_repeat_instance
via ROW_NUMBER().
- __Feature: Duplicate columns__
How: If the same column name appears in multiple tables, non-root columns get a _tablename suffix (e.g., birth_date in pt stays as-is, but in birth_epic becomes birth_date_birth_epic).

Interactive prompts:
1. Verify root table — confirm or override the detected root table.
2. Verify link column — confirm or override the column used to join to pt_identity (default: mrn_mpi).

(Experimental): Preserving edits — if the CSV already exists, user-editable columns (Field Label, Field Type, validation, branching
                                                                     logic, etc.) are preserved. New fields are appended. Removed fields are kept with a [REMOVED FROM SCHEMA] note.

Parameters:
  generate_data_dictionary(
    cnn            = NULL,           # optional existing DB connection
    schema_name    = NULL,           # from config.yml if NULL
    output_path    = "./data-public/metadata/redcap-data-dictionary.csv",
    root_table     = NULL,           # auto-detected if NULL
    link_column    = "mrn_mpi",      # column linking tables to pt_identity
    preserve_edits = TRUE            # merge with existing CSV
  )

## Step 2 — generate_redcap_ferry()

Takes the schema data from Step 1 and generates all ferry files.

What it generates:

- File: manipulation/redcap-ferry/pt-identity.sql
Purpose: Creates/populates the identity mapping table (mrn_mpi → record_id). Uses EXCEPT to insert only new patients. This allows     for patient REDCap records

- File: manipulation/redcap-ferry/<table>.sql
Purpose: One SQL file per table. Joins to pt_identity for record_id. Repeat instruments include ROW_NUMBER() and
literal instrument name.

- File: manipulation/redcap-ferry/redcap-ferry.R
Purpose: The upload script: loads data via SQL, grooms columns, verifies values, uploads to REDCap via
REDCapR::redcap_write().

- File: manipulation/redcap-ferry/.gitignore
Purpose: Excludes .backups/ from git.
Automatic exclusions: tables starting with ss_ (supporting/lookup tables) are excluded from the ferry.

#### Interactive prompt: in RStudio, you can exclude additional tables by entering comma-separated names.

Custom sections: any code blocks in redcap-ferry.R marked with # ---- custom-* headers are preserved across
regenerations. Use this for project-specific grooming or transformations.

Parameters:
  generate_redcap_ferry(
    schema_data     = NULL,          # output of generate_data_dictionary(); reconstructed from CSV if NULL
    dictionary_path = "./data-public/metadata/redcap-data-dictionary.csv",
    output_dir      = "manipulation/redcap-ferry",
    root_table      = NULL,          # from schema_data attributes if NULL
    link_column     = "mrn_mpi"
  )

## Step 2a - Verify Scripts

Check each of the generated scripts for the ferry. Verify that redcap_repeat_instrument row_number()s are correct. Verify table names make sense and unnecessary variables are commented out/removed. If unnecessary variables removed, may need to be deleted from redcap-data-dictionary.csv. 

---
## Phase 2: Importing into REDCap

After generation, there are manual REDCap admin steps before the ferry can upload:
  
1. Review the data dictionary — open data-public/metadata/redcap-data-dictionary.csv and edit Field Labels, validation
types, or branching logic as needed.
2. Import into REDCap — in your REDCap project, go to Project Setup > Data Dictionary > Upload, and upload the CSV.
3. Enable repeating instruments — in Project Setup > Enable optional modules and customizations > Repeating
instruments, enable the instruments that were detected as repeating (e.g., birth, dx_baby, dx_mom_comorbidities, note_ophthalmology_epic, report_epic_image, visit_meditech).

---
## Phase 3: Running the Ferry (uploading data)

First-time setup

The pt_identity table must exist before any ferry SQL can run. If this is the first time:
  
1. Uncomment the CREATE TABLE block in manipulation/redcap-ferry/pt-identity.sql and run it once against
cdw_transaction.
2. Re-comment the CREATE TABLE block immediately — never drop or recreate this table since record_id is an IDENTITY
column and values cannot be regenerated.

#### Regular execution

The pt-identity.sql script is already wired into flow.R as the last step in the rail, so new patients get assigned
record_id values each time flow.R runs.

To upload data to REDCap:
  
  `source("manipulation/redcap-ferry/redcap-ferry.R")`

This script:
1. Retrieves credentials from the security database
2. Executes each SQL file to pull data (joined to pt_identity for record_id)
3. Selects/orders columns for each dataset
4. Runs OuhscMunge::verify_value_headstart() for basic validation
5. Uploads each dataset to REDCap with REDCapR::redcap_write() in batches of 1000

Development safety: the line # stop("Uncomment to block uploads during development") can be uncommented to prevent
uploads while testing.

---
Generated SQL Patterns

Standard table (one row per patient, e.g., pt.sql):
```sql  
SELECT
i.record_id
, p.column_name
FROM schema.table p
inner join cdw_transaction.schema.pt_identity i ON p.mrn_mpi = i.mrn_mpi
```

Repeating instrument (multiple rows per patient, e.g., dx-baby.sql):
```sql 
  SELECT
i.record_id
, d.column_name AS column_name_tablename
, row_number() over(partition by i.record_id order by d.date_column)
as redcap_repeat_instance
, 'table_name' as redcap_repeat_instrument
FROM schema.table d
inner join cdw_transaction.schema.pt_identity i ON d.mrn_mpi = i.mrn_mpi
```
---
When to Re-run the Generator

Re-run run-redcap-setup.R when:
- New tables are added to the staging schema
- Columns are added/removed from existing tables
- A table's cardinality changes (e.g., a table that was one-row-per-patient now has multiple rows)

  The generator will preserve your edits to the data dictionary and back up any changed files. After regenerating,
  re-import the updated data dictionary into REDCap.
