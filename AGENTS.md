# Agent Preferences — CRDW Research Project

## Project Context

This is a clinical research data warehouse (CRDW) project repository, spawned from
`cdw-skeleton-1` via the `pluripotent` R package. It follows a standard structure used
across all OuhscBbmc research projects.

## Session Start Checklist

At the start of every session, do these steps before anything else:

1. Check whether `documentation/github-issues.md` exists.
   - If it's missing, run `utility/export-repo-issues.py` automatically and tell the user it was generated.
   - Prefer `python utility/export-repo-issues.py`. If `python` is not on PATH, try `py utility/export-repo-issues.py`. If both fail on this Windows workstation, use:

     ```powershell
     & "$env:LOCALAPPDATA\Microsoft\WindowsApps\PythonSoftwareFoundation.Python.3.11_qbz5n2kfra8p0\python.exe" utility\export-repo-issues.py
     ```

   - If it exists but was last generated more than 7 days ago (check the `Generated:` line
     at the top), offer to refresh it before proceeding.
2. Read `documentation/github-issues.md` to understand the research question, inclusion
   criteria, current task status, and any open items.
3. Read `config.yml` to get `project_name` and `schema_name`.
4. Check what SQL scripts already exist in `manipulation/` so you don't regenerate things
   that are already there.

Only after those steps are done should you respond to what the user is asking.

## Repo Structure

- `config.yml` — project configuration. Key fields: `project_name`, `schema_name`,
  `dsn_staging`. The `schema_name` is how the project's staging tables are addressed in SQL,
  e.g. `[cdw_cache_staging].[{schema_name}].[table_name]`.
- `flow.R` — master orchestration script. Runs all manipulation and analysis steps in order.
  Do not run this without explicit permission.
- `manipulation/` — data extraction and transformation scripts.
  - `manipulation/ss/` — study-specific concept-set lookup tables (ss_dx, ss_med,
    ss_clinic, etc.). These identify which diagnoses/medications/locations are in scope
    for the study and require PI review before being run. Placeholder files exist here
    until the PI provides reviewed concept sets. These are distinct from the extraction
    scripts in `manipulation/` — ss- files define *what* to look for; extraction scripts
    define *how* to pull it.
  - When creating an `ss_dx` script, use the placeholder script already in the repo as the
    starting point. Do not fetch or create a separate new template. The finished script
    should be named `manipulation/ss/ss-dx-create.sql`.
  - When creating `ss-*` files, do not add extra categories unless it seems necessary. Search sibling repos for examples. In the absence of an example, produce the minimum necessary fields to allow for SQL joins and for PI review.
  - `manipulation/scribe-factory.R` — pulls data from CRDW staging tables into local CSV files.
  - `manipulation/pt-list-ellis.R` — patient list processing (if present).
  - SQL scripts here pull from `cdw_cache_staging.{schema_name}.*` staging tables.
- `analysis/` — analysis and reporting scripts.
  - `analysis/inspection/inspection.R` — quick look at data shape and quality.
- `documentation/` — project documentation.
  - `documentation/github-issues.md` — generated mirror of GitHub issues. Read this first.
  - `documentation/inclusion-and-data-map.md` — inclusion criteria and data element map.
  - `documentation/protocol-to-ss-map.md` — maps protocol concepts to ss- table entries.
  - `documentation/ai-sessions/` — session logs (see Session Logs below).
- `data-public/` — data safe to commit to GitHub (aggregates, metadata, mock data).
- `data-unshared/` — local data not committed to GitHub (raw extracts, private outputs).
- `utility/` — helper scripts including `populate-scripts.py`.

## SQL Templates

When the user asks you to generate or scaffold SQL scripts, do not ask them which
templates they want. Instead:

1. Read `documentation/github-issues.md` and `README.md` to determine what data types
   the study needs (diagnoses, medications, labs, encounters, etc.) and which source
   systems are relevant (Epic, Meditech, Centricity, etc.).
2. Determine the minimal set of templates needed based on that context.
3. Run the populate script directly with your selection:

   ```terminal
   python utility/populate-scripts.py --templates patient dx medication-meditech
   ```

   If `python` is not on PATH, use the same Python fallback sequence from the session-start checklist.
4. Show the user which templates you chose and why, then describe what needs to be
   customized in each generated file (dates, WHERE clause, inclusion criteria).
   Name generated scripts with a two-digit sequence prefix that reflects dependency
   order, e.g. `01-patient.sql`, `02-dx.sql`, `03-medication-meditech.sql`. The patient
   pool script always runs first; downstream tables that join to `pt_pool` come after.
5. Add the generated SQL scripts to `flow.R` in dependency order, and add every output
   staging table that should be exported to `config.yml` under `tables_to_scribe`.
   `scribe-factory.R` requires `path_output_summary`, `path_output_description`, and
   one `tables_to_scribe` entry per exported table with `name`, `columns_include`,
   `path_output`, and `row_unit`.
   Never add `ss-` tables (ss_dx, ss_med, ss_clinic, etc.) to `tables_to_scribe` — they
   are concept-set lookup inputs, not study outputs.
6. Every generated SQL script should produce one or more permanent tables in the
   project schema. If a table is only intermediary, use a CTE or `#temp` table instead
   of a project-schema staging table. If the workflow naturally needs multiple permanent
   output tables and one script would become unclear, generate a second script rather
   than hiding a separate deliverable as an intermediary table.

Fetch templates from:

```plain
https://raw.githubusercontent.com/OuhscBbmc/cdw-skeleton-1/main/manipulation/templates/{template-name}.sql
```

Do not fetch templates speculatively — only when the user asks for script generation.

Available templates and when to use them:

| Template | Use when |
|---|---|
| `patient.sql` | Always — builds the permanent `patient` table; any patient-pool helper should be a CTE or `#temp` table unless explicitly needed as a deliverable |
| `dx.sql` | Study involves diagnoses / problem list (Epic + harmonized) |
| `dx-meditech.sql` | Study needs encounter-level billing diagnoses from Meditech only |
| `medication-epic.sql` | Medications from Epic (admin date >= 2023-06-03) |
| `medication-meditech.sql` | Medications from Meditech (admin date < 2023-06-03) |
| `medication-harmonized.sql` | Medications needed across both Epic and Meditech |
| `medication-centricity.sql` | Medications from Centricity (OB/GYN patients) |
| `encounter-epic.sql` | Encounter/visit data from Epic |
| `encounter-meditech.sql` | Encounter/visit data from Meditech |
| `encounter-harmonized.sql` | Encounters needed across both systems |
| `lab-epic.sql` | Lab results from Epic |
| `lab-meditech.sql` | Lab results from Meditech |
| `obs-epic.sql` | Observations/vitals from Epic |
| `obs-meditech.sql` | Observations/vitals from Meditech |
| `obs-centricity.sql` | Observations from Centricity |
| `note-epic.sql` | Clinical notes from Epic |
| `note-meditech.sql` | Clinical notes from Meditech |
| `note-centricity.sql` | Clinical notes from Centricity |
| `procedure-harmonized.sql` | Procedures (CPT codes) across systems |
| `charlson-comorbidities.sql` | Charlson comorbidity index |
| `elixhauser-comorbidities.sql` | Elixhauser comorbidity index |
| `pt-identity.sql` | Only when the project requires a REDCap database or stable REDCap `record_id`; do not generate for routine cross-system MRN lookup |
| `patient-insurance.sql` | Insurance / payer data |
| `birth-epic.sql` | Birth records from Epic |
| `birth-meditech.sql` | Birth records from Meditech |
| `image-epic.sql` | Imaging/radiology from Epic |
| `visit-gecb.sql` | Visit data from GECB (scheduling/billing system) |
| `invoice-gecb.sql` | Invoices/charges from GECB |

Template variables to substitute when generating scripts:

- `{project_schema}` — replace with `schema_name` from `config.yml`
- `{date_start}` — study start date from IRB / inclusion criteria
- `{date_stop}` — study end date from IRB / inclusion criteria

## Searching Other Repos for Patterns

If a template doesn't exist or the user needs an example of how a specific pattern has been
implemented before, search sibling repos which will be contained in the GitHub directory (one level up from the current repo directory):

Look for SQL files with similar naming conventions (e.g. `medication-meditech.sql`,
`patient-flags.sql`). Prefer recent repos. Always show the user what you found and where
before reusing anything.

## SQL Coding Style

- Keywords lower case **except**: `SELECT`, `FROM`, `WHERE`, `GROUP BY`, `HAVING`,
  `ORDER BY`, `DECLARE`
- In `CREATE TABLE`: nullable columns omit the `null` keyword — just the type is enough.
  Only `not null` needs to be stated explicitly.
- In `CREATE TABLE`: use a trailing comma on the last column definition (before the closing `)`).
- In `INSERT` statements: never include a column list. Use `INSERT table` followed
  directly by `SELECT`.
- All `CREATE TABLE` statements for a script's output tables must appear before the first
  `SELECT` in the file. If a script uses a `#temp` table, define all permanent output
  tables first, then define and populate the temp table, then insert into the permanent tables.
- Use CTEs for readability; use `#temp` tables when intermediary logic is reused enough
  that a CTE becomes unreadable
- Add a commented out `--exec dbo.generate_create_table_sp '{schema_name}.{table_name}' above table definition so user can quickly re-create table definitions when making modifications.
- Every SQL script should create permanent project-schema tables as its final outputs.
  Intermediary tables should be CTEs or `#temp` tables. If a second permanent table is
  needed as a distinct deliverable or reusable project table, create it deliberately and
  consider splitting it into a second script.
- `ss-` files are an exception: study-specific lookup tables (ss_dx, ss_med, etc.) stay as
  separate staged tables when PI review is required
- When a SQL script creates a table intended for delivery, add a matching
  `tables_to_scribe` config entry so `manipulation/scribe-factory.R` exports it.
  Never add `ss-` tables to `tables_to_scribe` — they are lookup inputs, not outputs.
- Joins indented and nested under `FROM`
- Extra space in `left  join`
- Single-variable join on one line:

  ```sql
  FROM patient p
    inner join visit v on p.patient_id = v.visit_id
  ```

- Multi-variable join, each condition on its own line:

  ```sql
  FROM patient p
    inner join visit v on
      p.patient_id = v.visit_id
      and
      p.birth_date <= v.visit_date
  ```

## R Coding Style

- Prefix functions with their package: `dplyr::filter()`, `readr::read_csv()`
- `dplyr::distinct()` for deduplication in ellis scripts; no `count()` + `filter` diagnostic blocks
- 2 spaces per indentation level; spaces not tabs
- Match the existing style in the file being edited

## Safety Rules

- Do not execute any R script without explicit permission for that exact command.
  This includes `flow.R`, `spawn.R`, `renv`, knitr, and any script that reads or writes data.
- Do not run any R scripts or anything that  might access a database or data source without explicit permission.
- Do not access any .csv or excel files without explicit permission.
- Do not access files outside of the repo without explicit permission.
  without permission.
- `ss-` files require PI review and sign-off before they are run against live data.
- Editing files is always fine. Running them requires permission.

## Session Logs

At the end of every session that does real work, update the daily summary note briefly at:

```
documentation/ai-sessions/yyyy-mm-dd.md
```

Prefer one AI session note per day. If today's note already exists, append notes to the existing file instead of creating another same-day note. Create the `ai-sessions/`
folder if it doesn't exist. Include:

- what was done
- files created or changed
- commands run (if any)
- what was not run
- open items
- a short prompt a future agent can use to continue
- what LLM model was used and via what platform (e.g., Claude Sonnet 4.6 via Claude Code or GPT 5.5 via Codex, etc.)
- close every note with 'Dear Dr. Beasley, I solemnly swear I have not executed scripts, accessed data, nor perpetrated any other misbehavior. Sincerely, A Friendly Robot'
