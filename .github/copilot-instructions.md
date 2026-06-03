# CRDW Project — Copilot Instructions

This is a clinical research data warehouse (CRDW) project repository (OuhscBbmc).

## Read First

If `documentation/github-issues.md` exists, read it before starting any work. It contains
the research question, inclusion criteria, and current task status. If it is missing, generate
it automatically before proceeding:

```powershell
python utility/export-repo-issues.py
```

If `python` is not on PATH, try `py`, then this Windows Python path:

```powershell
& "$env:LOCALAPPDATA\Microsoft\WindowsApps\PythonSoftwareFoundation.Python.3.11_qbz5n2kfra8p0\python.exe" utility\export-repo-issues.py
```

## Repo Layout

- `config.yml` — project name and schema name for SQL staging tables
- `manipulation/` — SQL and R extraction scripts
- `manipulation/ss/` — study-specific lookup tables (require PI review before running)
- `manipulation/scribe-factory.R` — pulls staging data to local CSVs
- `documentation/` — project docs and issue mirror
- `data-unshared/` — local data, not committed to GitHub

For `ss_dx`, use the existing placeholder script in the repo as the starting point and
name the finished script `manipulation/ss/ss-dx-create.sql`. Do not fetch or create a
separately named diagnosis concept-set template.

## SQL Templates

When generating SQL scripts, fetch templates from:
`https://raw.githubusercontent.com/OuhscBbmc/cdw-skeleton-1/main/manipulation/ss/templates/`

Common templates: `patient.sql`, `dx.sql`, `medication-epic.sql`, `medication-meditech.sql`,
`encounter-harmonized.sql`, `lab-epic.sql`, `charlson-comorbidities.sql`.

Substitute `{project_schema}` with the `schema_name` from `config.yml`.

Every generated SQL script should produce one or more permanent tables in the project
schema. Use CTEs or `#temp` tables for intermediary data. If distinct permanent outputs
would make one script unclear, split them into separate scripts. Only generate
`pt-identity.sql` when the project requires a REDCap database or stable REDCap
`record_id`; it is not needed for routine cross-system MRN lookup.

When adding SQL scripts that create delivery tables, also add them to `flow.R` in dependency
order and add matching `config.yml` `tables_to_scribe` entries. `scribe-factory.R` needs
`path_output_summary`, `path_output_description`, and each table entry's `name`,
`columns_include`, `path_output`, and `row_unit`.

## SQL Style

- Keywords lower case except: `SELECT`, `FROM`, `WHERE`, `GROUP BY`, `HAVING`, `ORDER BY`, `DECLARE`
- CTEs or `#temp` tables preferred for intermediary data; permanent project-schema tables should be final script outputs
- Joins nested under `FROM`, extra space in `left  join`
- 2-space indentation

## R Style

- Prefix functions: `dplyr::filter()`, `readr::read_csv()`
- `dplyr::distinct()` for deduplication; no count+filter diagnostic blocks
- 2-space indentation, spaces not tabs

## Safety

- Do not suggest running R scripts, `flow.R`, or SQL against live databases
- `ss-` files require PI review before running
- Editing files is safe; running them requires explicit user permission
