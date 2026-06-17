# CRDW Project — Copilot Instructions

This is a clinical research data warehouse (CRDW) project repository (OuhscBbmc).
For full detail on any section below, read the corresponding file in `ai/`.

## Session Start

Before starting any work:

1. Read `ai/safety-rules.md`.
2. Read `config.yml` — note `project_name` and `schema_name`.
3. Check `ai/ai-state.md`:
   - **Exists and < 7 days old** (check `Last updated:`) → read it; skip
     `documentation/github-issues.md` entirely.
   - **Missing or stale** → read `documentation/github-issues.md`. Summarize it, then write
     `ai/ai-state.md` immediately (format in `ai/session-logging.md`). If github-issues.md is
     missing, generate it first: `python utility/export-repo-issues.py`
     (fallback: `py`, then the Windows App path for Python 3.11).

## Safety Rules (summary — full rules in `ai/safety-rules.md`)

- Do not suggest running R scripts, `flow.R`, `scribe-factory.R`, or SQL against live databases.
- Do not access `.csv` or Excel files without explicit user permission.
- `ss-` files require PI review before running.
- Editing files is safe; running them requires explicit user permission.

## Repo Layout

- `config.yml` — project name and schema name for SQL staging tables
- `manipulation/` — SQL extraction scripts and R transformation scripts
- `manipulation/ss/` — study-specific lookup tables (require PI review before running)
- `manipulation/scribe-factory.R` — pulls staging data to local CSVs
- `ai/` — machine-facing context files: style guides, safety rules, `ai-state.md` (session state)
- `documentation/` — human-facing docs: github-issues.md, audit logs in `ai-sessions/`
- `data-unshared/` — local data, not committed to GitHub

## SQL Work

When editing or generating SQL files, read `ai/sql-style.md` and `ai/sql-templates.md` first.

Key SQL rules (abbreviated — see `ai/sql-style.md` for complete rules):
- Keywords lower case except: `SELECT`, `FROM`, `WHERE`, `GROUP BY`, `HAVING`, `ORDER BY`,
  `DECLARE`, `CREATE TABLE`, `DROP TABLE`, `INSERT`, `UNION`, `VALUES`, `WITH`
- `CREATE TABLE`: nullable columns omit `null`; trailing comma on last column; pad for alignment
- `INSERT`: no column list — `INSERT table` followed directly by `SELECT`
- All `CREATE TABLE` before first `SELECT` in the file
- Joins nested under `FROM`; extra space in `left  join`; 2-space indentation
- Start staging files with `use cdw_cache_staging;`

When generating SQL from templates, also read `ai/sql-templates.md`:
- Fetch URL: `https://raw.githubusercontent.com/OuhscBbmc/cdw-skeleton-1/main/manipulation/templates/{name}.sql`
- Substitute `{project_schema}`, `{date_start}`, `{date_stop}`
- Name scripts descriptively: `patient.sql`, `dx.sql`, `medication-meditech.sql`, etc. Execution order is set in `flow.R`, not by filename.
- Never add `ss-` tables to `tables_to_scribe` in `config.yml`

For `ss_dx` and other study-specific lookups: use the existing placeholder in `manipulation/ss/`
as the starting point. Name finished scripts `manipulation/ss/ss-{type}-create.sql`.

## R Work

When editing R files, read `ai/r-style.md` first.

Key R rules: prefix functions (`dplyr::filter()`), use `dplyr::distinct()` for deduplication,
2-space indentation, match existing file style.

## Session Logging

At the end of any session that does real work, read `ai/session-logging.md` and write a dated
note to `documentation/ai-sessions/YYYY-MM-DD.md`.
