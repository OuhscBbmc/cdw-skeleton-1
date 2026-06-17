# Agent Preferences — CRDW Research Project

## Project Context

This is a clinical research data warehouse (CRDW) project repository, spawned from
`cdw-skeleton-1` via the `pluripotent` R package. It follows a standard structure used
across all OuhscBbmc research projects.

**Read `ai/safety-rules.md` before doing anything else.** Load other `ai/` files only when
the task requires them — do not load them speculatively.

## Session Start Checklist

Do these steps in order before responding to any request:

1. Read `ai/safety-rules.md`.
2. Read `config.yml` — extract `project_name` and `schema_name`.
3. Check `ai/ai-state.md`:
   - **Exists and < 7 days old** (check `Last updated:`) → read it; skip
     `documentation/github-issues.md` entirely.
   - **Missing or stale** → read `documentation/github-issues.md`. Summarize, then write
     `ai/ai-state.md` immediately (format in `ai/session-logging.md`). If github-issues.md is
     missing, run `utility/export-repo-issues.py` first. Prefer `python`; fallback `py`;
     Windows fallback:
     ```powershell
     & "$env:LOCALAPPDATA\Microsoft\WindowsApps\PythonSoftwareFoundation.Python.3.11_qbz5n2kfra8p0\python.exe" utility\export-repo-issues.py
     ```
4. List `.sql` files already in `manipulation/` (exclude `templates/` and `sweep-and-specify/`).

## Repo Structure

- `config.yml` — `project_name`, `schema_name`, `dsn_staging`. Schema addressed in SQL as
  `[cdw_cache_staging].[{schema_name}].[table_name]`.
- `flow.R` — master orchestration script. Do not run without explicit permission.
- `manipulation/` — SQL extraction and R transformation scripts.
  - `manipulation/ss/` — study-specific concept-set lookup tables (ss_dx, ss_med, ss_clinic, etc.).
    These define *what* to look for and require PI review before running. Distinct from extraction
    scripts, which define *how* to pull data. Use the placeholder already in the repo as the starting
    point; name finished scripts `manipulation/ss/ss-{type}-create.sql`.
- `analysis/` — analysis and reporting scripts.
- `ai/` — machine-facing context files (safety rules, style guides, session state). Do not edit manually.
- `documentation/` — human-facing project docs and audit logs.
  - `documentation/github-issues.md` — generated mirror of GitHub issues.
  - `documentation/ai-sessions/` — human audit logs (one per day, append-only).
- `data-public/` — data safe to commit (aggregates, metadata, mock data).
- `data-unshared/` — local data, not committed.
- `utility/` — helper scripts including `populate-scripts.py`.

## Workflow

Detailed instructions for each workflow step live in `.claude/commands/`. Claude Code
users invoke them as slash commands. Codex and other tools: read the relevant file and
follow the steps listed there.

When asked to run a workflow step by name, read `.claude/commands/[name].md` and follow
the steps there. Claude Code users invoke them as `/[name]`; all other tools just say
the step name (e.g. `sql-work`) and read the corresponding file.

| Step name | When to use |
|---|---|
| `project-orient` | New contributor or returning after a gap |
| `session-start` | Beginning of any normal session |
| `sql-inventory` | List existing scripts and their status |
| `sql-generate` | Scaffold scripts for a fresh project |
| `sql-work` | Review and style-correct scripts in sequence |
| `ss-create [type]` | Build a concept-set lookup table (dx, med, lab, etc.) |
| `session-end` | Write session outputs and audit log |

## Lazy-Loading Rules

| Task | Load |
|---|---|
| Session start | `ai/safety-rules.md` + `ai/ai-state.md` (or github-issues.md if stale) |
| Any SQL editing | `ai/sql-style.md` |
| Generating SQL from templates | `ai/sql-style.md` + `ai/sql-templates.md` |
| Creating `ss-` lookup tables | `ai/sql-style.md` |
| Any R editing | `ai/r-style.md` |
| Writing session outputs | `ai/session-logging.md` |

Do not load any `ai/` file unless the current task requires it.
