# Agent Preferences — CRDW Research Project

## Project Context

This is a clinical research data warehouse (CRDW) project repository, spawned from
`cdw-skeleton-1` via the `pluripotent` R package. It follows a standard structure used
across all OuhscBbmc research projects.

**Read `ai/safety-rules.md` before doing anything else.** Load other `ai/` files only when
the task requires them — do not load them speculatively.

## Session Start

Do these steps in order before responding to any request:

1. Read `ai/safety-rules.md`.
2. Read `config.yml` — extract `project_name` and `schema_name`.
3. Check `ai/ai-state.md`:
   - **Exists and < 7 days old** (check `Last updated:`) → read it; skip
     `documentation/github-issues.md` entirely. Report state in 3 bullets: study aim,
     current focus, next steps.
   - **Missing or stale** → read `documentation/github-issues.md`. Summarize in 3 bullets,
     then write `ai/ai-state.md` immediately (format below). If github-issues.md is
     missing, run `utility/export-repo-issues.py` first. Prefer `python`; fallback `py`;
     Windows fallback:
     ```powershell
     & "$env:LOCALAPPDATA\Microsoft\WindowsApps\PythonSoftwareFoundation.Python.3.11_qbz5n2kfra8p0\python.exe" utility\export-repo-issues.py
     ```
Maintenance note: run `/cdw-doctor` once per machine setup, or any time a command fails with an auth, network, or python-not-found error; this is a maintenance/diagnostic command, not part of the every-session or new-project sequence.

## Orient Workflow

Run this any time you want a full current briefing on the project — not just at the start,
but any time you need to re-anchor: after a meeting, when resuming complex work, or when
you want a prompt for what to tackle next. Always reads fresh regardless of ai-state age.
Claude Code users: `/cdw-orient`. All others: follow these steps.

1. Read `ai/safety-rules.md` and `config.yml`.
2. Read `documentation/github-issues.md` in full — always, bypassing `ai/ai-state.md`.
   If missing or > 7 days old, run `utility/export-repo-issues.py` first.
3. Read the two most recent files in `documentation/ai-sessions/`.
4. Spawn a subagent to scan `manipulation/` (exclude `templates/`, `sweep-and-specify/`):
   for each `.sql` file read the first 20 lines and return file name, stated purpose,
   output table(s), and status (populated / stub / empty).
5. Produce a structured briefing:
   - **Study summary** — research question and aim in 2–3 sentences
   - **Inclusion criteria** — bulleted list
   - **Data pipeline** — scripts, their status, what they produce
   - **Concept-set lookups** — which `ss-` tables are needed; done / pending PI review
   - **Recent work** — what happened in the last 1–2 sessions
   - **Open items** — blockers, decisions needed
   - **Suggested first task** — one concrete next action
6. Ask: "Does this match your understanding, or is anything out of date?"
7. Overwrite `ai/ai-state.md` (format below).

## ai-state.md Format

```
Last updated: YYYY-MM-DD
project_name: ...
schema_name: ...

Study aim: [one sentence]
Inclusion criteria:
  - ...

Scripts:
  patient.sql — populated — #14
  dx.sql — stub — #15
  [or "none yet"]
  [format: filename — status — #issue]

Current focus: [what was being worked on]
Next steps:
  - ...
Open blockers: [or "none"]
```

Keep it under 20 lines. Terse — this is machine-read.

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

Detailed step instructions live in `.claude/commands/`. Claude Code users invoke them as
slash commands. All other tools: read `.claude/commands/[step].md` and follow the steps there.

| Step | Claude Code | When to use |
|---|---|---|
| `cdw-orient` | `/cdw-orient` | Start every session — quick if state is fresh, full briefing if stale |
| `cdw-plan` | `/cdw-plan` | New project — translate meeting notes into confirmed data plan |
| `cdw-sql-scaffold` | `/cdw-sql-scaffold` | Pull script templates; creates a GitHub issue per script |
| `cdw-ss-build` | `/cdw-ss-build [type]` | Generate discovery query and send to PI (non-blocking) |
| `cdw-sql-work` | `/cdw-sql-work` | Customize scripts; starts patient.sql immediately, resumes others as ss-files return |
| `cdw-end-session` | `/cdw-end-session` | Write session outputs and audit log |

**New project:** `/cdw-orient` → `/cdw-plan` → `/cdw-sql-scaffold` → `/cdw-ss-build [type]` *(send to PI, non-blocking)* → `/cdw-sql-work` *(patient.sql now; resume others when PI returns)* → `/cdw-end-session`

**Every session:** `/cdw-orient` → *(picks up where you left off)* → `/cdw-end-session`

## Lazy-Loading Rules

| Task | Load |
|---|---|
| Session start | `ai/safety-rules.md` + `ai/ai-state.md` (or github-issues.md if stale) |
| Any SQL editing | `ai/sql-style.md` |
| Generating SQL from templates | `ai/sql-style.md` + `ai/sql-templates.md` |
| Creating `ss-` lookup tables | `ai/sql-style.md` |
| Any R editing | `ai/r-style.md` |

Do not load any `ai/` file unless the current task requires it.
