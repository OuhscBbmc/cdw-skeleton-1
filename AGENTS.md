# Agent Preferences — CRDW Research Project

## Project Context

This is a clinical research data warehouse (CRDW) project repository, spawned from
`cdw-skeleton-1` via the `pluripotent` R package. It follows a standard structure used
across all OuhscBbmc research projects.

Load `ai/` files only when the task requires them — do not load them speculatively.

## Safety Rules

These apply to every session. No exceptions.

- Never run any R script, `flow.R`, knitr, or renv without explicit permission naming that exact command.
- Never access data files (`.csv`, `.rds`, `.RData`, `.parquet`, databases, etc.) without explicit permission.
- Never access `data-private/`, `data-unshared/`, local extracts, or derived-data folders without explicit permission.
- Never include PHI, PII, row-level records, secrets, or connection strings in code, docs, prompts, commits, issues, or comments. Use synthetic placeholders instead.
- Never commit files without explicit permission. Our policy requires human review of each committed file before it reaches GitHub.
- Editing code, SQL, config, and documentation is safe. Running files or opening data outputs requires explicit permission for that exact command.
- Never construct, execute, or test a database connection (ODBC, SQL Server, or otherwise) without explicit permission, even to verify connectivity.
- A granted permission covers only the exact action named. It does not extend to related files, folders, schemas, or commands.

If a session involves running scripts, accessing data, or committing, read `ai/safety-rules.md` for the full ruleset before proceeding.

## Session Start

Do these steps in order before responding to any request:

1. Read `config.yml` — extract `project_name` and `schema_name`.
3. Check `ai/ai-state.md`:
   - **Exists and < 7 days old** (check `Last updated:`) → read it; skip
     `documentation/github-issues.md` entirely. Report state in 3 bullets: study aim,
     current focus, next steps.
   - **Missing or stale** → read `documentation/github-issues.md`. Summarize in 3 bullets,
     then write `ai/ai-state.md` immediately (format below). If github-issues.md is
     missing, run `utility/export-repo-issues.py` first (`python`, fallback `py`). If
     Python isn't found, run `/cdw-doctor` to diagnose.
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
last_updated_by: username
skeleton_synced: YYYY-MM-DD
project_name: ...
schema_name: ...
planning_issue: #N

Study aim: [one sentence]
Inclusion criteria:
  - ...

Scripts:
  patient.sql — populated — #14
  dx.sql — stub — #15
  encounter.sql — stub — #16 — pending: ss_enc
  [or "none yet"]
  [format: filename — status — #issue — pending: ss_xxx (omit if ready to work)]

Current focus: [what was being worked on]
Next steps:
  - ...
Open blockers: [or "none"]
```

Keep it under 25 lines. Terse — this is machine-read.

`last_updated_by` comes from `git config user.name`, normalized to lowercase-hyphenated.
At session start, if the state is fresh (< 7 days) but `last_updated_by` differs from the
current user, include that fact in the 3-bullet summary — e.g., "State last written by
grace-cruz on 2026-06-23." The user can decide whether to trust it or run a full orient.

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

Detailed step instructions live in `.claude/commands/`. The files are plain markdown — no
Claude-specific syntax — so any agent can read and follow them directly.

- **Claude Code:** invoke as slash commands (e.g., `/cdw-orient`)
- **Codex and all other agents:** at the start of each session, read
  `.claude/commands/cdw-orient.md` and follow the steps there. For subsequent steps, read
  the matching file listed below. Do not skip orient — it sets the state context everything
  else depends on.

| Step | File | Claude Code | When to use |
|---|---|---|---|
| Orient | `cdw-orient.md` | `/cdw-orient` | Start every session — quick if state is fresh, full briefing if stale |
| Plan | `cdw-plan.md` | `/cdw-plan` | New project — translate meeting notes into confirmed data plan |
| Scaffold | `cdw-sql-scaffold.md` | `/cdw-sql-scaffold` | Pull script templates; creates a GitHub issue per script |
| Concept sets | `cdw-ss-build.md` | `/cdw-ss-build [type]` | Generate discovery query and send to PI (non-blocking) |
| SQL work | `cdw-sql-work.md` | `/cdw-sql-work` | Customize scripts; starts patient.sql immediately, resumes others as ss-files return |
| End session | `cdw-end-session.md` | `/cdw-end-session` | Write session outputs and audit log |

**New project:** orient → plan → scaffold → concept sets *(send to PI, non-blocking)* → SQL work *(patient.sql now; resume others when PI returns)* → end session

**Every session:** orient → *(picks up where you left off)* → end session

## Lazy-Loading Rules

| Task | Load |
|---|---|
| Session start | `ai/ai-state.md` (or `documentation/github-issues.md` if stale) |
| Running scripts, accessing data, or committing | `ai/safety-rules.md` (full ruleset) |
| Any SQL editing | `ai/sql-style.md` |
| Generating SQL from templates | `ai/sql-style.md` + `ai/sql-templates.md` |
| Creating `ss-` lookup tables | `ai/sql-style.md` |
| Any R editing | `ai/r-style.md` |
| Drafting PI email | `ai/pi-email-templates.md` |

Do not load any `ai/` file unless the current task requires it.
