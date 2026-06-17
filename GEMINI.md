# Agent Preferences — CRDW Research Project (Gemini)

This is a clinical research data warehouse (CRDW) project repository, spawned from
`cdw-skeleton-1` via the `pluripotent` R package. It follows a standard structure used
across all OuhscBbmc research projects.

**Read `ai/safety-rules.md` before doing anything else.** Load other `ai/` files only when
the task requires them — do not load them speculatively.

In Gemini CLI, use `@ai/sql-style.md`, `@ai/sql-templates.md`, etc. to load topic files
on demand when the task calls for them.

## Session Start Checklist

Do these steps in order before responding to any request:

1. Read `@ai/safety-rules.md`.
2. Read `@config.yml` — extract `project_name` and `schema_name`.
3. Check `ai/ai-state.md`:
   - **Exists and < 7 days old** → read it; skip `documentation/github-issues.md` entirely.
   - **Missing or stale** → read `documentation/github-issues.md`. Summarize, then write
     `ai/ai-state.md` immediately (format in `@ai/session-logging.md`). If github-issues.md is
     missing, run `python utility/export-repo-issues.py` first.
4. List `.sql` files already in `manipulation/` (exclude `templates/` and `sweep-and-specify/`).

## Repo Structure

- `config.yml` — `project_name`, `schema_name`, `dsn_staging`.
- `flow.R` — master orchestration script. Do not run without explicit permission.
- `manipulation/` — SQL extraction and R transformation scripts.
  - `manipulation/ss/` — study-specific concept-set lookup tables. Define *what* to look for;
    require PI review before running. Name finished scripts `manipulation/ss/ss-{type}-create.sql`.
- `documentation/github-issues.md` — generated mirror of GitHub issues; read at session start.
- `documentation/ai-sessions/` — session logs.
- `data-public/` — data safe to commit.
- `data-unshared/` — local data, not committed.
- `utility/` — helper scripts.

## Lazy-Loading Rules

| Task | Load |
|---|---|
| Any SQL editing | `@ai/sql-style.md` |
| Generating SQL from templates | `@ai/sql-style.md` + `@ai/sql-templates.md` |
| Creating `ss-` lookup tables | `@ai/sql-style.md` |
| Any R editing | `@ai/r-style.md` |
| Writing session log | `@ai/session-logging.md` |

## Workflow Steps

When asked to run a workflow step by name, read `.claude/commands/[name].md` and follow
the steps there. Step names: `project-orient`, `session-start`, `sql-inventory`,
`sql-generate`, `sql-work`, `ss-create`, `session-end`.

## Typical Session Sequence

1. **Orient** — run the session start checklist above.
2. **Inventory** — list `.sql` files in `manipulation/` (exclude `templates/`, `sweep-and-specify/`);
   report sequence, purpose, and status (populated / stub / missing).
3. **Generate** (fresh projects only) — load `@ai/sql-templates.md`; infer required templates from
   study context; run `python utility/populate-scripts.py --templates ...`; add to `flow.R` and
   `config.yml`.
4. **SQL review** — load `@ai/sql-style.md`; distill to a compact checklist; check each script
   in sequence and propose edits.
5. **Lookup tables** — load `@ai/sql-style.md`; build `ss_dx`, `ss_med`, etc. following
   STEP 1 (discovery query) → PI review → STEP 2 (load). Remind user: PI sign-off required.
6. **Close** — load `@ai/session-logging.md`; overwrite `ai/ai-state.md` with current state;
   append human audit log to `documentation/ai-sessions/YYYY-MM-DD.md`.
