# /cdw-start

Run at the beginning of every Claude Code session in a CRDW project repo.

## Steps

1. Read `ai/safety-rules.md`.

2. Read `config.yml`. Extract and note `project_name` and `schema_name`.

3. Check `ai/ai-state.md`:
   - **Exists and < 7 days old** (check the `Last updated:` line) → read it; skip
     `documentation/github-issues.md` entirely. Report state to user in 3 bullets:
     study aim, current focus, next steps.
   - **Missing or stale** → read `documentation/github-issues.md`. Summarize in 3 bullets:
     research question, inclusion criteria, open tasks. Write `ai/ai-state.md` now
     (do not wait for session-end — format in `/cdw-end-session`).

4. Tell the user what to run next based on project state:
   - **New project, no scripts yet** → "Run `/cdw-plan` to confirm data asks, then `/cdw-sql-scaffold`."
   - **Scripts scaffolded, work in progress** → "Run `/cdw-sql-work` to continue. Blocked scripts: [list any waiting on ss-files]."
   - **Returning for ss-file work** → "Run `/cdw-ss-build [type]` then `/cdw-sql-work` to resume."

## What NOT to do here

- Do not read `ai/sql-style.md`, `ai/sql-templates.md`, `ai/r-style.md`, or any SQL files.
- Do not propose changes or generate anything yet.
