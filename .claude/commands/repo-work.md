# /repo-work

Run this at the start of every Claude Code session in a CRDW project repo.

## Steps

1. Read `AGENTS.md` to load project rules, SQL style, and safety constraints.

2. Check `documentation/github-issues.md`:
   - If missing: run `python utility/export-repo-issues.py` and tell the user it was generated.
   - If present: check the `Generated:` line at the top. If older than 7 days, ask the user if they want to refresh it before proceeding.

3. Read `documentation/github-issues.md` in full. Summarize:
   - The research question and study aim in one or two sentences
   - The current open issues and any clear next steps
   - What data types the study needs (diagnoses, medications, labs, encounters, etc.)

4. Read `config.yml` to get `project_name` and `schema_name`.

5. List what SQL scripts already exist in `manipulation/` so you don't regenerate them.

6. If no SQL scripts exist yet beyond placeholders, propose which templates to populate
   based on the study context — but do not run `populate-scripts.py` yet. Present the
   proposal and wait for the user to confirm before running anything.

7. Ask the user what they want to work on today.

## Reminders

- Do not run R scripts, flow.R, scribe-factory.R, or SQL against live databases without
  explicit permission for that exact command.
- Do not read or modify CSV files or spreadsheets without explicit permission.
- ss- files require PI review before running.
- Editing files is always safe. Running them requires a go-ahead.
- Write a session log to `documentation/ai-sessions/yyyy-mm-dd short-title.md` at the end.

## Script Naming

Name SQL scripts with a two-digit sequence prefix that reflects dependency order:
`01-patient.sql`, `02-dx.sql`, `03-medication-meditech.sql`, etc. The patient pool
script is always `01`. This makes run order unambiguous without needing to read `flow.R`.

## On tables_to_scribe

Never add `ss-` files (ss_dx, ss_med, ss_clinic, etc.) to `tables_to_scribe` in
`config.yml`. They are concept-set lookup inputs used by extraction scripts, not
study output tables. Only final delivery tables belong in `tables_to_scribe`.
