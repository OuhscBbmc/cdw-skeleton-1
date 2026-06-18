# /cdw-sql-scaffold

Pull down SQL script templates for all confirmed data asks and open a GitHub tracking
issue for each new script. No customization — scripts are committed as-is before any
logic is introduced.

Run after `/cdw-plan`.

## Steps

1. Read `ai/ai-state.md` for the confirmed data plan (scripts needed + ss-dependencies).
   If no plan exists, run `/cdw-plan` first.

2. Spawn an Explore subagent:
   > "List all `.sql` files in `manipulation/` (exclude `templates/`, `sweep-and-specify/`, `ss/`).
   > Return a table: `File` | `Status` (populated / stub)"

3. Identify which planned scripts are missing. If none: "All scripts already present." Stop.

4. Ask: "Scaffold [list of missing scripts]?" Wait for confirmation.

5. Read `ai/sql-templates.md`. Run:
   ```
   python utility/populate-scripts.py --templates [list]
   ```
   Use the Python fallback from AGENTS.md if `python` is not on PATH.

6. Add each generated script to `flow.R` in dependency order, **commented out**.

7. Add each delivery table to `config.yml` under `tables_to_scribe` with `name`,
   `columns_include`, `path_output`, `row_unit`. Never add ss- tables.

8. For each generated script, check whether a tracking issue already exists:
   ```
   gh issue list --search "Script: [filename]" --state all --json number,title
   ```
   - **Issue found** → note the existing issue number; do not create a duplicate.
   - **No issue found** → create one:
     ```
     gh issue create --title "Script: [filename]" --body "..."
     ```
     Issue body:
     - **Purpose** — what the script produces (one sentence)
     - **Customization needed** — checklist of placeholders (dates, WHERE clause, inclusion criteria)
     - **ss-table dependencies** — which ss-files it joins to, or "None — can start immediately"
     - **Checklist:**
       - `[ ]` Script customized
       - `[ ]` Uncommented in `flow.R`
       - `[ ]` Validated against staging

9. Update `ai/ai-state.md` — add each script with its issue number:
   ```
   Scripts:
     patient.sql — stub — #14
     dx.sql — stub — #15
   ```

10. Tell the user:
    "Scripts scaffolded. Issues: [script → #N list].
    Commit these templates now.
    Next: run `/cdw-ss-build [type]` for each ss-file needed, then `/cdw-sql-work` to
    start on `patient.sql` while you wait for PI to return the ss-files."

## Notes

- `patient.sql` is always first in `flow.R` — it builds `pt_pool`.
- Do not fill in any study-specific values here. That happens in `/cdw-sql-work`.
- One issue check/create per script — do not batch.
