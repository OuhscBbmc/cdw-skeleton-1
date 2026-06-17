# /sql-generate

Scaffold SQL extraction scripts for a fresh project. Run after `/session-start` when no
scripts exist yet in `manipulation/`.

## Steps

1. Read `ai/sql-style.md` and `ai/sql-templates.md`.

2. Read `ai/ai-state.md` (or `documentation/github-issues.md` if state is stale) to determine:
   - What data types the study needs (diagnoses, medications, labs, encounters, etc.)
   - Which source systems are relevant (Epic, Meditech, Centricity, GECB)
   - Whether the project requires REDCap integration (determines if `pt-identity.sql` is needed)

3. Determine the minimal set of templates needed based on that context. Do not ask the user
   which templates to use — infer from the study description. Show your reasoning.

4. Run the populate script with your selection:
   ```
   python utility/populate-scripts.py --templates patient dx medication-meditech
   ```
   Use the Python fallback from AGENTS.md if `python` is not on PATH.

5. For each generated script, describe:
   - What needs to be customized (dates, WHERE clause, inclusion criteria)
   - Which ss- lookup tables it will join to (if any)

6. Add each generated script to `flow.R` in dependency order.

7. Add each delivery table to `config.yml` under `tables_to_scribe` with `name`,
   `columns_include`, `path_output`, and `row_unit`. Never add ss- tables.

8. Update `ai/ai-state.md` with the new script inventory.

9. Tell the user: "Scripts scaffolded. Run `/sql-work` to walk through and customize each
   one, or `/ss-create [type]` to build concept-set lookup tables."

## Notes

- `patient.sql` is always first — it builds `pt_pool`. All other scripts join to it.
- Do not fetch templates speculatively — only what the study context requires.
- If the user already has some scripts, skip to the missing ones rather than regenerating.
