# /cdw-ss-build

Build a study-specific concept-set lookup table. Usage: `/cdw-ss-build [dx|med|lab|location|obs|cpt]`

Run after `/cdw-sql-scaffold`. This is non-blocking — generate the discovery query, send
it to the PI, then immediately continue with `/cdw-sql-work` on `patient.sql` while you wait.

## Steps

1. Read `ai/sql-style.md` and the existing placeholder in `manipulation/ss/` for the requested type.
   (If no placeholder exists, note it and proceed from scratch.)

3. Read `documentation/github-issues.md` for concept context — which diagnoses, medications,
   labs, or other concepts are described in the study scope.

4. **STEP 1 — Discovery query**
   - Generate the keyword or pattern search query against the appropriate CDW lexis dimension
     (e.g., `cdw_outpost.lexis.dim_dx` for diagnoses).
   - Present the query to the user. Do not run it.
   - Tell the user: "Send this query to your PI. Ask them to mark `desired = 'TRUE'` on
     rows to include and assign categories. While you wait, run `/cdw-sql-work` to start
     on `patient.sql` — it has no ss-file dependency."

5. **STEP 2 — Email to PI (optional)**
   Ask the user: "Would you like me to draft an email to the PI with the query attached?"
   If yes, read `ai/pi-email-templates.md` and use the template matching the concept type.
   Fill in `[Last Name]` and `[Study Name]` from the project context.

6. **STEP 3 — Load (when PI returns annotated results)**
   - Generate the `INSERT` statement to populate `{schema_name}.ss_{type}` from the
     annotated results.
   - Write the finished script to `manipulation/ss/ss-{type}-create.sql`.
   - Remind the user: "`ss-` files require PI sign-off before running against live data."
   - Tell the user: "Return to `/cdw-sql-work` — scripts that depend on `ss_{type}` can
     now be customized."

## Style Notes

- Minimum necessary fields for SQL joins and PI review — do not add extra categories unless needed.
- Never add `ss-` tables to `tables_to_scribe` in `config.yml`.
- Search sibling repos for examples if the pattern is unclear (see `ai/sql-templates.md`).
