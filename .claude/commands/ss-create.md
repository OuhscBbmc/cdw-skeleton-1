# /ss-create

Build a study-specific concept-set lookup table. Usage: `/ss-create [dx|med|lab|location|obs|cpt]`

## Steps

1. Read `ai/sql-style.md`.

2. Read the existing placeholder in `manipulation/ss/` for the requested type.
   (If no placeholder exists, note it and proceed from scratch.)

3. Read `documentation/github-issues.md` for concept context — which diagnoses, medications,
   labs, or other concepts are described in the study scope.

4. **STEP 1 — Discovery query**
   - Generate the keyword or pattern search query against the appropriate CDW lexis dimension
     (e.g., `cdw_outpost.lexis.dim_dx` for diagnoses).
   - Present the query to the user. Do not run it.
   - Tell the user: "Run STEP 1, review the results, mark `desired = 'TRUE'` on rows to include,
     and assign categories. Come back for STEP 2."

5. **STEP 2 — Load (when user returns with annotations)**
   - Generate the `INSERT` statement to populate `{schema_name}.ss_{type}` from the annotated results.
   - Write the finished script to `manipulation/ss/ss-{type}-create.sql`.
   - Remind the user: "`ss-` files require PI sign-off before running against live data."

## Style Notes

- Minimum necessary fields for SQL joins and PI review — do not add extra categories unless needed.
- Never add `ss-` tables to `tables_to_scribe` in `config.yml`.
- Search sibling repos for examples if the pattern is unclear (see `ai/sql-templates.md`).
