# /cdw-ss-build

Build a study-specific concept-set lookup table. Usage: `/cdw-ss-build [dx|med|lab|location|obs|cpt]`

Run after `/cdw-sql-scaffold`. This is non-blocking — write the ss-file, run it to generate
a CSV for team review, then immediately continue with `/cdw-sql-work` on `patient.sql` while
you wait for the team's feedback.

## Steps

1. Read `ai/sql-style.md` and the existing placeholder in `manipulation/ss/placeholder/` for
   the requested type. (If no placeholder exists, note it and proceed from scratch.)

2. Read `ai/ai-state.md` for study aim and inclusion criteria. Then find `planning_issue`
   in state and read only that issue's section from `documentation/github-issues.md` for
   clinical concept detail (drug classes, diagnosis groupings, lab names, etc.). If
   `planning_issue` is missing, ask the user which issue has the original meeting notes.

3. **STEP 1 — Write the ss-file**
   - Search sibling repos under the GitHub directory for an existing `ss-{type}-create.sql`
     to use as a pattern reference.
   - Write `manipulation/ss/ss-{type}-create.sql` using `SELECT ... INTO {schema}.ss_{type}`
     directly from the appropriate CDW lexis dimension (e.g., `cdw_outpost.lexis.dx`).
   - The WHERE clause should cover:
     - All relevant code ranges and families (ICD codes, NDC, LOINC, CPT, etc.)
     - Keyword searches on description fields for the condition or drug names in plain English
       (e.g., `icd_description like '%cardiac arrest%'`) — do not rely on codes alone
   - If the study needs to distinguish between subtypes (e.g., different arrest rhythms,
     drug classes, complication types), include a CASE statement assigning a best-guess
     `category` using plain-English values. Omit the category column entirely when a
     single concept group is being captured and no subtype distinction is needed.
   - Add inline comments on every WHERE condition and every CASE branch explaining what
     the code range means in plain English.
   - Tell the user: "Run `ss-{type}-create.sql`, then export `{schema}.ss_{type}` to a CSV
     and send it to the team for review. Ask them to confirm the category assignments and flag
     any codes that should be removed or added. While you wait, run `/cdw-sql-work` to start
     on scripts with no ss-dependency."

4. **STEP 2 — Email to team (optional)**
   Ask the user: "Would you like me to draft an email to the team with the CSV attached?"
   If yes, read `ai/pi-email-templates.md` and use the template matching the concept type.
   Fill in `[Last Name]` and `[Study Name]` from the project context. Attach instructions
   to review category assignments and note any missing codes — do not include the SQL query.

5. **STEP 3 — Update (when team returns annotated results)**
   - Read the team's feedback. Update the WHERE clause and CASE categories in the existing
     `manipulation/ss/ss-{type}-create.sql` to reflect their corrections.
   - Do not create a new file — edit the one already in the repo.
   - Update `ai/ai-state.md`: read the current file, mark any scripts that were waiting on
     this ss-file as unblocked. Preserve all other fields. Rewrite the whole file.
   - Append an incremental log entry to `documentation/ai-sessions/YYYY-MM-DD-{user}.md`:
     ```
     ## /cdw-ss-build {type} — YYYY-MM-DD HH:MM
     ss-file updated: manipulation/ss/ss-{type}-create.sql
     Unblocked scripts: [list]
     Files changed: manipulation/ss/ss-{type}-create.sql, ai/ai-state.md
     ```
   - Remind the user: "`ss-` files require team sign-off before running against live data."
   - Tell the user: "Return to `/cdw-sql-work` — scripts that depend on `ss_{type}` can
     now be customized."

## Style Notes

- Use `SELECT ... INTO {schema}.ss_{type}` — not CREATE TABLE + INSERT, not a VALUES block.
- Minimum necessary fields for SQL joins and team review.
- Never add `ss-` tables to `tables_to_scribe` in `config.yml`.
- Always include both code-range filters AND plain-English keyword searches in the WHERE clause.
- Every CASE branch and WHERE condition must have an inline comment explaining what it means.
