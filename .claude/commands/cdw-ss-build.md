# /cdw-ss-build

Build a study-specific concept-set lookup table. Usage: `/cdw-ss-build [dx|med|lab|location|obs|cpt]`

Run after `/cdw-sql-scaffold`. This is non-blocking — generate the discovery query, send
it to the PI, then immediately continue with `/cdw-sql-work` on `patient.sql` while you wait.

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
   - Tell the user: "Send this query to your PI. Ask them to mark `desired = 'TRUE'` on
     rows to include and assign categories. While you wait, run `/cdw-sql-work` to start
     on `patient.sql` — it has no ss-file dependency."

5. **STEP 2 — Craft eamil to PI**
  Aks the user if they would like to craft an email to the study team use this as a template 
  
 - For diangosis codes use this below
  Dear Dr. [Last Name],
    We are moving forward with the data extraction for the [Study Name] and need your input before we can proceed.
    We have generated a list of diagnosis codes (ICD-9 and ICD-10) that may be relevant to your study. Please review the attached spreadsheet and confirm which codes should be included in the data extraction.
    What we need from you

    Open the attached spreadsheet.
    Review the diagnosis codes listed.
    In the Desired column, change any code from TRUE to FALSE if it should be excluded from the study.

    All codes are currently marked TRUE.
    You only need to update the codes that should not be included.


    (Optional) Update the Category column if a code is incorrectly categorized.
    Reply to this email with the updated spreadsheet attached.

    Notes

    If all listed codes are appropriate for the study, no changes are necessary.
    Please feel free to add comments or notes in the spreadsheet if any codes require clarification.

    Thank you for your assistance. Once we receive the reviewed file, we will proceed with the data extraction.

- For CPT codes use this 
    Dear Dr. [Last Name],
    We are moving forward with the data extraction for [Study Name] and need your input before we can proceed.
    We have generated a list of CPT codes that may be relevant to your study. Please review the attached spreadsheet and confirm which codes should be included in the data extraction.
    What we need from you

    Open the attached spreadsheet.
    Review the CPT codes listed.
    In the Desired column, change any code from TRUE to FALSE if it should be excluded from the study.

    All codes are currently marked TRUE.
    You only need to update the codes that should not be included.


    (Optional) Update the Category column if a code is incorrectly categorized (e.g., office visit, laboratory, pathology, surgery, imaging, infusion, chemotherapy, radiation oncology, etc.).
    Reply to this email with the updated spreadsheet attached.

    Notes

    If all listed CPT codes are appropriate for the study, no changes are necessary.
    If there are CPT codes that should be added but are not currently listed, please include them in the spreadsheet or note them in your reply.
    Feel free to add comments or notes for any codes that may require clarification.

    Thank you for your assistance. Once we receive the reviewed file, we will proceed with the data extraction.

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
