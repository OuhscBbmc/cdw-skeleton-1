# /cdw-plan

Translate meeting notes into a confirmed data plan: which scripts to scaffold and which
ss-files to build. Run after `/cdw-start` on a new project, before `/cdw-sql-scaffold`.

## Steps

1. Find the initial meeting issue in `documentation/github-issues.md`. The title always
   contains "initial meeting" — scan headers only to locate it, then read just that
   issue's section. If `github-issues.md` is missing, run `utility/export-repo-issues.py`
   first. Note the issue number — it will be written to `ai-state.md` as `planning_issue`.

2. Extract and structure the study's data asks:
   - **Patient population** — inclusion/exclusion criteria, date range
   - **Diagnoses** — conditions or ICD code families needed
   - **Medications** — drug names, classes, or NDC families
   - **Labs** — test names or LOINC families
   - **Encounters/locations** — clinic types, visit types
   - **Other** — CPT codes, observations, REDCap integration

3. Map each ask to a concrete deliverable:

   | Data ask | Script | ss-file needed | Can start immediately? |
   |----------|--------|----------------|------------------------|
   | Patient pool | `patient.sql` | None | Yes |
   | Diagnoses | `dx.sql` | `ss_dx` | No — needs PI review |
   | Medications | `medication-[source].sql` | `ss_med` | No — needs PI review |
   | Labs | `lab.sql` | `ss_lab` | No — needs PI review |
   | etc. | | | |

4. Present the plan. Ask: "Does this capture everything, or are there asks I missed?"
   Wait for confirmation or corrections before proceeding.

5. Update `ai/ai-state.md`: read the current file if it exists, add or overwrite
   `planning_issue` and `Data asks:`, preserve all other fields, rewrite the whole file.

6. Append an incremental log entry to `documentation/ai-sessions/YYYY-MM-DD-{user}.md`
   (create file and folder if missing):
   ```
   ## /cdw-plan — YYYY-MM-DD HH:MM
   planning_issue: #N
   Data asks confirmed: [one-line list]
   Files changed: ai/ai-state.md
   ```

7. Tell the user: "Plan confirmed. Run `/cdw-sql-scaffold` to pull script templates and
   open a GitHub tracking issue for each, then `/cdw-ss-build [type]` for each ss-file
   needed while you work on scripts with no ss-dependency."

## Notes

- Do not infer asks that aren't in the meeting notes — ask if unclear.
- If no meeting issue exists, ask the user to summarize the data asks before proceeding.
