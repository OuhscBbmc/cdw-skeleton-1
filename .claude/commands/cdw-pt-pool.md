# /cdw-pt-pool

Customize SQL scripts in dependency order, activating each in `flow.R` when done.
Safe to run multiple times — picks up where it left off.

## Steps

1. Read `ai/safety-rules.md` and `config.yml` (always).

2. Check two freshness conditions:
   - **ai-state fresh?** — read `ai/ai-state.md`, check `Last updated:` < 7 days old
   - **issues fresh?** — read `documentation/github-issues.md`, check `Generated:` < 7 days old

3. **Full mode** (ai-state missing, stale, OR issues stale):

   a. If `documentation/github-issues.md` is missing or > 7 days old, refresh it:
      ```
      python utility/export-repo-issues.py
      ```
      Use the Python fallback from AGENTS.md if `python` is not on PATH.

   b. Read `documentation/github-issues.md` in full.

   c. Read the two most recent files in `documentation/ai-sessions/`.


   e. Produce a structured briefing:
      - **Study summary** — research question and aim in 2–3 sentences
      - **Inclusion criteria** — bulleted list
      - **Data pipeline** — scripts, status, what they produce, GitHub issue # per script
      - **Concept-set lookups** — ss-tables needed; done / pending PI review
      - **Recent work** — last 1–2 sessions
      - **Open items** — blockers, decisions needed
      - **Suggested next task** — one concrete action

Task:

1. Load the spreadsheet:
   S:\BBMC\prairie-outpost\crdw-research-code-generator\data-private\inclusion_scripts.xlsx

2. Read the following columns:
   - inclusion_criteria
   - script_content

3. Compare the current project's inclusion criteria against all records in the spreadsheet.

4. Identify the existing inclusion criteria that are most semantically similar to the project's inclusion criteria.

5. Using the corresponding script_content from the best-matching records, generate a pt_pool script for the current project.

6. When creating the pt_pool:
   - Prioritize the highest-similarity matches.
   - Combine relevant logic from multiple matches when appropriate.
   - Remove duplicate or conflicting language.
   - Preserve study-specific requirements from the current project's inclusion criteria.
   - Output only the final cleaned pt_pool script.
