# /cdw-sql-work

Customize SQL scripts in dependency order, activating each in `flow.R` when done.
Starts with scripts that have no ss-file dependency (`patient.sql` first), then resumes
on ss-dependent scripts as PI returns annotated concept sets.

Safe to run multiple times — picks up where it left off.

## Steps

1. Read `ai/sql-style.md` once. Distill into a compact checklist — 12–15 bullets:
   - SELECT/FROM/WHERE/GROUP BY/HAVING/INSERT/DECLARE/CREATE TABLE/DROP TABLE uppercase; all other keywords lower
   - Each SELECT column on its own line with leading comma
   - Joins indented under FROM; extra space in `left  join`
   - Multi-condition joins: each condition and operator on its own line
   - CREATE TABLE: pad column names and types for alignment; trailing comma on last column
   - DECLARE: pad names, types, values, and comments for alignment
   - No column list in INSERT — `INSERT table` then `SELECT` directly
   - All CREATE TABLE before first SELECT in file
   - `use cdw_cache_staging;` at top of staging files
   - 2-space indentation; no tabs
   - No trailing whitespace; file ends with newline; no triple blank lines

2. Read `flow.R` for script order. Read `ai/ai-state.md` for the data plan, date ranges,
   inclusion criteria, and the script→issue# map (e.g. `patient.sql — stub — #14`).
   Check `manipulation/ss/` to see which ss-files have been returned and built.

3. Partition scripts into two groups:
   - **Ready now** — no ss-dependency, or ss-file already exists in `manipulation/ss/`
   - **Blocked** — ss-file dependency not yet returned by PI

   Report both groups to the user before proceeding.

4. Work through **ready** scripts in `flow.R` order (`patient.sql` first):

   a. Spawn a subagent:
      > "Read the full contents of `[file path]`. Identify:
      > 1. Placeholders needing study-specific input (dates, WHERE conditions,
      >    inclusion criteria, ss- table join references)
      > 2. Style violations against this checklist: [paste checklist]
      > Return two bulleted lists with line numbers. Do not rewrite the file."

   b. Present findings. Propose specific values for each placeholder based on the data
      plan and issues.

   c. Wait for user confirmation or corrections.

   d. Apply all edits with the Edit tool.

   e. Uncomment this script's entry in `flow.R`.

   f. Comment on the script's GitHub issue:
      ```
      gh issue comment [number] --body "Script customized and activated in flow.R."
      ```

   g. Confirm: "`[script]` done." Move to next ready script.

5. After all ready scripts, report blocked scripts:
   "Waiting on PI for: `ss_dx` (blocks `dx.sql`), `ss_med` (blocks `medication.sql`).
   Run `/cdw-ss-build [type]` when annotated results arrive, then re-run `/cdw-sql-work`."

6. When all scripts are done: "All scripts activated. Run `/cdw-end-session`."

## Notes

- One subagent per script — do not batch.
- Apply all edits in the main agent so the user can review each change.
- If a script is already populated and uncommented, note it and skip.
