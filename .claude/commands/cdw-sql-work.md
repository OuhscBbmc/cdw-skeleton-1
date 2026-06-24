# /cdw-sql-work

Customize SQL scripts in dependency order, activating each in `flow.R` when done.
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

2. Read `flow.R` and `ai/ai-state.md`. Collect: data plan, date ranges, inclusion criteria,
   script→issue# map. Check `manipulation/ss/` for which ss-files exist.

3. **Determine script execution order** based on data dependencies — not a fixed sequence.
   Read the first 20 lines of each script stub to understand its output table and what it
   filters on. Then reason through the dependency graph:

   - A script that defines the patient cohort (e.g., `patient.sql`) must run before any
     script that filters `on patient_id` from that cohort — unless the downstream script
     rebuilds that filter itself.
   - If patient inclusion depends on a clinical event (visit, encounter, diagnosis, etc.),
     the script that defines that event may need to run first, or its logic may belong as a
     CTE inside `patient.sql`. Choose based on whether the downstream script needs the full
     event detail or just the inclusion flag:
     - **Full detail needed later** (e.g., encounter dates, types, providers): build the
       encounter script first so `patient.sql` can join to it.
     - **Inclusion only, no detail needed** (e.g., "must have had any visit"): inline a CTE
       in `patient.sql` rather than creating a separate encounter script for this purpose.
       If encounter detail is requested separately, pull it in a later script filtered to the
       already-built cohort.
   - Scripts that only filter on `patient_id` from the cohort table have no inter-script
     dependency and can run in any order after the cohort is defined.
   - ss-dependent scripts are blocked until their ss-file exists.

   Present the proposed order and rationale to the user before proceeding. If the dependency
   graph is ambiguous, ask — don't assume.

4. Partition scripts:
   - **Ready now** — dependencies met (upstream scripts populated, ss-file exists if needed)
   - **Blocked** — waiting on upstream script or PI ss-file

   Report both groups.

5. Work through **ready** scripts in the order determined in step 3:

   a. Spawn a subagent:
      > "Read the full contents of `[file path]`. Identify:
      > 1. Placeholders needing study-specific input (dates, WHERE conditions,
      >    inclusion criteria, ss- table join references)
      > 2. Style violations against this checklist: [paste checklist]
      > Return two bulleted lists with line numbers. Do not rewrite the file."

   b. Present findings. For each placeholder, either propose a specific value drawn
      directly from the data plan or issues, or — if the right value isn't clear from
      the available context — ask explicitly. Do not guess at clinical values (date
      ranges, encounter types, inclusion criteria, code categories) without including a comment to remind user to verify clinical values with a SME. A wrong assumption
      here propagates silently into results.

   c. Wait for user confirmation or corrections before making any edits.

   d. Apply all edits with the Edit tool.

   e. Uncomment this script's entry in `flow.R`.

   f. Comment on the script's GitHub issue — but first check for an existing activation
      comment to avoid duplicates:
      ```
      gh issue view [number] --json comments --jq '.comments[].body' | grep -q "activated in flow.R"
      ```
      If the grep returns nothing, post the comment:
      ```
      gh issue comment [number] --body "Script customized and activated in flow.R."
      ```
      If it already exists, skip silently.

   g. Confirm: "`[script]` done." Move to next ready script.

6. After all ready scripts, report blocked scripts:
   "Waiting on PI for: `ss_dx` (blocks `dx.sql`), `ss_med` (blocks `medication.sql`).
   Run `/cdw-ss-build [type]` when annotated results arrive, then re-run `/cdw-sql-work`."

7. When all scripts are done: "All scripts activated. Run `/cdw-end-session`."

## Notes

- One subagent per script — do not batch.
- Apply all edits in the main agent so the user can review each change.
- If a script is already populated and uncommented, note it and skip.
