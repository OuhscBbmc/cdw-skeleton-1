# /sql-work

Walk through existing SQL scripts in sequence, reviewing and editing each one.
Run after `/sql-inventory` (or `/sql-generate` for a fresh project).

## Steps

1. Read `ai/sql-style.md` once. Distill it into a compact review checklist — 12–15 bullets
   covering the most commonly violated rules. You will pass this checklist (not the full
   style guide) to each subagent. Example rules to include:
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

2. Get the script list from `/sql-inventory` (run it now if not done this session).

3. For each script in the order they appear in `flow.R`:

   a. Use the Agent tool to spawn a subagent with this prompt:

      > "Read the full contents of `[file path]`. Check every line against this style
      > checklist. Return ONLY a bulleted list of specific violations with line numbers.
      > Do not rewrite the file.
      >
      > [paste your distilled checklist here]"

   b. Present the subagent's findings to the user.

   c. Wait for user input:
      - **Confirm / yes** → apply the listed edits with the Edit tool. Move to next script.
      - **Skip** → note it and move on.
      - **Specific instruction** → apply that instead, then move on.

4. After all scripts: "All scripts reviewed. Run `/ss-create [type]` or `/session-end`."

## Notes

- One subagent per script — do not batch multiple scripts into one call.
- Apply edits in the main agent (not inside the subagent) so the user can see each change.
- If a script is a stub with no SQL yet, note it and skip.
