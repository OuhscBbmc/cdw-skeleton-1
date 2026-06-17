# /sql-inventory

Inventory the SQL scripts that already exist in this project. Run after `/session-start`.

## Steps

Use the Agent tool to spawn an Explore subagent with the following task:

> "List all `.sql` files in `manipulation/` that are not inside `templates/`,
> `sweep-and-specify/`, or `ss/`. For each file:
> - Read the first 15 lines (header comment block).
> - Extract: sequence number (from filename prefix), file name, stated purpose, and output table name(s).
> - Determine status: **populated** if the file contains actual SQL beyond placeholder comments;
>   **stub** if it contains only comment blocks or `-- TODO` lines.
>
> Return a markdown table with columns: `#` | `File` | `Purpose` | `Output Table(s)` | `Status`"

Post the subagent's table directly into the session.

Then say:
"Run `/sql-work` to walk through these scripts in sequence, `/ss-create [type]` to build a
study-specific lookup table, or tell me which script to jump to."

## What NOT to do here

- Do not read `ai/sql-style.md` or any template files.
- Do not suggest edits yet — inventory only.
