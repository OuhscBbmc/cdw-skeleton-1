# /cdw-ai-update

Pulls the latest versions of all AI workflow files from `cdw-skeleton-1` and overwrites
them locally. Use this when the skeleton repo has been updated and you want to bring a
project repo current.

**Never overwrites** `ai/ai-state.md` — that file is project-specific.

## Files updated

| Local path | Source |
|---|---|
| `AGENTS.md` | cdw-skeleton-1/main |
| `ai/README.md` | cdw-skeleton-1/main |
| `ai/safety-rules.md` | cdw-skeleton-1/main |
| `ai/sql-style.md` | cdw-skeleton-1/main |
| `ai/sql-templates.md` | cdw-skeleton-1/main |
| `ai/r-style.md` | cdw-skeleton-1/main |
| `ai/pi-email-templates.md` | cdw-skeleton-1/main |
| `.claude/commands/cdw-orient.md` | cdw-skeleton-1/main |
| `.claude/commands/cdw-plan.md` | cdw-skeleton-1/main |
| `.claude/commands/cdw-sql-scaffold.md` | cdw-skeleton-1/main |
| `.claude/commands/cdw-ss-build.md` | cdw-skeleton-1/main |
| `.claude/commands/cdw-sql-work.md` | cdw-skeleton-1/main |
| `.claude/commands/cdw-end-session.md` | cdw-skeleton-1/main |
| `.claude/commands/cdw-ai-update.md` | cdw-skeleton-1/main |
| `.vscode/tasks.json` | cdw-skeleton-1/main |
| `utility/run-python.ps1` | cdw-skeleton-1/main |
| `utility/populate-scripts.py` | cdw-skeleton-1/main |
| `utility/export-repo-issues.py` | cdw-skeleton-1/main |

## Steps

1. For each file in the table above, fetch from:
   `https://raw.githubusercontent.com/OuhscBbmc/cdw-skeleton-1/main/[local path]`

   Use WebFetch for each. Write the result directly to the local path with the Write tool.
   Do not diff or summarize content — just overwrite silently.

2. After all files are written, run:
   ```
   git diff --name-only
   ```
   and report which files actually changed (i.e., had different content from what was on disk).

3. Update the `skeleton_synced` line in `ai/ai-state.md` to today's date. If `ai-state.md`
   doesn't exist yet, skip this step. Edit only that one line — do not touch anything else.

4. Append an incremental log entry to `documentation/ai-sessions/YYYY-MM-DD-{user}.md`
   (create file and folder if missing):
   ```
   ## /cdw-ai-update — YYYY-MM-DD HH:MM
   Files updated from skeleton: [list, or "none — already up to date"]
   skeleton_synced: YYYY-MM-DD
   ```

5. If any files changed, say:
   "Updated: [list]. Run `/cdw-orient` to confirm everything looks right before continuing."

6. If nothing changed, say:
   "All AI files are already up to date." Still update `skeleton_synced`.

## Notes

- Fetch all files in parallel where possible.
- Do NOT update `ai/ai-state.md` — it holds project-specific state that would be destroyed.
- If a fetch fails (404, network error), report it by name and continue with the rest.
- This command updates itself (`cdw-ai-update.md`) — that's intentional.
