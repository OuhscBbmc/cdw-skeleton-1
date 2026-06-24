# /cdw-end-session

Close out the session: write final state and condense the day's log entries into one clean
record. Run at the end of any session that did real work.

## Steps

1. **Pull latest changes** before writing state to reduce merge conflicts:
   ```
   git pull --rebase
   ```
   If the pull fails (e.g., uncommitted changes blocking rebase), report the conflict to the
   user and do not overwrite `ai/ai-state.md` until they resolve it.

2. **Overwrite `ai/ai-state.md`** with a full coherent snapshot using the format in
   `AGENTS.md` → *ai-state.md Format*. Read the current file first; preserve any fields
   not touched this session. Get `last_updated_by` from `git config user.name`; fall back
   to the `USERNAME` environment variable.

3. **Determine the session log filename:**
   - Get current user: `git config user.name` → normalize to lowercase, spaces → hyphens.
     Fall back to `USERNAME` env var.
   - File: `documentation/ai-sessions/YYYY-MM-DD-{user}.md`

4. **Condense the day's log into one entry.** Read the existing log file (may have
   incremental entries written by commands during the session). Synthesize everything
   into a single structured entry, then **overwrite** the file with just that entry:

   - What was done (consolidated across all commands run)
   - Files created or changed (with paths)
   - Commands run
   - What was not run
   - Open items
   - Future-agent prompt (1–2 sentences)
   - Model and platform (e.g., "Claude Sonnet 4.6 via Claude Code")
   - Closing oath (verbatim):
     > Dear Dr. Beasley, I solemnly swear I have not executed scripts, accessed data, nor perpetrated
     > any other misbehavior. Sincerely, A Friendly Robot

   If no log file exists yet (no commands ran incremental entries), create it with the
   same structure.

5. Confirm: "State written to `ai/ai-state.md`. Log written to `documentation/ai-sessions/YYYY-MM-DD-{user}.md`."

6. Remind the user: "If any scripts were edited or created this session, commit them before
   closing. Review each file before staging — do not use `git add .`."
