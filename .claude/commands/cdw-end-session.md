# /cdw-end-session

Write both session outputs. Run at the end of any session that did real work.

## Steps

1. **Pull latest changes** before writing state to reduce merge conflicts:
   ```
   git pull --rebase
   ```
   If the pull fails (e.g., uncommitted changes blocking rebase), report the conflict to the
   user and do not overwrite `ai/ai-state.md` until they resolve it.

2. **Overwrite `ai/ai-state.md`** using the format in `AGENTS.md` → *ai-state.md Format*.
   Include a `last_updated_by:` line (see format). Get the value from `git config user.name`;
   fall back to the `USERNAME` environment variable if git config is empty.

3. **Determine the session log filename:**
   - Get current user: `git config user.name` → normalize to lowercase, spaces → hyphens
     (e.g., "Grace Cruz" → `grace-cruz`). Fall back to `USERNAME` env var.
   - File: `documentation/ai-sessions/YYYY-MM-DD-{user}.md`
   - Create the file and folder if missing; append if the file already exists (same user
     running multiple times in one day is fine — just append a new dated entry).

4. **Append to the session log file:**

   - What was done
   - Files created or changed (with paths)
   - Commands run
   - What was not run
   - Open items
   - Future-agent prompt (1–2 sentences)
   - Model and platform (e.g., "Claude Sonnet 4.6 via Claude Code")
   - Closing oath (verbatim):
     > Dear Dr. Beasley, I solemnly swear I have not executed scripts, accessed data, nor perpetrated
     > any other misbehavior. Sincerely, A Friendly Robot

5. Confirm: "State written to `ai/ai-state.md`. Log appended to `documentation/ai-sessions/YYYY-MM-DD-{user}.md`."
