# /cdw-end-session

Write both session outputs. Run at the end of any session that did real work.

## Steps

1. **Overwrite `ai/ai-state.md`** using the format in `AGENTS.md` → *ai-state.md Format*.

2. **Append to `documentation/ai-sessions/YYYY-MM-DD.md`** (create file and folder if missing):

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

3. Confirm: "State written to `ai/ai-state.md`. Log appended to `documentation/ai-sessions/YYYY-MM-DD.md`."
