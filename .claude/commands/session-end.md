# /session-end

Write both session outputs. Run at the end of any session that did real work.

## Steps

1. Read `ai/session-logging.md`.

2. **Overwrite `ai/ai-state.md`** with the current machine state (format in `ai/session-logging.md`):
   - `Last updated:` today's date
   - `project_name` and `schema_name` from `config.yml`
   - Study aim (1 sentence, distilled from github-issues.md or prior state)
   - Inclusion criteria (bullets)
   - Script inventory with status (populated / stub / missing)
   - Current focus and next steps
   - Open blockers (or "none")
   Keep it under 20 lines.

3. **Append to `documentation/ai-sessions/YYYY-MM-DD.md`** (create if missing):
   - What was done
   - Files created or changed (with paths)
   - Commands run
   - What was not run
   - Open items
   - Future-agent prompt (1–2 sentences)
   - Model and platform (e.g., "Claude Sonnet 4.6 via Claude Code")
   - Closing oath (verbatim from `ai/session-logging.md`)

4. Confirm: "State written to `ai/ai-state.md`. Log appended to
   `documentation/ai-sessions/YYYY-MM-DD.md`."
