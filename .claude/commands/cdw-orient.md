# /cdw-orient

Orient to the current project state. Runs in quick or full mode depending on how fresh
the cached state is. Replaces the need for a separate session-start command.

## Steps

1. Read `ai/safety-rules.md` and `config.yml` (always).

2. Check two freshness conditions:
   - **ai-state fresh?** — read `ai/ai-state.md`, check `Last updated:` < 7 days old
   - **issues fresh?** — read `documentation/github-issues.md`, check `Generated:` < 7 days old

3. **Quick mode** (ai-state fresh AND issues fresh):

   a. Read `ai/ai-state.md`. Report in 3 bullets: study aim, current focus, next steps.

   b. Tell user what to run next based on state:
      - No scripts yet → "Run `/cdw-plan` then `/cdw-sql-scaffold`."
      - Scripts scaffolded, work in progress → "Run `/cdw-sql-work`. Blocked: [list scripts waiting on ss-files]."
      - Waiting on PI for ss-files → "Run `/cdw-ss-build [type]` when results arrive, then `/cdw-sql-work`."
      - All scripts active → "Run `/cdw-end-session` or tell me what to work on."

4. **Full mode** (ai-state missing, stale, OR issues stale):

   a. If `documentation/github-issues.md` is missing or > 7 days old, refresh it:
      ```
      python utility/export-repo-issues.py
      ```
      Use the Python fallback from AGENTS.md if `python` is not on PATH.

   b. Read `documentation/github-issues.md` in full.

   c. Read the two most recent files in `documentation/ai-sessions/`.

   d. Spawn an Explore subagent:
      > "List all `.sql` files in `manipulation/` (exclude `templates/`, `sweep-and-specify/`).
      > For each, read the first 20 lines. Return: file name, stated purpose, output table(s),
      > status (populated / stub / empty)."

   e. Produce a structured briefing:
      - **Study summary** — research question and aim in 2–3 sentences
      - **Inclusion criteria** — bulleted list
      - **Data pipeline** — scripts, status, what they produce, GitHub issue # per script
      - **Concept-set lookups** — ss-tables needed; done / pending PI review
      - **Recent work** — last 1–2 sessions
      - **Open items** — blockers, decisions needed
      - **Suggested next task** — one concrete action

   f. Ask: "Does this match your understanding, or is anything out of date?"

   g. Overwrite `ai/ai-state.md` (format in AGENTS.md → *ai-state.md Format*).

## Notes

- Use full mode any time you want a complete re-anchor, regardless of freshness.
  Just say "full orient" to skip the freshness check.
- Do not load `ai/sql-style.md` or any SQL files here.
