# /project-orient

Full project briefing for a new contributor or anyone returning after a gap.
Unlike `/session-start`, this always reads source documents fresh and produces a
structured handoff — not just a quick orient.

## Steps

1. Read `ai/safety-rules.md`.

2. Read `config.yml` — extract `project_name` and `schema_name`.

3. Read `documentation/github-issues.md` in full (bypass `ai/ai-state.md` even if fresh).

4. Read the two most recent files in `documentation/ai-sessions/` to understand recent history.

5. Use the Agent tool to spawn an Explore subagent:
   > "List all `.sql` files in `manipulation/` (exclude `templates/`, `sweep-and-specify/`).
   > For each, read the first 20 lines. Return:
   > - File name
   > - Stated purpose
   > - Output table name(s)
   > - Status: populated / stub / empty"

6. Produce a structured briefing:

   **Study summary** — research question and aim in 2–3 sentences
   **Inclusion criteria** — bulleted list
   **Data pipeline** — which scripts exist, their status, what they produce
   **Concept-set lookups** — which `ss-` tables are needed; which are done / pending PI review
   **Recent work** — what happened in the last 1–2 sessions (from audit logs)
   **Open items** — blockers, unfinished tasks, decisions needed
   **Suggested first task** — one concrete action the new contributor should take

7. Ask the user: "Does this match your understanding, or is anything out of date?"

8. Overwrite `ai/ai-state.md` with current state (format in `ai/session-logging.md`).

## When to use

- First session on a project you haven't touched before
- Returning after >2 weeks away
- Handing off to another contributor
- Any time `ai/ai-state.md` feels unreliable
