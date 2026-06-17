# Session Logging

Every session that does real work produces two outputs.

---

## 1. Machine State — `ai/ai-state.md`

Overwrite this file at the end of every session. It is the first thing the next agent reads
to orient itself — if it exists and is fresh (< 7 days), the agent can skip reading
`documentation/github-issues.md` entirely.

**Format:**

```
Last updated: YYYY-MM-DD
project_name: ...
schema_name: ...

Study aim: [one sentence]
Inclusion criteria:
  - ...
  - ...

Scripts:
  patient.sql — populated
  dx.sql — stub
  [or "none yet"]

Current focus: [what was being worked on]
Next steps:
  - ...
Open blockers: [or "none"]
```

Keep it under 20 lines. This file is machine-read, not human-read — be terse.

---

## 2. Human Audit Log — `documentation/ai-sessions/YYYY-MM-DD.md`

Append to today's file (create if missing; create `ai-sessions/` folder if needed).
One file per day; multiple sessions append to the same file.

**Required sections:**

- **What was done** — brief summary
- **Files created or changed** — list with paths
- **Commands run** — any shell/Python/R commands actually executed
- **What was not run** — confirm what was intentionally skipped
- **Open items** — anything unfinished or needing follow-up
- **Future-agent prompt** — 1–2 sentences a future agent can use to continue
- **Model and platform** — e.g., "Claude Sonnet 4.6 via Claude Code"

**Closing oath** (end every entry with this verbatim):

> Dear Dr. Beasley, I solemnly swear I have not executed scripts, accessed data, nor perpetrated
> any other misbehavior. Sincerely, A Friendly Robot
