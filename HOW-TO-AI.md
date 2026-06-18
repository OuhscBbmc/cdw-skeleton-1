# Using AI With This Repo

This repo includes tools that let you use any AI assistant to help pull GitHub issues,
scaffold SQL scripts, and understand the project context. This guide covers setup and
day-to-day use.

---

## One-Time Setup: GitHub Token

To pull issues from private repos you need a GitHub personal access token. This takes
about two minutes and you only do it once per computer.

1. Go to https://github.com/settings/tokens and click **Generate new token (classic)**
2. Give it a name like `CRDW issue export`
3. Check the **repo** scope (top-level checkbox)
4. If prompted to authorize for OuhscBbmc SSO, do that too
5. Copy the token (it won't be shown again)
6. Set it as an environment variable on your machine:

**Windows (PowerShell — run once, persists forever):**
```powershell
[System.Environment]::SetEnvironmentVariable("GITHUB_TOKEN", "your-token-here", "User")
```

After that, close and reopen VS Code or any terminal for it to take effect. If you have
the GitHub CLI installed (`gh`), it will use that automatically as a fallback.

---

## Pulling GitHub Issues

Issues are pulled into `documentation/github-issues.md` — a readable file that shows the
research question, inclusion criteria, meeting notes, and current task status.

**In VS Code (no terminal needed):**
Press `Ctrl+Shift+P`, type **Pull GitHub Issues**, press Enter.

**In a terminal:**
```
python utility/export-repo-issues.py
```
Fallback: `py utility/export-repo-issues.py`
Windows fallback:
```powershell
& "$env:LOCALAPPDATA\Microsoft\WindowsApps\PythonSoftwareFoundation.Python.3.11_qbz5n2kfra8p0\python.exe" utility\export-repo-issues.py
```

Re-run any time issues are updated on GitHub.

---

## Populating SQL Scripts

Fetches SQL templates for your project and places them in `manipulation/` with your
project schema substituted in. You can also let Claude Code do this automatically with
`/sql-generate` (see below).

**In VS Code:** `Ctrl+Shift+P` → **Populate CRDW Scripts**

**In a terminal:**
```
python utility/populate-scripts.py
```
For specific templates without the menu:
```
python utility/populate-scripts.py --templates patient dx medication-meditech
```

Review and customize each script before running — especially dates, WHERE clauses, and
inclusion criteria.

---

## Using Your AI Tool

### Claude Code

```
claude
```

Reads `AGENTS.md` automatically. Use the slash commands for a structured, token-efficient
session:

| Command | What it does |
|---|---|
| `/cdw-orient` | Full briefing for a new contributor or returning after a gap |
| `/cdw-start` | Quick session open: load safety rules, check project state |
| `/cdw-plan` | New project — translate meeting notes into a confirmed data plan |
| `/cdw-sql-scaffold` | Pull script templates; creates a GitHub issue per script |
| `/cdw-ss-build [type]` | Generate discovery query, send to PI (non-blocking) |
| `/cdw-sql-work` | Customize scripts; starts patient.sql now, resumes others as ss-files return |
| `/cdw-end-session` | Write machine state to `ai/ai-state.md`; append human audit log |

**New project:** `/cdw-start` → `/cdw-plan` → `/cdw-sql-scaffold` → `/cdw-ss-build [type]` → `/cdw-sql-work` → `/cdw-end-session`
**Returning session:** `/cdw-start` → `/cdw-sql-work` → `/cdw-end-session`

### Codex (OpenAI CLI)

```
codex
```

Reads `AGENTS.md` automatically, which tells it: when asked to run a workflow step by
name, find the instructions in `.claude/commands/[name].md`. So you just say the step name:

```
session-start
```
```
sql-work
```

---

## The One Rule

**Don't run any R files without explicit permission. Don't touch any csv files or
spreadsheets without explicit permission.**

That means: don't run `flow.R`, `scribe-factory.R`, or any SQL script against
`cdw_cache_staging` without confirming with the project lead. Editing scripts is always
fine. Running them against real data needs a go-ahead.

`ss-` files (ss_dx, ss_med, etc.) also need PI review and sign-off before they're
run — they define which patients and concepts are in scope for the study.

---

## File Structure Reference

```
ai/                        machine-facing (do not edit manually)
  safety-rules.md          safety constraints for all AI tools
  ai-state.md              current project state — overwritten each session
  sql-style.md             SQL coding conventions
  sql-templates.md         available templates and when to use them
  r-style.md               R coding conventions
  (log format is in .claude/commands/cdw-end-session.md)

documentation/             human-facing
  github-issues.md         mirror of GitHub issues (run export script to refresh)
  ai-sessions/             audit logs — one file per day, append-only
```

---

## Quick Reference

| Task | VS Code | Terminal / CLI |
|---|---|---|
| Pull GitHub issues | `Ctrl+Shift+P` → Pull GitHub Issues | `python utility/export-repo-issues.py` |
| Populate SQL scripts | `Ctrl+Shift+P` → Populate CRDW Scripts | `python utility/populate-scripts.py` |
| Start Claude Code | — | `claude` (from repo root) |
| Start Codex | — | `codex` (from repo root) |
| Orient session | — | `/cdw-orient` |
| Build concept-set tables | — | `/cdw-ss-build [type]` |
| Scaffold scripts | — | `/cdw-sql-scaffold` |
| Customize scripts | — | `/cdw-sql-work` |
| End session | — | `/cdw-end-session` |
