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
| `/project-orient` | Full briefing for a new contributor or returning after a gap |
| `/session-start` | Orient: load safety rules, check project state, summarize study |
| `/sql-inventory` | List existing scripts with status (populated / stub) |
| `/sql-generate` | Scaffold new scripts from templates based on study context |
| `/sql-work` | Walk through scripts in sequence and apply style corrections |
| `/ss-create [type]` | Build a study-specific lookup table (dx, med, lab, etc.) |
| `/session-end` | Write machine state to `ai/ai-state.md`; append human audit log |

First time / returning after a gap: `/project-orient` → `/sql-work` → `/session-end`
Typical session: `/session-start` → `/sql-inventory` → `/sql-work` → `/session-end`
Fresh project: `/session-start` → `/sql-generate` → `/sql-work` → `/ss-create dx` → `/session-end`

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

### Gemini CLI

```
gemini
```

Reads `GEMINI.md` automatically. Use `@`-mentions to load topic files on demand:
- `@ai/sql-style.md` when editing SQL
- `@ai/sql-templates.md` when generating scripts
- `@ai/session-logging.md` when closing a session

### GitHub Copilot (VS Code)

Nothing extra to do. Copilot reads `.github/copilot-instructions.md` automatically.
For best results, pull issues first so Copilot has the research context:
`Ctrl+Shift+P` → **Pull GitHub Issues**

When editing SQL, open `ai/sql-style.md` in a tab — Copilot will factor it in.

### ChatGPT (web or desktop)

ChatGPT doesn't read files automatically. At the start of each session, paste the
contents of the files relevant to what you're doing:

- **Always paste:** `ai/safety-rules.md`
- **For SQL work:** also paste `ai/sql-style.md`
- **For new scripts:** also paste `ai/sql-templates.md`
- **For project context:** also paste `documentation/github-issues.md` or `ai/ai-state.md`

If you work on the same repo frequently, paste `ai/safety-rules.md` into ChatGPT's
**Custom Instructions** (Settings → Personalization) so you don't repeat it every session.

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
  session-logging.md       log format for both outputs

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
| Start Gemini CLI | — | `gemini` (from repo root) |
| Orient session (Claude) | — | `/session-start` |
| Generate scripts (Claude) | — | `/sql-generate` |
| Review scripts (Claude) | — | `/sql-work` |
| End session (Claude) | — | `/session-end` |
