# AI Workflow Guide

This folder contains machine-facing context files used by AI assistants (Claude Code,
Codex) to work safely and consistently on CRDW projects. Do not edit these files
manually — `ai-state.md` is maintained by the AI at the end of each session, and the
style guides are shared across projects.

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

## Getting Started

### New project

You've just created the repo and documented the initial meeting in a GitHub issue.

```
/cdw-orient        ← detects no state exists; gives full briefing
/cdw-plan          ← reads the meeting issue; maps data asks to scripts + ss-files
/cdw-sql-scaffold  ← pulls script templates; opens one GitHub issue per script
/cdw-ss-build dx   ← generates discovery query for diagnoses; send to PI
/cdw-ss-build med  ← same for medications (repeat for each concept type needed)
/cdw-sql-work      ← starts patient.sql immediately; pauses on scripts awaiting PI
/cdw-end-session   ← writes project state + audit log
```

While you wait for the PI to return annotated concept sets, `/cdw-sql-work` will work
through any scripts that don't depend on ss-files (`patient.sql` first). When the PI
returns results, run `/cdw-ss-build [type]` Step 2 to load them, then re-run
`/cdw-sql-work` to resume the dependent scripts.

### Existing project, returning after any gap

```
/cdw-orient        ← quick if state is fresh (<7 days); full briefing if stale
                      tells you exactly what to do next
/cdw-sql-work      ← or whatever orient recommends
/cdw-end-session
```

That's the full normal session. Orient handles the rest.

---

## Commands

| Command | What it does |
|---|---|
| `/cdw-orient` | **Start every session.** Quick mode (3-bullet summary + next step) if project state is fresh. Full mode (reads issues, scans scripts, structured briefing) if state is stale or missing. Say "full orient" to force the full version anytime. |
| `/cdw-plan` | Reads the initial meeting issue and maps data asks to a concrete list of scripts and ss-files needed. Confirms with you before anything is generated. Run once at project start. |
| `/cdw-sql-scaffold` | Pulls SQL script templates for all confirmed data asks. Adds each to `flow.R` commented out. Opens a GitHub tracking issue per script (checks for duplicates first). Commit these templates before customizing. |
| `/cdw-ss-build [type]` | Builds a study-specific concept-set lookup table. Step 1 generates a discovery query to send to the PI. Step 2 (run when PI returns annotated results) writes the finished script. Types: `dx`, `med`, `lab`, `location`, `obs`, `cpt`. |
| `/cdw-sql-work` | Works through scripts in `flow.R` order, filling in study-specific values and activating each in `flow.R` when done. Starts with scripts that have no ss-file dependency. Pauses on blocked scripts and tells you what's waiting. Safe to re-run as ss-files come in. |
| `/cdw-end-session` | Writes current project state to `ai-state.md` and appends a human audit log to `documentation/ai-sessions/`. Run at the end of any session that did real work. |

These are Claude Code slash commands. Codex users: say the step name (e.g. `cdw-orient`)
and it will find the instructions in `.claude/commands/`.

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
`/cdw-sql-scaffold`.

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

## The One Rule

**Don't run any R files without explicit permission. Don't touch any csv files or
spreadsheets without explicit permission.**

That means: don't run `flow.R`, `scribe-factory.R`, or any SQL script against
`cdw_cache_staging` without confirming with the project lead. Editing scripts is always
fine. Running them against real data needs a go-ahead.

`ss-` files (ss_dx, ss_med, etc.) also need PI review and sign-off before they're
run — they define which patients and concepts are in scope for the study.

---

## Files in This Folder

| File | What it is | Who maintains it |
|---|---|---|
| `ai-state.md` | Compact project snapshot: study aim, inclusion criteria, script status with GitHub issue numbers, current focus, next steps. The AI reads this at session start to orient quickly without re-reading everything. | AI (written at `/cdw-end-session`) |
| `safety-rules.md` | Guardrails that apply to every AI tool every session: what requires explicit permission, what is never allowed, PHI/PII rules. | Human (edit when policies change) |
| `sql-style.md` | SQL coding conventions for this project family: keyword casing, indentation, join formatting, CREATE TABLE alignment, etc. | Human |
| `sql-templates.md` | Describes available SQL script templates and when to use each. Referenced during `/cdw-sql-scaffold`. | Human |
| `r-style.md` | R coding conventions: function prefixing, deduplication patterns, indentation. | Human |

### How the AI uses these files

The AI loads files lazily — only what the current task requires:

| Task | Files loaded |
|---|---|
| Any session start | `safety-rules.md` + `ai-state.md` (or `github-issues.md` if stale) |
| SQL editing | `sql-style.md` |
| Scaffolding scripts | `sql-style.md` + `sql-templates.md` |
| Building ss-files | `sql-style.md` |
| R editing | `r-style.md` |

Style guides are never loaded speculatively. This keeps context lean and responses fast.

---

## GitHub Issue Tracking

When `/cdw-sql-scaffold` creates scripts, it opens one GitHub issue per script with a
customization checklist. The issue number is stored in `ai-state.md` alongside the script
status. As `/cdw-sql-work` completes each script, it comments on the corresponding issue.
This gives you a live progress tracker on GitHub without any manual overhead.

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
