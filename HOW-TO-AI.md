# Using AI With This Repo

This repo includes tools that let you use any AI assistant to help pull GitHub issues,
scaffold SQL scripts, and understand the project context. This guide covers setup and
day-to-day use.

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

After that, close and reopen VS Code or any terminal for it to take effect. You never
need to touch it again.

If you have the GitHub CLI installed (`gh`), it will use that automatically as a fallback
and you may not need to do any of the above.

---

## Pulling GitHub Issues

Issues are pulled into `documentation/github-issues.md` — a readable file that shows the
research question, inclusion criteria, meeting notes, and current task status.

**In VS Code (no terminal needed):**

Press `Ctrl+Shift+P`, type **Pull GitHub Issues**, press Enter. The terminal panel will
show progress and close when done.

**In a terminal:**
```
python utility/export-repo-issues.py
```

If `python` is not on PATH, try:
```powershell
py utility/export-repo-issues.py
```

On this Windows workstation, this fallback path is known to work:
```powershell
& "$env:LOCALAPPDATA\Microsoft\WindowsApps\PythonSoftwareFoundation.Python.3.11_qbz5n2kfra8p0\python.exe" utility\export-repo-issues.py
```

Re-run any time issues are updated on GitHub. The file overwrites cleanly each time.

---

## Populating SQL Scripts

This fetches the relevant SQL script templates for your project and places them in
`manipulation/` with your project schema already substituted in.

**In VS Code (no terminal needed):**

Press `Ctrl+Shift+P`, type **Populate CRDW Scripts**, press Enter. You'll see a numbered
menu in the terminal — type the numbers for the scripts you want and press Enter.

**In a terminal:**
```
python utility/populate-scripts.py
```

If `python` is not on PATH, use `py` or the Windows Python path shown above.

For specific templates without the menu:
```
python utility/populate-scripts.py --templates patient dx medication-meditech
```

The scripts land in `manipulation/` ready to edit. Review and customize before running
against live data — especially dates, WHERE clauses, and inclusion criteria.

Each SQL script should leave behind permanent project-schema output tables only. Use CTEs
or `#temp` tables for intermediary data, or split distinct permanent outputs into separate
scripts. `pt-identity.sql` is only needed when the project requires a REDCap database or
stable REDCap `record_id`.

---

## Using Your AI Tool

### GitHub Copilot (VS Code)

Nothing extra to do. Copilot reads `.github/copilot-instructions.md` automatically and
already knows the repo structure, SQL style, and CRDW context. Just open a file and start
typing or asking questions in the Copilot Chat panel.

For best results, pull issues first so Copilot has the research context:
`Ctrl+Shift+P` → **Pull GitHub Issues**

### Claude Code

From a terminal in the repo root:
```
claude
```

It reads `AGENTS.md` automatically. To orient it at the start of a session:
```
Read AGENTS.md and documentation/github-issues.md, then tell me what this study is about.
```

### Codex (OpenAI CLI)

```
codex
```

Reads `AGENTS.md` automatically, same as Claude Code.

### Gemini CLI

```
gemini
```

Reads `GEMINI.md` automatically (same content as `AGENTS.md`).

### ChatGPT (web or desktop)

ChatGPT doesn't read files automatically. At the start of each session, upload
`AGENTS.md` and `documentation/github-issues.md` using the attachment button,
then say:

> Read these files and tell me what this project is about before we start.

If you work on the same repo frequently, paste the contents of `AGENTS.md` into
ChatGPT's **Custom Instructions** (Settings → Personalization → Custom Instructions)
so you don't have to upload it every time.

---

## The One Rule

**Don't run any R files without explicit permission. Don't touch any csv files or spreadsheets without explicit permission.**

That means: don't run `flow.R`, `scribe-factory.R`, or any SQL script in SSMS or a
query window against `cdw_cache_staging` without confirming with the project lead.
Editing scripts is always fine. Running them against real data needs a go-ahead.

`ss-` files (ss_dx, ss_med, etc.) also need PI review and sign-off before they're
run — they define which patients and concepts are in scope for the study.

---

## Quick Reference

| Task | VS Code | Terminal |
|---|---|---|
| Pull GitHub issues | `Ctrl+Shift+P` → Pull GitHub Issues | `python utility/export-repo-issues.py` |
| Populate SQL scripts | `Ctrl+Shift+P` → Populate CRDW Scripts | `python utility/populate-scripts.py` |
| Start Claude Code | — | `claude` (from repo root) |
| Start Gemini CLI | — | `gemini` (from repo root) |
