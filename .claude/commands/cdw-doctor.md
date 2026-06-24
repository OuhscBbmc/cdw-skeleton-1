# /cdw-doctor

Verify external dependencies used by the AI workflow. This is a maintenance and
diagnostic command, not part of the every-session or new-project sequence.

Run this once per machine setup, or any time a command fails with an auth,
network, or Python-not-found error. Do not attempt to fix anything automatically;
report only.

## Steps

1. Read `ai/safety-rules.md`.

2. Run these checks from the repo root. Each check must be reported as success
   or failure in the output table.

   | Check | Command / test | Success condition | Failure fix |
   |---|---|---|---|
   | GitHub CLI auth | `gh auth status` | exits 0 | Install GitHub CLI from https://cli.github.com/ if missing, then run `gh auth login`. |
   | Python | `python --version`; if that fails, `py --version` | one command exits 0 and reports Python 3 | Install Python from https://www.python.org/downloads/windows/ or make it available on PATH. |
   | Git repo health | `git status` | exits 0 | Verify your terminal is in the repo root; if `git status` still fails, reclone from the project GitHub URL. |
   | Issue exporter script | test that `utility/export-repo-issues.py` exists | file exists | Restore the file with `git restore -- utility/export-repo-issues.py` or copy it from the current skeleton. |
   | Script populator | test that `utility/populate-scripts.py` exists | file exists | Restore the file with `git restore -- utility/populate-scripts.py` or copy it from the current skeleton. |

3. If `gh auth status` fails, also check whether `$env:GITHUB_TOKEN` is present.
   Report this as an alternate path in the GitHub CLI auth row or failure detail,
   not as a separate requirement:
   - Present: "Alternate available: `GITHUB_TOKEN` is set."
   - Missing: "Alternate unavailable: set `GITHUB_TOKEN` or run `gh auth login`."
   Do not print the token value.

4. Output a short table with exactly these columns:

   | Check | Status | Result | Fix |
   |---|---|---|---|

   Use `✅` for passing checks and `❌` for failing checks. Keep each result and fix
   to one line.

5. After the table:

   - If every required check is `✅`, print:
     > All checks passed. Safe to run /cdw-orient.

   - If anything is `❌`, print `Failed checks:` followed by a bullet list. Each
     bullet must include the failed check name and the exact remediation command
     or link from the table above.

## Notes

- Do not run `flow.R`, any R script, database command, or project data access.
- Do not run package installs, `gh auth login`, `git restore`, or any repair command.
- A dirty working tree does not fail the git repo health check; this command only
  verifies that `git status` can run successfully in this repo.
