# Safety Rules

These rules apply to every session, every AI assistant, every task.

- Do not execute any R script without explicit permission for that exact command.
  This includes `flow.R`, `spawn.R`, `renv`, knitr, and any script that reads or writes data.
- Do not run anything that might access a database or data source without explicit permission.
- Do not access any `.csv` or Excel file without explicit permission.
- Do not access files outside the repo without explicit permission.
- `ss-` files (ss_dx, ss_med, ss_clinic, etc.) require PI review and sign-off before they
  are run against live data.
- Editing files is always safe. Running them requires explicit permission for that exact command.
