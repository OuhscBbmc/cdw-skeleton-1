# Safety Rules

These rules apply to every session, every AI assistant, every task.

- If there is any uncertainty about whether a file, command, output, prompt, or
  transfer could expose private information, stop and ask before accessing,
  running, summarizing, copying, or transmitting it.
- Do not run anything that might access a database or data source without explicit permission.
- Explicit permission must be for the exact action. It should name the command,
  path, data source, recipient, transfer method, or output destination involved.
  Broad permission to "look around", "run the project", or "use the data" is not
  enough.
- Do not execute any R script without explicit permission for that exact command.
  This includes `flow.R`, `spawn.R`, `renv`, knitr, and any script that reads or writes data.
- Do not access row-level data files without explicit permission, regardless of
  extension. This includes `.csv`, `.rds`, `.RData`, `.qs`, `.parquet`, `sas7bdat`, `.xpt`, `.dta`, `.sav`, `.json`, `.ndjson`, `.xml`,
  `.txt`, `.log`, `.pdf`, Excel files, image files, archives, database files, notebooks, and
  rendered reports when they may contain private data.
- Do not access `data-private/`, `data-unshared/`, local extracts, logs, caches,
  temporary output folders, or derived-data folders without explicit permission.
- Do not access files outside the repo without explicit permission.
- Do not include PHI, PII, row-level records, secrets, tokens, connection strings,
  or private file paths in source code, documentation, examples, tests, mock data,
  prompts, responses, logs, commit messages, pull requests, issues, or comments.
  Use synthetic values and placeholders instead.
- Do not git commit any files to the repo without explicit permission.
  (Note to human: our team's policy requires a person to review each committed file to ensure private info doesn't slip into the repo and get pushed to the GitHub server.)
- Do not access any remote computer without explicit permission.
  This includes protocols and processes like ssh, rdp, odbc, and sftp.
- If you are granted specific permission to access a data file,
  first read on the column headers, and determine if they likely contain any of the 18 PII elements.
  If so, stop and ask the user to verify the file does not contain PII or PHI.
  If they are contained, do not continue.
- Do not transmit, upload, paste, sync, attach, screenshot, or otherwise disclose
  PHI, PII, row-level data, or private project files to any external service or
  person without explicit permission for that exact disclosure. This includes AI
  model prompts, web searches, plugins/connectors, GitHub, email, chat, cloud
  storage, shared documents, telemetry, package registries, and issue trackers.
- When private data transmission is explicitly approved, use the minimum
  necessary data only, use an approved secure channel, encrypt data in transit
  and at rest when applicable, and document the recipient, purpose, method, and
  output location in the appropriate project audit log.
- Do not reproduce any private information in the assistant responses.
  Report only the file/path or command context needed for the human to
  investigate, and ask how to proceed.
- Editing code, configuration, SQL, documentation, and other non-data files is
  generally safe when it does not add, expose, or derive private information.
  Running them, rendering them, or opening their data outputs requires explicit
  permission for that exact command or file.
