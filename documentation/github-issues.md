# GitHub Issues

- Repo: `OuhscBbmc/cdw-skeleton-1`
- Local repo: `D:\projects\crdw\cdw-skeleton-1`
- Source: live GitHub API
- Generated: 2026-06-08 17:04
- Issue count: 5

Use this file as a working reference for progress, acceptance criteria, and inclusion criteria. Re-run `python utility/export-repo-issues.py` when issues change.

## Issue Index

- [#1 use yaml file to specify tables & columns to scribe](#issue-1-use-yaml-file-to-specify-tables-columns-to-scribe)
- [#2 description & summary](#issue-2-description-summary)
- [#3 Revise description_template to acknowledge funding source](#issue-3-revise-description-template-to-acknowledge-funding-source)
- [#6 modernize scribe-factory for purrr 1.0](#issue-6-modernize-scribe-factory-for-purrr-1-0)
- [#10 add more sql style directives, and apply them to the templates](#issue-10-add-more-sql-style-directives-and-apply-them-to-the-templates)

## Issue #1: use yaml file to specify tables & columns to scribe

- State: closed
- URL: https://github.com/OuhscBbmc/cdw-skeleton-1/issues/1
- Author: wibeasley
- Assignees: @wibeasley
- Labels: none
- Created: 2019-03-25T22:16:55Z
- Updated: 2019-03-28T06:42:18Z
- Closed: 2019-03-28T06:42:18Z
- Comments: 0

### Issue Body

ie, write to csv

## Issue #2: description & summary

- State: closed
- URL: https://github.com/OuhscBbmc/cdw-skeleton-1/issues/2
- Author: wibeasley
- Assignees: @wibeasley
- Labels: none
- Created: 2019-04-01T04:56:16Z
- Updated: 2021-10-05T01:59:34Z
- Closed: 2021-10-05T01:59:34Z
- Comments: 0

### Issue Body

produce a file that describes the CSVs to help orient the research to the CDW product

cc: @athumann 

## Issue #3: Revise description_template to acknowledge funding source

- State: closed
- URL: https://github.com/OuhscBbmc/cdw-skeleton-1/issues/3
- Author: genevaneva
- Assignees: @wibeasley
- Labels: none
- Created: 2021-06-18T13:54:11Z
- Updated: 2021-06-22T13:37:18Z
- Closed: 2021-06-22T13:37:18Z
- Comments: 4

### Issue Body

Can we change the description template from this: 

```
description_template <- paste0(
  "---\ntitle: %s Extracts\n\n---\n\n",
  "Project: `%s`\n",
  "============================\n\n",
  "Data Extracts from the BBMC CDW\n\n",
  "%i datasets were derived from the CDW and saved as separate csvs.\n",
  "The collection of datasets is described in the file `%s`\n",
  "which can be opened in Excel, Notepad++, or any program that can read plain text.\n\n",
  "%s\n\n",
  "The datasets were saved by %s at %s.\n\n",
  "%s\n\n",
  "%s\n\n"
)
```

to  this?
```
description_template <- paste0(
  "---\ntitle: %s Extracts\n\n---\n\n",
  "Project: `%s`\n",
  "============================\n\n",
  "Data Extracts from the BBMC CRDW\n\n",
  "%i datasets were derived from the CRDW and saved as separate csvs.\n",
  "The collection of datasets is described in the file `%s`\n",
  "which can be opened in Excel, Notepad++, or any program that can read plain text.\n\n",
  "%s\n\n",
  "The datasets were saved by %s at %s.\n\n",
  "%s\n\n",
  "%s\n\n",
  "This work was made possible by the NIH grant U54GM104938 -[ (Oklahoma Shared Clinical and Translational Resource)](http://osctr.ouhsc.edu). Because our continued existence depends partly on productivity in research dissemination, when producing articles and presentations that utilize these data, please include this grant number in your acknowledgements.\n\n
 Our suggested acknowledgement:  'Data for this research were provided by the University of Oklahoma Health Sciences Center Clinical Research Data Warehouse (http://ouhsc.edu/bbmc/crdw), whose work is made possible by NIH grant U54GM104938.'\n"
```

or something to that effect? @deshea you always have good suggestions for polishing these things.

### Comments

#### Comment 1 by @deshea

- Created: 2021-06-18T14:12:44Z

I think that looks great, @genevamarshall. Good idea!

#### Comment 2 by @wibeasley

- Created: 2021-06-18T18:08:18Z

@genevamarshall, I like it.  Please commit & push the improved text.

FYI to everyone, it won't display for existing projects unless the scribe-factory is updated too.

#### Comment 3 by @genevaneva

- Created: 2021-06-18T18:09:23Z

I don't have permissions to edit in this repo. 

#### Comment 4 by @wibeasley

- Created: 2021-06-21T16:04:09Z

@genevamarshall, try it again.  I added the 'cdw-admin' as a team to 'cdw-collaborator', which can read/write this repo.

## Issue #6: modernize scribe-factory for purrr 1.0

- State: closed
- URL: https://github.com/OuhscBbmc/cdw-skeleton-1/issues/6
- Author: wibeasley
- Assignees: @wibeasley, @akanagwa
- Labels: none
- Created: 2023-01-09T19:04:29Z
- Updated: 2023-01-09T20:14:00Z
- Closed: 2023-01-09T20:14:00Z
- Comments: 1

### Issue Body

@akanagwa, the purrr package was updated recently.  The scribe factories of existing repos may need this update.  Just copy & paste this into the old repos when they fail.

The big fix was https://github.com/OuhscBbmc/cdw-skeleton-1/commit/32f71d66e2155ca840361c85a0004d33ecf354e4

There were a few other soft (non-breaking) changes to purrr that I'll update now.

### Comments

#### Comment 1 by @wibeasley

- Created: 2023-01-09T20:14:00Z

@akanagwa, the changes are made to the file, and keep an eye out for old repos that need this update.  If so, remember just copy & paste from https://github.com/OuhscBbmc/cdw-skeleton-1/blob/main/manipulation/scribe-factory.R

## Issue #10: add more sql style directives, and apply them to the templates

- State: open
- URL: https://github.com/OuhscBbmc/cdw-skeleton-1/issues/10
- Author: wibeasley
- Assignees: @wibeasley
- Labels: none
- Created: 2026-06-08T22:02:39Z
- Updated: 2026-06-08T22:03:33Z
- Closed: not closed
- Comments: 0

### Issue Body

Codex prompt
> Apply the sql coding styles in agents.md to the sql files in this repo
