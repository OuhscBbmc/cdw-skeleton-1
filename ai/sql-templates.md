# SQL Templates

## Fetch URL

```
https://raw.githubusercontent.com/OuhscBbmc/cdw-skeleton-1/main/manipulation/templates/{template-name}.sql
```

Do not fetch templates speculatively — only when the user asks for script generation.

## Variable Substitution

| Variable | Replace with |
|---|---|
| `{project_schema}` | `schema_name` from `config.yml` |
| `{date_start}` | Study start date from IRB / inclusion criteria |
| `{date_stop}` | Study end date from IRB / inclusion criteria |

## Script Naming Convention

Name scripts descriptively: `patient.sql`, `dx.sql`, `medication-meditech.sql`, etc.
The patient pool script (`patient.sql`) must run before any script that joins to `pt_pool`,
but do not use numeric prefixes — scripts are ordered in `flow.R`, not by filename.

## Available Templates

| Template | Use when |
|---|---|
| `patient.sql` | Always — builds the permanent `patient` table; patient-pool helpers should be CTEs or `#temp` unless explicitly needed as a deliverable |
| `dx.sql` | Study involves diagnoses / problem list (Epic + harmonized) |
| `dx-meditech.sql` | Study needs encounter-level billing diagnoses from Meditech only |
| `medication-epic.sql` | Medications from Epic (admin date >= 2023-06-03) |
| `medication-meditech.sql` | Medications from Meditech (admin date < 2023-06-03) |
| `medication-harmonized.sql` | Medications needed across both Epic and Meditech |
| `medication-centricity.sql` | Medications from Centricity |
| `encounter-epic.sql` | Encounter/visit data from Epic |
| `encounter-meditech.sql` | Encounter/visit data from Meditech |
| `encounter-harmonized.sql` | Encounters needed across both systems |
| `lab-epic.sql` | Lab results from Epic |
| `lab-meditech.sql` | Lab results from Meditech |
| `obs-epic.sql` | Observations/vitals from Epic |
| `obs-meditech.sql` | Observations/vitals from Meditech |
| `obs-centricity.sql` | Observations from Centricity |
| `note-epic.sql` | Clinical notes from Epic |
| `note-meditech.sql` | Clinical notes from Meditech |
| `note-centricity.sql` | Clinical notes from Centricity |
| `procedure-harmonized.sql` | Procedures (CPT codes) across systems |
| `charlson-comorbidities.sql` | Charlson comorbidity index |
| `elixhauser-comorbidities.sql` | Elixhauser comorbidity index |
| `pt-identity.sql` | Only when the project requires a REDCap database or stable REDCap `record_id`; do not generate for routine cross-system MRN lookup |
| `patient-insurance.sql` | Insurance / payer data |
| `birth-epic.sql` | Birth records from Epic |
| `birth-meditech.sql` | Birth records from Meditech |
| `image-epic.sql` | Imaging/radiology from Epic |
| `visit-gecb.sql` | Visit data from GECB (scheduling/billing system) |
| `invoice-gecb.sql` | Invoices/charges from GECB |

## After Generating Scripts

1. Show the user which templates were chosen and why.
2. Describe what needs to be customized in each file (dates, WHERE clause, inclusion criteria).
3. Add generated scripts to `flow.R` in dependency order.
4. Add every delivery table to `config.yml` under `tables_to_scribe`. Each entry needs:
   `name`, `columns_include`, `path_output`, `row_unit`.
   Also add `path_output_summary` and `path_output_description` at the top level.
5. Never add `ss-` tables (ss_dx, ss_med, ss_clinic, etc.) to `tables_to_scribe` — they are
   concept-set lookup inputs, not study outputs.
6. Every generated SQL script should produce one or more permanent tables in the project schema.
   If a second permanent table is a distinct deliverable, generate a separate script rather than
   hiding it as an intermediary.

## Searching Sibling Repos

If a template doesn't exist or the user needs a pattern example, search sibling repos in the
GitHub directory (one level up from the current repo). Look for SQL files with similar naming
(e.g., `medication-meditech.sql`, `patient-flags.sql`). Prefer recent repos. Always show the
user what you found and where before reusing anything.
