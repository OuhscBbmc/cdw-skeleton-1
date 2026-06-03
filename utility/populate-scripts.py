"""
populate-scripts.py
-------------------
Fetches relevant SQL templates from cdw-skeleton-1 and places them in
manipulation/ss/ for this project, with project variables substituted in.

Usage:
  python utility/populate-scripts.py                  # interactive mode
  python utility/populate-scripts.py --templates dx medication-meditech patient

Templates are fetched from:
  https://raw.githubusercontent.com/OuhscBbmc/cdw-skeleton-1/main/manipulation/ss/templates/

Requirements: Python 3.8+, no third-party packages needed.
"""

import argparse
import re
import sys
import urllib.request
from pathlib import Path

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------

TEMPLATE_BASE_URL = (
    "https://raw.githubusercontent.com/OuhscBbmc/cdw-skeleton-1/main"
    "/manipulation/templates/{name}.sql"
)

AVAILABLE_TEMPLATES = [
    "patient",
    "dx",
    "dx-meditech",
    "medication-epic",
    "medication-meditech",
    "medication-harmonized",
    "medication-centricity",
    "encounter-epic",
    "encounter-meditech",
    "encounter-harmonized",
    "lab-epic",
    "lab-meditech",
    "obs-epic",
    "obs-meditech",
    "obs-centricity",
    "note-epic",
    "note-meditech",
    "note-centricity",
    "procedure-harmonized",
    "charlson-comorbidities",
    "elixhauser-comorbidities",
    "pt-identity",
    "patient-insurance",
    "birth-epic",
    "birth-meditech",
    "image-epic",
    "visit-gecb",
    "invoice-gecb",
]

# Plain-English descriptions used in interactive mode
TEMPLATE_DESCRIPTIONS = {
    "patient":                 "Patient pool + demographics (always needed)",
    "dx":                      "Diagnoses / problem list (Epic + harmonized)",
    "dx-meditech":             "Diagnoses — encounter-level Meditech billing only",
    "medication-epic":         "Medications — Epic (admin date >= 2023-06-03)",
    "medication-meditech":     "Medications — Meditech (admin date < 2023-06-03)",
    "medication-harmonized":   "Medications — both Epic and Meditech",
    "medication-centricity":   "Medications — Centricity (OB/GYN)",
    "encounter-epic":          "Encounters / visits — Epic",
    "encounter-meditech":      "Encounters / visits — Meditech",
    "encounter-harmonized":    "Encounters — both systems",
    "lab-epic":                "Lab results — Epic",
    "lab-meditech":            "Lab results — Meditech",
    "obs-epic":                "Observations / vitals — Epic",
    "obs-meditech":            "Observations / vitals — Meditech",
    "obs-centricity":          "Observations — Centricity",
    "note-epic":               "Clinical notes — Epic",
    "note-meditech":           "Clinical notes — Meditech",
    "note-centricity":         "Clinical notes — Centricity",
    "procedure-harmonized":    "Procedures (CPT codes) across systems",
    "charlson-comorbidities":  "Charlson comorbidity index",
    "elixhauser-comorbidities":"Elixhauser comorbidity index",
    "pt-identity":             "Cross-system MRN lookups",
    "patient-insurance":       "Insurance / payer data",
    "birth-epic":              "Birth records — Epic",
    "birth-meditech":          "Birth records — Meditech",
    "image-epic":              "Imaging / radiology — Epic",
    "visit-gecb":              "Visits — GECB scheduling/billing",
    "invoice-gecb":            "Invoices / charges — GECB",
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def find_repo_root() -> Path:
    """Walk up from cwd until we find config.yml."""
    path = Path.cwd()
    for _ in range(6):
        if (path / "config.yml").exists():
            return path
        path = path.parent
    return Path.cwd()


def read_schema_from_config(root: Path) -> str | None:
    config_path = root / "config.yml"
    if not config_path.exists():
        return None
    text = config_path.read_text(encoding="utf-8")
    match = re.search(r"schema_name\s*:\s*['\"]?([^'\"#\n]+)['\"]?", text)
    if match:
        return match.group(1).strip()
    return None


def fetch_template(name: str) -> str | None:
    url = TEMPLATE_BASE_URL.format(name=name)
    try:
        with urllib.request.urlopen(url, timeout=10) as resp:
            return resp.read().decode("utf-8")
    except Exception as exc:
        print(f"  [error] Could not fetch {name}: {exc}")
        return None


def substitute_variables(sql: str, schema: str) -> str:
    sql = sql.replace("{project_schema}", schema)
    return sql


def write_script(dest: Path, content: str, overwrite: bool = False) -> bool:
    if dest.exists() and not overwrite:
        answer = input(f"  {dest.name} already exists. Overwrite? [y/N] ").strip().lower()
        if answer != "y":
            print(f"  Skipped {dest.name}")
            return False
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_text(content, encoding="utf-8")
    print(f"  Written: {dest.relative_to(dest.parent.parent.parent)}")
    return True


# ---------------------------------------------------------------------------
# Interactive template picker
# ---------------------------------------------------------------------------

def pick_templates_interactively() -> list[str]:
    print("\nAvailable templates:")
    for i, name in enumerate(AVAILABLE_TEMPLATES, start=1):
        desc = TEMPLATE_DESCRIPTIONS.get(name, "")
        print(f"  {i:2}. {name:<30} {desc}")
    print()
    raw = input(
        "Enter template numbers or names (comma or space separated).\n"
        "Press Enter to select patient + dx + medication-meditech as a starting set:\n> "
    ).strip()

    if not raw:
        return ["patient", "dx", "medication-meditech"]

    chosen = []
    for token in re.split(r"[,\s]+", raw):
        token = token.strip()
        if not token:
            continue
        if token.isdigit():
            idx = int(token) - 1
            if 0 <= idx < len(AVAILABLE_TEMPLATES):
                chosen.append(AVAILABLE_TEMPLATES[idx])
            else:
                print(f"  [warn] No template at index {token}, skipping.")
        elif token in AVAILABLE_TEMPLATES:
            chosen.append(token)
        else:
            print(f"  [warn] Unknown template '{token}', skipping.")
    return chosen


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Populate CRDW SQL script scaffolds.")
    parser.add_argument(
        "--templates", nargs="*",
        help="Template names to fetch. Omit for interactive mode.",
    )
    parser.add_argument(
        "--schema", default=None,
        help="Override project schema name (default: read from config.yml).",
    )
    parser.add_argument(
        "--overwrite", action="store_true",
        help="Overwrite existing files without prompting.",
    )
    args = parser.parse_args()

    root = find_repo_root()
    print(f"Repo root: {root}")

    # Schema
    schema = args.schema or read_schema_from_config(root)
    if not schema or schema.startswith("{"):
        schema = input("  Enter project schema name (e.g. sidorov_stroke_1): ").strip()
    print(f"Schema:    {schema}")

    # Templates
    if args.templates is not None:
        templates = args.templates
    else:
        templates = pick_templates_interactively()

    if not templates:
        print("No templates selected. Exiting.")
        sys.exit(0)

    print(f"\nFetching {len(templates)} template(s)...\n")
    dest_dir = root / "manipulation"

    written = 0
    for name in templates:
        if name not in AVAILABLE_TEMPLATES:
            print(f"  [warn] Unknown template '{name}', skipping.")
            continue
        print(f"  Fetching {name}.sql ...")
        content = fetch_template(name)
        if content is None:
            continue
        content = substitute_variables(content, schema)
        dest = dest_dir / f"{name}.sql"
        if write_script(dest, content, overwrite=args.overwrite):
            written += 1

    print(f"\nDone. {written} file(s) written to manipulation/ss/")
    if written:
        print(
            "\nNext steps:\n"
            "  1. Review each generated file — customize dates, columns, and WHERE clauses.\n"
            "  2. ss-dx and ss-med files require PI review before running.\n"
            "  3. Do not run scripts against live data without explicit permission.\n"
        )


if __name__ == "__main__":
    main()
