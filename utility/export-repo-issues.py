"""
export-repo-issues.py
---------------------
Pulls all GitHub issues for this repo and writes them to
documentation/github-issues.md as a readable working reference.

Run from anywhere inside the repo:
  python utility/export-repo-issues.py

Or from outside, passing the repo name:
  python utility/export-repo-issues.py sidorov-stroke-1
  python utility/export-repo-issues.py OuhscBbmc/sidorov-stroke-1

Requires GITHUB_TOKEN or GH_TOKEN environment variable for private repos.
See HOW-TO-AI.md for setup instructions.

Requirements: Python 3.8+, no third-party packages needed.
"""

import argparse
import json
import os
import re
import subprocess
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime
from pathlib import Path


DEFAULT_OWNER = "OuhscBbmc"
DEFAULT_REPO_ROOT = Path(r"C:\Users\gcruz\Documents\GitHub")
OUTPUT_RELATIVE_PATH = Path("documentation") / "github-issues.md"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def find_repo_root(start: Path = None) -> Path | None:
    """Walk up from start (or cwd) to find the repo root (.git or config.yml)."""
    path = (start or Path.cwd()).resolve()
    for _ in range(8):
        if (path / ".git").exists() or (path / "config.yml").exists():
            return path
        if path.parent == path:
            break
        path = path.parent
    return None


def detect_repo_slug(repo_root: Path) -> str | None:
    """Try to read the remote origin URL from .git/config."""
    git_config = repo_root / ".git" / "config"
    if not git_config.exists():
        return None
    text = git_config.read_text(encoding="utf-8")
    match = re.search(r"url\s*=\s*.*[:/]([^/]+/[^/\n]+?)(?:\.git)?\s*$", text, re.MULTILINE)
    if match:
        return match.group(1)
    return None


def token() -> str:
    return (
        os.environ.get("GITHUB_TOKEN", "").strip()
        or os.environ.get("GH_TOKEN", "").strip()
    )


def gh_token_fallback() -> str:
    """Try to get a token from the GitHub CLI if installed."""
    try:
        result = subprocess.run(
            ["gh", "auth", "token"],
            capture_output=True, text=True, timeout=5
        )
        if result.returncode == 0:
            return result.stdout.strip()
    except Exception:
        pass
    return ""


def get_token() -> str:
    tok = token()
    if not tok:
        tok = gh_token_fallback()
    return tok


def github_get(path_or_url: str, params: dict = None) -> dict | list:
    url = (
        path_or_url
        if path_or_url.startswith("https://")
        else f"https://api.github.com{path_or_url}"
    )
    if params:
        url += "?" + urllib.parse.urlencode(params)

    req = urllib.request.Request(url)
    tok = get_token()
    if tok:
        req.add_header("Authorization", f"Bearer {tok}")
    req.add_header("Accept", "application/vnd.github+json")
    req.add_header("X-GitHub-Api-Version", "2022-11-28")
    req.add_header("User-Agent", "crdw-issue-exporter/1.0")

    for attempt in range(3):
        try:
            with urllib.request.urlopen(req, timeout=30) as resp:
                return json.loads(resp.read())
        except urllib.error.URLError:
            if attempt >= 2:
                raise
            time.sleep(3)


def github_get_all(path_or_url: str, params: dict = None) -> list:
    items = []
    page = 1
    while True:
        page_params = dict(params or {})
        page_params.update({"per_page": 100, "page": page})
        page_items = github_get(path_or_url, page_params)
        items.extend(page_items)
        if len(page_items) < 100:
            break
        page += 1
    return items


def clean_text(value: str) -> str:
    """Repair mojibake from double-encoded UTF-8."""
    if not isinstance(value, str):
        return value
    if not any(m in value for m in ("â", "ð", "Â", "Ã")):
        return value
    try:
        return value.encode("latin-1").decode("utf-8")
    except UnicodeError:
        return value


def slugify(value: str) -> str:
    value = value.lower()
    value = re.sub(r"[^a-z0-9]+", "-", value)
    return value.strip("-")


# ---------------------------------------------------------------------------
# Fetch
# ---------------------------------------------------------------------------

def fetch_issues(repo_slug: str) -> list:
    raw_issues = github_get_all(f"/repos/{repo_slug}/issues", {"state": "all"})
    issues = []
    for raw in raw_issues:
        if raw.get("pull_request"):
            continue
        number = int(raw["number"])
        comments_raw = (
            github_get_all(raw["comments_url"]) if raw.get("comments", 0) else []
        )
        issues.append({
            "repo":      repo_slug,
            "number":    number,
            "title":     clean_text(raw.get("title") or f"Issue {number}"),
            "state":     raw.get("state") or "unknown",
            "url":       raw.get("html_url") or "",
            "author":    (raw.get("user") or {}).get("login") or "unknown",
            "assignees": [a.get("login", "") for a in raw.get("assignees", []) if a.get("login")],
            "labels":    [lb.get("name", "") for lb in raw.get("labels", []) if lb.get("name")],
            "created":   raw.get("created_at") or "",
            "updated":   raw.get("updated_at") or "",
            "closed":    raw.get("closed_at") or "",
            "body":      clean_text(raw.get("body") or ""),
            "comments":  [
                {
                    "author":  (c.get("user") or {}).get("login") or "unknown",
                    "created": c.get("created_at") or "",
                    "body":    clean_text(c.get("body") or ""),
                }
                for c in comments_raw
            ],
        })
    issues.sort(key=lambda i: i["number"])
    return issues


# ---------------------------------------------------------------------------
# Render
# ---------------------------------------------------------------------------

def render_issue(item: dict) -> str:
    lines = [
        f"## Issue #{item['number']}: {item['title']}",
        "",
        f"- State: {item['state']}",
        f"- URL: {item['url'] or 'unknown'}",
        f"- Author: {item['author']}",
        f"- Assignees: {', '.join('@' + a for a in item['assignees']) if item['assignees'] else 'none'}",
        f"- Labels: {', '.join(item['labels']) if item['labels'] else 'none'}",
        f"- Created: {item['created'] or 'unknown'}",
        f"- Updated: {item['updated'] or 'unknown'}",
        f"- Closed: {item['closed'] or 'not closed'}",
        f"- Comments: {len(item['comments'])}",
        "",
        "### Issue Body",
        "",
        item["body"] or "_No issue body._",
        "",
    ]

    if item["comments"]:
        lines.extend(["### Comments", ""])
        for idx, comment in enumerate(item["comments"], start=1):
            lines.extend([
                f"#### Comment {idx} by @{comment['author']}",
                "",
                f"- Created: {comment['created'] or 'unknown'}",
                "",
                comment["body"] or "_No comment body._",
                "",
            ])

    return "\n".join(lines)


def render_document(repo_slug: str, repo_root: Path, issues: list) -> str:
    now = datetime.now().strftime("%Y-%m-%d %H:%M")
    lines = [
        "# GitHub Issues",
        "",
        f"- Repo: `{repo_slug}`",
        f"- Local repo: `{repo_root}`",
        f"- Source: live GitHub API",
        f"- Generated: {now}",
        f"- Issue count: {len(issues)}",
        "",
        "Use this file as a working reference for progress, acceptance criteria, and "
        "inclusion criteria. Re-run `python utility/export-repo-issues.py` when issues change.",
        "",
        "## Issue Index",
        "",
    ]
    for issue in issues:
        anchor = f"issue-{issue['number']}-{slugify(issue['title'])}"
        lines.append(f"- [#{issue['number']} {issue['title']}](#{anchor})")
    lines.append("")
    for issue in issues:
        lines.append(render_issue(issue))
    return "\n".join(lines).rstrip() + "\n"


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def resolve_slug_and_root(target: str | None) -> tuple[str, Path]:
    """Return (repo_slug, repo_root) from a target string or cwd detection."""
    if target:
        # Could be a path, a slug, or just a repo name
        target_path = Path(target)
        if target_path.exists():
            repo_root = target_path.resolve()
            slug = detect_repo_slug(repo_root)
            name = repo_root.name
            return slug or f"{DEFAULT_OWNER}/{name}", repo_root

        name = target.split("/")[-1]
        slug = target if "/" in target else f"{DEFAULT_OWNER}/{target}"
        candidate = DEFAULT_REPO_ROOT / name
        repo_root = candidate if candidate.exists() else DEFAULT_REPO_ROOT / name
        return slug, repo_root

    # Auto-detect from cwd
    repo_root = find_repo_root()
    if repo_root is None:
        print("Could not find repo root. Run from inside a repo or pass a repo name.")
        sys.exit(1)

    slug = detect_repo_slug(repo_root)
    if not slug:
        name = repo_root.name
        slug = f"{DEFAULT_OWNER}/{name}"
        print(f"Could not read remote URL; assuming slug: {slug}")

    return slug, repo_root


def main(argv=None):
    parser = argparse.ArgumentParser(description="Export GitHub issues to documentation/github-issues.md")
    parser.add_argument(
        "repo", nargs="?", default=None,
        help="Repo name, owner/repo, or local path. Defaults to auto-detect from cwd."
    )
    parser.add_argument("--output", default=str(OUTPUT_RELATIVE_PATH))
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args(argv)

    repo_slug, repo_root = resolve_slug_and_root(args.repo)
    print(f"Repo:      {repo_slug}")
    print(f"Repo root: {repo_root}")

    if not get_token():
        print(
            "\n[warning] No GITHUB_TOKEN or GH_TOKEN found and GitHub CLI not available.\n"
            "Private repos will fail. See HOW-TO-AI.md for token setup instructions.\n"
        )

    print("Fetching issues...")
    try:
        issues = fetch_issues(repo_slug)
    except urllib.error.HTTPError as exc:
        if exc.code == 401:
            print("Authentication failed. Check your GITHUB_TOKEN. See HOW-TO-AI.md.")
        elif exc.code == 404:
            print(f"Repo not found: {repo_slug}. Check the repo name and your token permissions.")
        else:
            print(f"GitHub API error: {exc}")
        return 1

    if not issues:
        print("No issues found.")
        return 0

    output_path = repo_root / args.output
    content = render_document(repo_slug, repo_root, issues)

    if args.dry_run:
        print(f"Would write {len(issues)} issues to {output_path}")
        print(content[:800])
        return 0

    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(content, encoding="utf-8")
    print(f"Wrote {len(issues)} issues to {output_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
