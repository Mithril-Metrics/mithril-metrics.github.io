"""Release scheduled posts.

For every case whose 00_meta.yml has `publish_at:` set to a time that has passed, and whose
report.qmd still says `draft: true`, set `draft: false`, set the post date to the release date,
and mark the case as published in 00_meta.yml. Prints the released case ids, one per line.

publish_at format: ISO 8601 with a UTC offset, e.g. "2026-11-22T09:00+01:00".
Run from the repository root. Standard library only.
"""
import datetime as dt
import pathlib
import re
import sys

now = dt.datetime.now(dt.timezone.utc)
released = []
errors = []

for meta_path in sorted(pathlib.Path("cases").glob("*/00_meta.yml")):
    case = meta_path.parent
    if case.name.startswith("_"):
        continue
    meta = meta_path.read_text(encoding="utf-8")
    m = re.search(r'^publish_at:\s*"?([^"#\n]*?)"?\s*(?:#.*)?$', meta, re.M)
    if not m or m.group(1).strip() in ("", "null", "~"):
        continue
    raw = m.group(1).strip()
    try:
        when = dt.datetime.fromisoformat(raw)
    except ValueError:
        errors.append(f"{case.name}: cannot read publish_at '{raw}'")
        continue
    if when.tzinfo is None:
        errors.append(f"{case.name}: publish_at '{raw}' has no UTC offset (e.g. +01:00)")
        continue
    if when > now:
        continue

    report = case / "report.qmd"
    text = report.read_text(encoding="utf-8")
    if not re.search(r"^draft:\s*true\b", text, re.M):
        continue  # already published
    text = re.sub(r"^draft:\s*true\b", "draft: false", text, count=1, flags=re.M)
    text = re.sub(r'^date:\s*"?[0-9-]+"?', f'date: "{when.date().isoformat()}"', text, count=1, flags=re.M)
    report.write_text(text, encoding="utf-8")

    meta = re.sub(r"^status:\s*\S+", "status: published", meta, count=1, flags=re.M)
    meta = re.sub(r"^publish_at:.*$", lambda x: x.group(0) + f'\npublished_at: "{now.strftime("%Y-%m-%dT%H:%MZ")}"',
                  meta, count=1, flags=re.M)
    meta_path.write_text(meta, encoding="utf-8")
    released.append(case.name)

for e in errors:
    print(f"WARNING {e}", file=sys.stderr)
print("\n".join(released))
