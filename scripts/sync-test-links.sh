#!/usr/bin/env bash
# sync-test-links.sh - keep the **Tests:** callouts in compiler/docs/*.md
# in sync with the actual test suite.
#
# Walks every tests/Test/*.hs to find `test "<name>_section_<tag>"`
# invocations, captures the test body (lines from the test up to the
# next test in the same list literal), and rewrites
# `<!-- BEGIN tests:<tag> -->` ... `<!-- END tests:<tag> -->` blocks in
# every spec doc with:
#
#   - a sorted list of `[<test name>](<github permalink>)` bullets, and
#   - a collapsible `<details>` block containing each verbatim test
#     body so the reader can see the assertion without leaving the
#     page.
#
# Also walks the fixture directories listed in tests/.spec-section-map
# (tests/scenarios, tests/format, tests/corpus/*) and counts the
# fixtures into the matching spec section.
#
# Modes:
#
#   sync-test-links.sh             # rewrite the docs in place
#   sync-test-links.sh --check     # exit 1 if any doc would change (CI)
#
# The script is implemented in Python because it manipulates
# multi-line markdown blocks and that is awkward in pure shell. Python
# 3 ships on every platform CI uses, so this stays "no new
# dependencies".

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

exec python3 - "$@" <<'PY'
import os
import re
import sys
from pathlib import Path
from subprocess import check_output

CHECK_MODE = "--check" in sys.argv[1:]

REPO_OWNER = "quone-lang"
REPO_NAME = "compiler"
DOCS_DIR = Path("docs")
TESTS_DIR = Path("tests")
TEST_MODULES_DIR = TESTS_DIR / "Test"
SECTION_MAP_PATH = TESTS_DIR / ".spec-section-map"

# `test "<name>_section_<tag>"` recogniser. The name part allows
# uppercase letters too (some test names embed caps, e.g.
# `lex/integer_with_L_suffix`). The tag is `[a-z0-9_]+`.
# We anchor on the leading `test "` token so bare references in
# comments don't match.
TEST_RE = re.compile(r'\btest\s+"([a-z_]+/[A-Za-z0-9_]+)_section_([a-z0-9_]+)"')

# `<!-- BEGIN tests:X.Y -->` and `<!-- END tests:X.Y -->` markers. The tag
# X.Y is whatever the spec author used; it MUST match a section tag in a
# test name (with `.` replaced by `_`) or a value in
# .spec-section-map.
BEGIN_RE = re.compile(r"<!-- BEGIN tests:([A-Za-z0-9._-]+) -->")
END_RE = re.compile(r"<!-- END tests:([A-Za-z0-9._-]+) -->")


def head_sha() -> str:
    return check_output(["git", "rev-parse", "HEAD"]).decode().strip()


def normalise_tag(tag: str) -> str:
    """Spec authors write `3.3` in markers; tests encode `3_3`. Normalise
    both to a canonical underscore form so the lookup matches."""
    return tag.replace(".", "_")


def discover_named_tests():
    """Walk tests/Test/*.hs and return a list of dicts:
        { name, section, file (relative to repo root), line, body (str) }
    `body` is the verbatim source from the `test "..."` line up to (but
    not including) the next `test "..."` or the closing `]` of the
    enclosing list literal, whichever comes first."""
    found = []
    for hs in sorted(TEST_MODULES_DIR.glob("*.hs")):
        text = hs.read_text()
        lines = text.splitlines()
        starts = []  # list of (lineno, full_name, name_short, section, marker_col)
        for idx, line in enumerate(lines, start=1):
            m = TEST_RE.search(line)
            if not m:
                continue
            # Column of the `[` or `,` introducing this list element. Used
            # to detect the matching closing `]` (which sits at the same
            # column or further left).
            marker_col = len(line) - len(line.lstrip(" "))
            short, tag = m.group(1), m.group(2)
            full = f"{short}_section_{tag}"
            starts.append((idx, full, short, tag, marker_col))
        for i, (start_line, full, short, tag, marker_col) in enumerate(starts):
            # The body ends at the LATER of:
            #   - the line BEFORE the next test in the file, or
            #   - the line BEFORE the closing `]` of THIS test's list
            #     literal (a line whose first non-space char is `]` at a
            #     column <= marker_col), or
            #   - end of file.
            # We pick the EARLIEST of the candidates that lies after the
            # start line.
            candidates = [len(lines)]
            if i + 1 < len(starts):
                candidates.append(starts[i + 1][0] - 1)
            # Search forward for the closing `]` of this list literal.
            for j in range(start_line, len(lines)):
                s = lines[j]
                stripped = s.lstrip(" ")
                col = len(s) - len(stripped)
                if (
                    stripped.startswith("]")
                    and col <= marker_col
                    and j > start_line - 1
                ):
                    candidates.append(j)
                    break
            end_line = min(c for c in candidates if c >= start_line)
            body_lines = lines[start_line - 1 : end_line]
            if body_lines:
                first = body_lines[0]
                body_lines[0] = re.sub(r"^\s*[,\[]\s*", "    ", first)
                while body_lines and not body_lines[-1].strip():
                    body_lines.pop()
            found.append(
                {
                    "name": full,
                    "name_short": short,
                    "section": tag,
                    "file": str(hs),
                    "line": start_line,
                    "body": "\n".join(body_lines),
                }
            )
    return found


def discover_fixture_tests(section_map):
    """For each entry in tests/.spec-section-map, walk the directory and
    return one synthetic test per fixture file. The 'body' is empty -
    fixtures don't have inline assertions; they're external files."""
    found = []
    for fixture_dir, section_tag in section_map.items():
        if not Path(fixture_dir).is_dir():
            continue
        # Pick the file kind that defines a test. By convention:
        #   tests/scenarios/*.Q  -> one scenario per .Q
        #   tests/format/*.in.Q  -> one fixture per .in.Q
        #   tests/corpus/**/*.Q  -> one test per .Q
        if "format" in fixture_dir:
            files = sorted(Path(fixture_dir).glob("*.in.Q"))
        else:
            files = sorted(Path(fixture_dir).rglob("*.Q"))
        for f in files:
            stem = f.stem.replace(".in", "")  # `.in.Q` -> name without ".in"
            found.append(
                {
                    "name": f"{Path(fixture_dir).name}/{stem}",
                    "name_short": f"{Path(fixture_dir).name}/{stem}",
                    "section": section_tag,
                    "file": str(f),
                    "line": 1,
                    "body": "",
                }
            )
    return found


def load_section_map():
    """Parse tests/.spec-section-map. Returns dict { dir_path: section_tag }."""
    out = {}
    if not SECTION_MAP_PATH.exists():
        return out
    for raw in SECTION_MAP_PATH.read_text().splitlines():
        line = raw.split("#", 1)[0].strip()
        if not line:
            continue
        parts = line.split()
        if len(parts) != 2:
            continue
        out[parts[0]] = parts[1]
    return out


def github_permalink(sha: str, file_path: str, line: int) -> str:
    return (
        f"https://github.com/{REPO_OWNER}/{REPO_NAME}/blob/{sha}/"
        f"{file_path}#L{line}"
    )


def render_block(tag: str, tests, sha: str) -> str:
    """Render the contents of one <!-- BEGIN tests:X --> block.
    Output is the body BETWEEN the markers (not including them)."""
    if not tests:
        return "\n_(no tests pin this section yet)_\n"
    bullets = []
    bodies = []
    # Tests already in source order (we collect them deterministically).
    for t in tests:
        link = github_permalink(sha, t["file"], t["line"])
        bullets.append(f"- [`{t['name']}`]({link})")
        if t["body"]:
            bodies.append(t["body"])
    parts = [
        "",
        f"**Tests** ({len(tests)}):",
        "",
        *bullets,
        "",
    ]
    if bodies:
        parts.extend(
            [
                "<details>",
                "<summary>Show test source</summary>",
                "",
                "```haskell",
                *bodies,
                "```",
                "",
                "</details>",
                "",
            ]
        )
    return "\n".join(parts)


def rewrite_doc(path: Path, blocks_by_tag, sha: str):
    """Rewrite the contents of every BEGIN/END pair in `path` to match
    `blocks_by_tag`. Returns (new_text, changed_bool, tags_seen)."""
    src = path.read_text()
    out = []
    i = 0
    tags_seen = []
    while i < len(src):
        m = BEGIN_RE.search(src, i)
        if not m:
            out.append(src[i:])
            break
        # Copy everything up to and including the BEGIN marker line.
        out.append(src[i : m.end()])
        tag = m.group(1)
        tags_seen.append(tag)
        # Find the matching END marker.
        end = END_RE.search(src, m.end())
        if not end or end.group(1) != tag:
            print(
                f"WARNING: BEGIN tests:{tag} in {path} has no matching END"
                " marker (or tag mismatch); leaving block untouched.",
                file=sys.stderr,
            )
            out.append(src[m.end() : end.end() if end else len(src)])
            i = end.end() if end else len(src)
            continue
        # Replace the body with the rendered block.
        canonical_tag = normalise_tag(tag)
        tests = blocks_by_tag.get(canonical_tag, [])
        rendered = render_block(tag, tests, sha)
        out.append(rendered)
        out.append(src[end.start() : end.end()])
        i = end.end()
    new_text = "".join(out)
    return new_text, new_text != src, tags_seen


def main():
    sha = head_sha()
    section_map = load_section_map()

    named = discover_named_tests()
    fixtures = discover_fixture_tests(section_map)
    all_tests = named + fixtures

    # Index by canonical (underscored) section tag.
    blocks_by_tag = {}
    for t in all_tests:
        tag = normalise_tag(t["section"])
        blocks_by_tag.setdefault(tag, []).append(t)
    # Sort each bucket by file path then line for deterministic output.
    for tag in blocks_by_tag:
        blocks_by_tag[tag].sort(key=lambda t: (t["file"], t["line"]))

    # Rewrite every doc.
    changed_files = []
    all_seen = set()
    for doc in sorted(DOCS_DIR.glob("*.md")):
        new_text, changed, seen = rewrite_doc(doc, blocks_by_tag, sha)
        all_seen.update(normalise_tag(t) for t in seen)
        if changed:
            if CHECK_MODE:
                changed_files.append(str(doc))
            else:
                doc.write_text(new_text)
                print(f"updated {doc}")

    # Warn about test-section tags that have no marker pair anywhere.
    unmapped = sorted(set(blocks_by_tag) - all_seen)
    if unmapped:
        print(
            "\nWARNING: these test sections have no `<!-- BEGIN tests:X -->`"
            " marker in any spec doc:\n  "
            + "\n  ".join(unmapped)
            + "\nAdd marker pairs in the appropriate doc, or rename the tests.",
            file=sys.stderr,
        )

    if CHECK_MODE:
        if changed_files:
            print(
                "\nCHECK FAILED: the following docs are out of sync:\n  "
                + "\n  ".join(changed_files)
                + "\n\nRun `compiler/scripts/sync-test-links.sh` to refresh.",
                file=sys.stderr,
            )
            sys.exit(1)
        print("all docs are in sync")
        sys.exit(0)

    print(
        f"\nlinked {len(all_tests)} tests across "
        f"{len(blocks_by_tag)} sections in {len(list(DOCS_DIR.glob('*.md')))}"
        " docs."
    )


if __name__ == "__main__":
    main()
PY
