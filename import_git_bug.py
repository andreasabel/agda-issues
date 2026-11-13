#!/usr/bin/env python3
"""Export GitHub issues from git-bug v4 objects without network access.

The GitHub bridge does not store pull requests or assignees. This exports
issues only, using the existing chart input schema and "None" for assignees.
"""

import argparse
from contextlib import contextmanager
from datetime import date, datetime, timezone
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile


@contextmanager
def object_reader(repo):
    # One persistent Git process instead of a subprocess (or request) per issue.
    with subprocess.Popen(
        ["git", "-C", str(repo), "cat-file", "--batch"],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE,
    ) as process:
        def read(name):
            process.stdin.write(name.encode() + b"\n")
            process.stdin.flush()
            header = process.stdout.readline().split()
            if len(header) != 3:
                raise ValueError(f"Missing git-bug object: {name}")
            content = process.stdout.read(int(header[2]))
            if process.stdout.read(1) != b"\n":
                raise ValueError(f"Truncated Git object: {name}")
            return content

        try:
            yield read
        finally:
            process.stdin.close()
            process.wait()


def iso_time(timestamp):
    return datetime.fromtimestamp(timestamp, timezone.utc).isoformat()


def apply_ops(record, ops, url_pattern):
    record = record.copy() if record else {}
    for op in ops:
        if op["type"] == 1:
            url = op.get("metadata", {}).get("github-url", "")
            match = url_pattern.fullmatch(url)
            record = {
                "number": match.group(1) if match else None,
                "created_at": iso_time(op["timestamp"]),
                "closed_at": None,
                "is_pull_request": False,
                "assignee": "None",
            }
        elif op["type"] == 4:
            if op["status"] not in (1, 2):
                raise ValueError(f"Unknown git-bug status: {op['status']}")
            record["closed_at"] = (
                iso_time(op["timestamp"]) if op["status"] == 2 else None
            )
    return record


def export_issues(repo, org, project):
    def git(*args, **kwargs):
        return subprocess.check_output(
            ["git", "-C", str(repo), *args], text=True, **kwargs
        )

    # Snapshot the tips so a concurrent bridge pull cannot change this export.
    tips = git("for-each-ref", "--format=%(objectname)", "refs/bugs/").splitlines()
    if not tips:
        raise ValueError("No git-bug issues found; run git-bug bridge pull first")
    commits = git(
        "rev-list", "--reverse", "--topo-order", "--parents", "--stdin",
        input="\n".join(tips) + "\n",
    ).splitlines()
    pattern = re.compile(
        rf"https://github\.com/{re.escape(org)}/{re.escape(project)}/issues/(\d+)"
    )
    states = {}
    with object_reader(repo) as read:
        for line in commits:
            commit, *parents = line.split()
            if len(parents) > 1:
                raise ValueError("Merged git-bug histories are not supported")
            # Fail safely if git-bug changes its on-disk format.
            read(f"{commit}:version-4")
            pack = json.loads(read(f"{commit}:ops"))
            states[commit] = apply_ops(
                states[parents[0]] if parents else {}, pack["ops"], pattern
            )
    issues = {}
    for tip in tips:
        record = states[tip].copy()
        number = record.pop("number", None)
        if number is not None:
            if number in issues:
                raise ValueError(f"Multiple git-bug issues for GitHub #{number}")
            issues[number] = record
    if not issues:
        raise ValueError(f"No GitHub issues found for {org}/{project}")
    return {project: dict(sorted(issues.items(), key=lambda item: int(item[0])))}


def write_json(path, data):
    # Keep any previous export intact if conversion or writing is interrupted.
    temporary = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w", encoding="utf-8", dir=path.parent,
            prefix=f".{path.name}.", delete=False,
        ) as output:
            temporary = output.name
            json.dump(data, output, indent=2)
            output.write("\n")
            output.flush()
            os.fsync(output.fileno())
        os.replace(temporary, path)
    finally:
        if temporary and os.path.exists(temporary):
            os.unlink(temporary)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--git-bug-repo", type=Path, default=Path.home() / "agda")
    parser.add_argument("--org", default="agda")
    parser.add_argument("--repo", default="agda")
    parser.add_argument("-o", "--output", type=Path)
    args = parser.parse_args()
    output = args.output or Path(f"{args.org}-{date.today()}-issues.json")
    try:
        data = export_issues(args.git_bug_repo.expanduser(), args.org, args.repo)
        write_json(output, data)
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        parser.exit(1, f"Export failed: {error}\n")
    print(f"Wrote {len(data[args.repo])} issues to {output}")
    print("Pull requests omitted; assignees unavailable (set to 'None').")


if __name__ == "__main__":
    main()
