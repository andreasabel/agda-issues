import json
from pathlib import Path
import subprocess
import tempfile
import unittest

from import_git_bug import export_issues, write_json


class ExportTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.repo = Path(self.directory.name)
        self.git("init", "--quiet")

    def git(self, *args, input=None):
        return subprocess.check_output(
            ["git", "-C", str(self.repo), *args], input=input, text=True
        ).strip()

    def commit(self, ops, parent=None, version=4):
        blob = self.git("hash-object", "-w", "--stdin", input=json.dumps({"ops": ops}))
        empty = self.git("hash-object", "-w", "--stdin", input="")
        tree = self.git("mktree", input=(
            f"100644 blob {blob}\tops\n100644 blob {empty}\tversion-{version}\n"
        ))
        parents = ["-p", parent] if parent else []
        commit = self.git(
            "-c", "user.name=Test", "-c", "user.email=test@example.org",
            "commit-tree", tree, *parents, input="fixture\n",
        )
        self.git("update-ref", "refs/bugs/test", commit)
        return commit

    def create(self, url="https://github.com/agda/agda/issues/42"):
        return {"type": 1, "timestamp": 100, "metadata": {"github-url": url}}

    def test_close_reopen_and_close_across_packs(self):
        first = self.commit([self.create(), {"type": 4, "status": 2, "timestamp": 200}])
        second = self.commit([{"type": 4, "status": 1, "timestamp": 300}], first)
        record = export_issues(self.repo, "agda", "agda")["agda"]["42"]
        self.assertEqual(record, {
            "created_at": "1970-01-01T00:01:40+00:00", "closed_at": None,
            "is_pull_request": False, "assignee": "None",
        })
        self.commit([{"type": 4, "status": 2, "timestamp": 400}], second)
        data = export_issues(self.repo, "agda", "agda")
        self.assertEqual(data["agda"]["42"]["closed_at"], "1970-01-01T00:06:40+00:00")
        output = self.repo / "issues.json"
        output.write_text("previous export")
        write_json(output, data)
        self.assertEqual(json.loads(output.read_text()), data)

    def test_rejects_unknown_format(self):
        self.commit([self.create()], version=5)
        with self.assertRaisesRegex(ValueError, "Missing git-bug object"):
            export_issues(self.repo, "agda", "agda")

    def test_filters_other_repositories(self):
        self.commit([self.create("https://github.com/other/repo/issues/42")])
        with self.assertRaisesRegex(ValueError, "No GitHub issues"):
            export_issues(self.repo, "agda", "agda")

    def test_failed_write_preserves_existing_export(self):
        output = self.repo / "issues.json"
        output.write_text("previous export")
        with self.assertRaises(TypeError):
            write_json(output, {"invalid": object()})
        self.assertEqual(output.read_text(), "previous export")
        self.assertEqual(list(self.repo.glob(".issues.json.*")), [])


if __name__ == "__main__":
    unittest.main()
