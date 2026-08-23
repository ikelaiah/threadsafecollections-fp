#!/usr/bin/env python3
"""Regression tests for building all declared documentation releases."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))

from build_all_docs import build_all  # noqa: E402


class BuildAllDocsTests(unittest.TestCase):
    @staticmethod
    def git(root: Path, *arguments: str) -> None:
        subprocess.run(["git", "-C", str(root), *arguments], check=True, text=True, capture_output=True)

    def write_release_source(self, root: Path, body: str) -> None:
        source = root / "docs"
        source.mkdir(exist_ok=True)
        (source / "index.md").write_text(f"# Documentation\n\n{body}\n", encoding="utf-8")
        (source / "layout.json").write_text(
            json.dumps({"schema_version": 1, "release": "1.9.2"}),
            encoding="utf-8",
        )
        (source / "versions.json").write_text(
            json.dumps(
                {
                    "schema_version": 1,
                    "current": "1.9.2",
                    "site_url": "https://example.invalid/threadsafecollections-fp",
                    "repository_url": "https://github.com/example/threadsafecollections-fp",
                    "versions": [{"release": "1.9.2", "source_ref": "v1.9.2"}],
                }
            ),
            encoding="utf-8",
        )

    def test_builds_the_current_release_without_a_git_worktree(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "docs"
            source.mkdir()
            (source / "index.md").write_text("# Documentation\n", encoding="utf-8")
            (source / "layout.json").write_text(json.dumps({"schema_version": 1, "release": "1.9.1"}), encoding="utf-8")
            (source / "versions.json").write_text(
                json.dumps(
                    {
                        "schema_version": 1,
                        "current": "1.9.1",
                        "site_url": "https://example.invalid/ThreadSafeCollections-FP",
                        "repository_url": "https://github.com/example/ThreadSafeCollections-FP",
                        "versions": [{"release": "1.9.1", "source_ref": "main"}],
                    }
                ),
                encoding="utf-8",
            )

            count = build_all(root, root / "site", root / "artifacts")

            self.assertEqual(1, count)
            self.assertTrue((root / "site" / "1.9.1" / "index.html").is_file())
            self.assertTrue((root / "artifacts" / "threadsafecollections-fp-docs-1.9.1.zip").is_file())

    def test_released_mode_reads_current_docs_from_the_declared_tag(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.git(root, "init")
            self.git(root, "config", "user.email", "docs@example.invalid")
            self.git(root, "config", "user.name", "Documentation tests")
            self.write_release_source(root, "Released documentation reference.")
            self.git(root, "add", ".")
            self.git(root, "commit", "-m", "Release documentation")
            self.git(root, "tag", "v1.9.2")

            self.write_release_source(root, "Development documentation reference.")
            self.git(root, "add", ".")
            self.git(root, "commit", "-m", "Development documentation")

            site = root / "released-site"
            self.assertEqual(1, build_all(root, site, development_current=False))
            released_html = (site / "1.9.2" / "index.html").read_text(encoding="utf-8")
            self.assertIn("Released documentation reference.", released_html)
            self.assertNotIn("Development documentation reference.", released_html)


if __name__ == "__main__":
    unittest.main()
