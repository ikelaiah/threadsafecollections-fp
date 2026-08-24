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

    def write_release_source(
        self,
        root: Path,
        body: str,
        release: str,
        versions: list[dict[str, str]],
        current: str,
    ) -> None:
        source = root / "docs"
        source.mkdir(exist_ok=True)
        (source / "index.md").write_text(f"# Documentation\n\n{body}\n", encoding="utf-8")
        (source / "layout.json").write_text(
            json.dumps({"schema_version": 1, "release": release}),
            encoding="utf-8",
        )
        (source / "versions.json").write_text(
            json.dumps(
                {
                    "schema_version": 1,
                    "current": current,
                    "site_url": "https://example.invalid/threadsafecollections-fp",
                    "repository_url": "https://github.com/example/threadsafecollections-fp",
                    "versions": versions,
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

    def test_released_mode_builds_every_version_sharing_one_catalogue(self) -> None:
        versions = [
            {"release": "1.9.1", "source_ref": "v1.9.1"},
            {"release": "1.9.0", "source_ref": "v1.9.0"},
        ]
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.git(root, "init")
            self.git(root, "config", "user.email", "docs@example.invalid")
            self.git(root, "config", "user.name", "Documentation tests")
            self.write_release_source(root, "v1.9.0 documentation reference.", "1.9.0", versions, "1.9.1")
            self.git(root, "add", ".")
            self.git(root, "commit", "-m", "Release one")
            self.git(root, "tag", "v1.9.0")

            self.write_release_source(root, "v1.9.1 documentation reference.", "1.9.1", versions, "1.9.1")
            self.git(root, "add", ".")
            self.git(root, "commit", "-m", "Release two")
            self.git(root, "tag", "v1.9.1")

            self.write_release_source(root, "Development documentation reference.", "1.9.1", versions, "1.9.1")
            self.git(root, "add", ".")
            self.git(root, "commit", "-m", "Development documentation")

            # Released mode: every version's content comes from its immutable tag.
            site = root / "released-site"
            self.assertEqual(2, build_all(root, site, development_current=False))
            current_index = (site / "1.9.1" / "index.html").read_text(encoding="utf-8")
            legacy_index = (site / "1.9.0" / "index.html").read_text(encoding="utf-8")
            self.assertIn("v1.9.1 documentation reference.", current_index)
            self.assertNotIn("Development documentation reference.", current_index)
            self.assertIn("v1.9.0 documentation reference.", legacy_index)

            # Both generated versions share the same site-wide catalogue.
            option_texts = {"v1.9.1 (current)", "v1.9.0"}
            for index_html in (current_index, legacy_index):
                self.assertTrue(all(label in index_html for label in option_texts))
                self.assertNotIn("v1.9.0 (current)", index_html)

            def options(html: str) -> dict[str, tuple[str, str]]:
                import re
                return {text: (value, attrs) for value, attrs, text in
                        re.findall(r'<option value="([^"]+)"([^>]*)?>(.*?)</option>', html)}

            current_options = options(current_index)
            self.assertEqual("index.html", current_options["v1.9.1 (current)"][0])
            self.assertTrue("selected" in current_options["v1.9.1 (current)"][1])
            self.assertEqual("../1.9.0/index.html", current_options["v1.9.0"][0])
            self.assertTrue("selected" not in current_options["v1.9.0"][1])

            legacy_options = options(legacy_index)
            self.assertEqual("index.html", legacy_options["v1.9.0"][0])
            self.assertTrue("selected" in legacy_options["v1.9.0"][1])
            self.assertEqual("../1.9.1/index.html", legacy_options["v1.9.1 (current)"][0])
            self.assertTrue("selected" not in legacy_options["v1.9.1 (current)"][1])

            # Development mode: current comes from the checkout, older from the tag.
            dev_site = root / "dev-site"
            self.assertEqual(2, build_all(root, dev_site, development_current=True))
            dev_current = (dev_site / "1.9.1" / "index.html").read_text(encoding="utf-8")
            dev_legacy = (dev_site / "1.9.0" / "index.html").read_text(encoding="utf-8")
            self.assertIn("Development documentation reference.", dev_current)
            self.assertIn("v1.9.0 documentation reference.", dev_legacy)
            self.assertIn("v1.9.0", dev_current)  # selector still knows the historical version


if __name__ == "__main__":
    unittest.main()
