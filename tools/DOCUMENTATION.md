# Maintaining the ThreadSafeCollections-FP documentation site

The static publisher is intentionally dependency-free. It reads the Markdown
under `docs/`, produces a versioned GitHub Pages artifact, and packages an
offline copy with a SHA-256 checksum.

## Add or reorganise a page

1. Add the Markdown page under `docs/`.
2. Add its path to `required_pages` in `docs/layout.json`.
3. Add it to a `navigation` section for a primary page, or to `hidden_pages`
   when keeping an existing URL without placing it in the main sidebar.
4. Run the checks below.

`hidden_pages` are still generated and indexed so existing bookmarks remain
useful; they are simply not first-class navigation.

The documentation home page is `docs/index.md`, and its hero/actions/cards come
from the `homepage` block in `docs/layout.json`.

## Change a recipe

Recipes live in `docs/guides/recipes.md`. Every Pascal code block must match a
compiled-and-run program under `examples/documentation/` with the same expected
output, otherwise `tools/check_docs.py` and `tools/test_docs_examples.py` fail:

- edit the matching `examples/documentation/NN_*.pas` program and its
  `NN_*.pas.output` expectation;
- update the embedded block in the recipe to match the program exactly;
- refer to it with `[Source program](../../examples/documentation/NN_*.pas)`.

## Add a version

Add a `release` and `source_ref` entry to `docs/versions.json`. Only add a
historical release after its ref contains the compatible documentation source
(`docs/layout.json`, `docs/versions.json`, and `tools/`). The current release is
built from the checkout by default; `--released` builds every declared version
from its immutable `source_ref` tag and is what the release-triggered Pages
workflow uses.

## Publishing the first version (v0.8.8)

The documentation system starts at **v0.8.8**, released 2026-08-17. Because the
`v0.8.8` git tag predates the docs-as-code infrastructure (it has no
`docs/layout.json` or `tools/`), it cannot be built from its tag. Publish it
with **Actions → Publish documentation → Run workflow** on `main`
(`workflow_dispatch`), which builds the current release from the checkout.

Future releases tagged from `main` after this system landed carry the
infrastructure, so they can be published from their `release` event with
`--released`. When a version's tag predates the infrastructure, it must be
dropped from `docs/versions.json` before a later immutable build succeeds.

## Local verification

```text
pwsh -NoProfile -File ./tools/check-release-metadata.ps1 -ExpectedVersion 0.8.8
pwsh -NoProfile -File ./tools/check-docs.ps1
python tools/test_docs_examples.py
python tools/test_build_docs.py
python tools/test_build_all_docs.py
python tools/test_check_built_docs.py
python tools/test_check_docs.py
python tools/check_docs.py
python tools/build_all_docs.py --site-root site --offline-dir artifacts
python tools/check_built_docs.py --site site
```

The resulting archive is `artifacts/threadsafecollections-fp-docs-<version>.zip`
with a matching `.sha256` checksum. Open `site/index.html` through a local web
server when visually checking the site.

The repository's `ci.yml` runs the PowerShell and Python checks on every push,
and `.github/workflows/documentation.yml` builds and deploys the Pages site on
release publication or manual dispatch.