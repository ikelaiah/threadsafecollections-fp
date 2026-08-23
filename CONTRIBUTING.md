# Contributing

Bug reports, focused changes, tests, examples, and documentation improvements
are welcome.

## Before you start

- Read the [README](README.md) for the project's scope and intent, and the
  [Roadmap](ROADMAP.md) for planned work so your change lands where the project
  is heading.
- For behaviour changes or new API, open an issue or pull request describing the
  use case before writing a lot of code.

## Development workflow

1. Build the affected examples and run the relevant tests described in
   [Building and verification](docs/project/building.md).
2. Keep changes focused. A pull request should address one concern.
3. Add or update tests for behaviour changes. The suite builds on `fpcunit` and
   is run with `run-tests.ps1`/`run-tests.sh`.
4. For documentation changes, follow the guidance in
   [`tools/DOCUMENTATION.md`](tools/DOCUMENTATION.md) and run the documentation
   checks and builder before submitting.

## Documentation changes

The repository uses a docs-as-code workflow. The Markdown under `docs/` builds
into the versioned site at <https://ikelaiah.github.io/threadsafecollections-fp/>.
Before submitting a documentation change:

- add any new page to `docs/layout.json` (navigation or `hidden_pages`);
- keep links and anchors consistent — the builder fails on broken internal
  links;
- regenerate the generated cheat sheet with
  `pwsh -File ./tools/generate-cheatsheet.ps1` if the public API changed;
- run `tools/check-docs.ps1`, `tools/check-release-metadata.ps1`,
  `tools/check_docs.py`, and the `tools/test_*.py` tests;
- if you change a recipe, update the matching program under
  `examples/documentation/` so the two cannot drift.

## Release notes

When a release is prepared:

- add a `RELEASE-NOTES-v<version>.md` and `PR_v<version>.md` under
  `docs/history/`;
- add the release to `docs/versions.json` with its `source_ref` tag;
- link the new notes from `docs/start/index.md`.

The documentation site publishes on new GitHub releases and manual
`workflow_dispatch` runs. The initial version (v0.8.8) predates the docs
infrastructure and is published from `main` via `workflow_dispatch`; releases
tagged from `main` after the docs system landed publish from their release
event.

## License

By contributing you agree that your contributions are licensed under the
project's [MIT License](LICENSE).