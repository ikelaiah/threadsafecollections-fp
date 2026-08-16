# Pull Request: v0.8.7 CI and Verification

[Documentation home](README.md) ·
[Release notes](RELEASE-NOTES-v0.8.7.md) ·
[Changelog](../CHANGELOG.md)

**Audience:** reviewers and maintainers preparing the v0.8.7 release.

## Summary

This PR implements the v0.8.7 roadmap milestone, CI and verification. It
automates the practical FPCUnit suite, a Lazarus package smoke build, and
inexpensive documentation and release-metadata checks. It does not change
Pascal source, public APIs, algorithms, or runtime behavior.

## Changes

- Adds `run-tests.ps1` and `run-tests.sh`, which compile the FPCUnit runner
  with debug checks and HeapTrc, run the full suite, fail on errors,
  failures, or unfreed memory blocks, and preserve the raw output under
  `build-temp/tests/`.
- Links `cthreads` in `tests/TestRunner.lpr` on Unix so the threaded
  collision and stress tests can run on Linux.
- Adds `smoke-package.ps1` and `smoke-package.sh`, which verify the package
  version, confirm every `src` unit is listed in the package, build with
  `lazbuild --build-all`, verify the compiled units, and compile and run the
  tiny consumer `tools/package-smoke-consumer.lpr`.
- Adds `tools/check-docs.ps1` (links, heading fragments, fences, example
  inventory, stale cheat sheet) and `tools/check-release-metadata.ps1`
  (one-version consistency across README badge, package, changelog, release
  notes, documentation home, and cheat sheet).
- Expands `.github/workflows/ci.yml` with separate test, package-smoke, and
  documentation jobs alongside the existing example-build jobs, so failing
  reports identify examples, tests, packaging, or documentation.
- Bumps `package/lazarus/ThreadSafeCollections.lpk` to 0.8.7, regenerates
  `docs/CHEATSHEET.md`, and updates README, changelog, building guide, docs
  home, roadmap, release notes, and the test snapshot.
- Corrects the recorded v0.8.6 release date (2027-06-28 → 2026-07-28) so
  release records match git history.

## Verification

- FPC 3.2.2 on Win64: 118 tests, 0 errors, 0 failures, and 0 HeapTrc leaks
  through `run-tests.ps1` and `run-tests.sh` (Git Bash).
- Package smoke build passed on Windows with Lazarus 4.8 (lazbuild, all 7
  units, consumer run) through both `smoke-package.ps1` and
  `smoke-package.sh`.
- `tools/check-docs.ps1` and `tools/check-release-metadata.ps1` pass on the
  current tree.
- All 16 examples compile with both build scripts; local links, heading
  anchors, fences, and `git diff --check` are clean.

## Release check

The workflow files and `tools/check-release-metadata.ps1` hard-code the
current version (`0.8.7`); bump them together with the package metadata on
the next release.
