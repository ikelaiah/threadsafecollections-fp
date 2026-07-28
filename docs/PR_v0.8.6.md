# Pull Request: v0.8.6 Documentation and Onboarding

[Documentation home](README.md) ·
[Release notes](RELEASE-NOTES-v0.8.6.md) ·
[Changelog](../CHANGELOG.md)

**Audience:** reviewers and maintainers preparing the v0.8.6 release.

## Summary

This documentation-only PR prepares ThreadSafeCollections-FP 0.8.6 for new and
existing users. It does not change Pascal source, public APIs, algorithms, or
runtime behavior.

## Changes

- Reorganizes the README around a verified five-minute first run, installation,
  Free Pascal conventions, collection selection, and thread-safety boundaries.
- Adds a documentation home, verified build/test guide, roadmap, and
  repository-level MIT license.
- Adds navigation and audience guidance to current references while clearly
  marking release, development, and test records as historical snapshots.
- Corrects dependency terminology, CI scope, tested-environment claims, manual
  lock guidance, benchmark options, FPCUnit options, object ownership, and
  non-cryptographic hash guidance.
- Updates the generated API cheat sheet and v0.8.6 changelog entry.

## Verification

- FPC 3.2.2 on Win64: 118 tests, 0 errors, 0 failures, and 0 HeapTrc leaks.
- All 16 examples compiled with both the PowerShell and Bash build scripts.
- README examples compiled and ran.
- The Lazarus package compiled locally with Lazarus 4.8.
- Local Markdown links, filename casing, heading anchors, fences, generated
  cheat-sheet output, trailing whitespace, and `git diff --check` were verified.

## Release check

Before tagging v0.8.6, update
`package/lazarus/ThreadSafeCollections.lpk` from 0.8.5 to 0.8.6 and regenerate
`docs/CHEATSHEET.md`; both still report package version 0.8.5 in this
documentation-only commit.
