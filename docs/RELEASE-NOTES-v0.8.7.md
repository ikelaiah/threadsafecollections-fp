# ThreadSafeCollections-FP v0.8.7 Release Notes

[Documentation home](README.md) ·
[Pull request summary](PR_v0.8.7.md) ·
[Changelog](../CHANGELOG.md)

**Status:** Released

**Release date:** 2026-08-16

## Overview

Version 0.8.7 is a CI and verification release. It turns the project's
existing correctness claims into automated checks: the full FPCUnit suite now
runs in CI on Windows and Linux, the Lazarus package gets an automated smoke
build, and documentation and release metadata are checked automatically. It
does not change the Pascal API or runtime implementation.

## Highlights

- Full FPCUnit suite (fast unit, collision, and stress tests together) with
  HeapTrc leak verification now runs automatically on Windows CI and on Linux
  CI; `cthreads` was added to the test runner so Unix threaded tests run.
- A package smoke build (`lazbuild`, version check, source-inclusion check,
  compiled-unit check, and a tiny consumer program) runs on Linux CI and
  locally on Windows.
- Automated documentation checks cover local links, heading fragments, code
  fences, the example inventory, stale cheat sheets, and compilable examples
  through the existing example-build jobs.
- Automated release-metadata checks keep the README badge, package metadata,
  documentation home, changelog, release notes, and generated cheat sheet on
  one version.
- CI jobs are separated by concern (examples, tests, packaging,
  documentation), so a failing report names the area that failed.
- The Lazarus package metadata was brought forward to 0.8.7, and the recorded
  v0.8.6 release date was corrected to match git history.

## Verification

- FPC 3.2.2 on Win64: 118 tests, 0 errors, 0 failures, and 0 HeapTrc leaks
  through both `run-tests.ps1` and `run-tests.sh`.
- The Lazarus package compiled as `ThreadSafeCollections 0.8.7` with lazbuild,
  and the package smoke consumer passed on Windows with Lazarus 4.8.
- `tools/check-docs.ps1` and `tools/check-release-metadata.ps1` pass on the
  current tree; all 16 examples continue to compile with both build scripts.
- Linux CI additionally compiles examples, runs the FPCUnit suite, and builds
  the package with the distribution toolchain.

## Compatibility

This release is verification-only and introduces no intended source or runtime
compatibility changes.
