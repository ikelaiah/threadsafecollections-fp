# ThreadSafeCollections-FP v0.8.6 Release Notes

[Documentation home](../index.md) ·
[Pull request summary](PR_v0.8.6.md) ·
[Changelog](../../CHANGELOG.md)

**Status:** Released

**Release date:** 2026-07-28

## Overview

Version 0.8.6 is a documentation and onboarding release. It makes the current
collection behavior, dependencies, tested environments, build commands, and
limitations easier to find and verify. It does not change the Pascal API or
runtime implementation.

## Highlights

- A shorter README with a verified five-minute path from checkout to a working
  generic list.
- A documentation landing page and build guide covering examples, FPCUnit,
  Lazarus, benchmarks, and generated API documentation.
- Clear guidance for Free Pascal generics, comparers, ownership, iteration, and
  per-instance locking.
- Accurate separation of local Lazarus 4.8 package verification from CI, where
  Windows installs Lazarus 4.0.0 only to obtain FPC for example compilation.
- Current guides separated from historical release, development, and test
  snapshots.
- A roadmap from the 0.8.5 implementation baseline to 2.0.0.

## Accuracy corrections

- `Generics.Collections` is identified as the standard `rtl-generics` unit used
  by the current source for `TPair`, not as part of FCL or a third-party
  dependency.
- Manual `Lock` tokens are no longer presented as a portable way to wrap calls
  to already-locking public methods.
- Benchmark and FPCUnit option scopes now match the current programs.
- XXHash32 is documented as non-cryptographic, and object values are documented
  as not automatically freed by the collections.

## Verification

The release documentation was checked with FPC 3.2.2 and Lazarus 4.8 on Win64.
All 16 examples compiled, and the full suite completed with 118 tests, 0 errors,
0 failures, and 0 HeapTrc leaks. Linux CI compiles all examples with the
distribution FPC package; macOS is not currently tested.

## Compatibility

This release is documentation-only and introduces no intended source or runtime
compatibility changes.
