# Pull Request: v0.8.8 Concurrency Hardening

[Documentation home](../index.md) ·
[Release notes](RELEASE-NOTES-v0.8.8.md) ·
[Changelog](../../CHANGELOG.md)

**Audience:** reviewers and maintainers preparing the v0.8.8 release.

## Summary

This PR implements the v0.8.8 roadmap milestone, concurrency hardening. It
adds three FPCUnit suites that exercise races, deadlocks, iteration, resize,
and lifetime behavior for all four collection families, and it documents the
project's iterator policy decision. It does not change Pascal source, public
APIs, algorithms, or runtime behavior.

## Changes

- Adds `tests/ThreadSafeCollections.ConcurrencyTests.pas`
  (`TThreadSafeConcurrencyTests`, 11 tests): deterministic, event-coordinated
  concurrent add/remove/lookup, bulk add, resize, collision lookup,
  enumeration-with-mutation, interface-backed, and callback tests. Final
  states are verified exactly via sorted snapshots.
- Adds `tests/ThreadSafeCollections.DeadlockTests.pas`
  (`TThreadSafeDeadlockTests`, 11 tests): bounded-completion regressions for
  opposite lock order (`Dictionary.AddRange`, `HashSet.IntersectWith`,
  `List.AddRange`), self-source bulk operations, lock-holding enumeration
  with concurrent mutation, manual lock token serialization, Windows-only
  `Lock()` re-entry, and destruction after concurrent work. A test exceeding
  its bound halts the runner instead of hanging it.
- Adds `tests/ThreadSafeCollections.StressTests.pas`
  (`TThreadSafeRandomizedStressTests`, 6 tests): seeded randomized stress
  with fixed defaults; every run logs seed, thread count, iterations,
  collection, and operation mix; `STRESS_SEED` overrides the seed. Final
  states are verified deterministically after each randomized phase.
- Registers the new suites in `tests/TestRunner.lpr` (118 → 146 tests).
- Adds `docs/Thread-Safety-and-Iteration.md`, recording the decided iterator
  policy: lock-holding enumeration for List, Deque, and HashSet and snapshot
  enumeration for Dictionary remain intentionally different, with the
  trade-offs, usage rules, manual-lock limits, and lifetime rules documented.
- Updates README, changelog, roadmap baselines, building guide, docs home,
  test snapshot, package metadata (0.8.8), generated cheat sheet, and CI
  expected versions.

## Verification

- FPC 3.2.2 on Win64: 146 tests, 0 errors, 0 failures, and 0 HeapTrc leaks
  through `run-tests.ps1`.
- The three new suites were each run three times in isolation with zero
  failures; bounded waits and deterministic assertions keep them
  reproducible.
- All 16 examples compile with both build scripts; the Lazarus package builds
  as 0.8.8 with lazbuild; `tools/check-docs.ps1` and
  `tools/check-release-metadata.ps1` pass on the current tree.

## Notes for reviewers

- The randomized stress tests are deterministic by default and reproducible
  via the logged seed; no timing-based assertions are used for pass/fail in
  the stress or deadlock suites.
- The Windows-only re-entrant `Lock()` test documents a platform difference
  that v0.8.9 is scheduled to redesign; it is skipped on POSIX.
