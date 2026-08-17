# ThreadSafeCollections-FP v0.8.8 Release Notes

[Documentation home](README.md) ·
[Pull request summary](PR_v0.8.8.md) ·
[Changelog](../CHANGELOG.md)

**Status:** Released

**Release date:** 2026-08-17

## Overview

Version 0.8.8 is a concurrency hardening release. It adds three test suites
that exercise races, deadlocks, iteration, resize, and lifetime behavior
across the four collection families, and it records the project's iterator
policy decision. It does not change the Pascal API or runtime implementation.

## Highlights

- A deterministic, event-coordinated concurrency suite covers concurrent add,
  remove, lookup, resize, bulk, collision, enumeration, interface, and
  callback workloads for List, Deque, Dictionary, and HashSet, with exact
  final-state verification (146 tests in total).
- Bounded-completion deadlock regressions cover opposite lock order,
  self-source bulk operations, lock-holding enumeration with concurrent
  mutation, manual lock token serialization, the Windows-only `Lock()`
  re-entry behavior, and lifetime/destruction boundaries. A regression that
  does not complete within its bound fails the runner instead of hanging it.
- Seeded randomized stress tests cover add, remove, contains, clear, resize,
  enumeration, ranges, and poor hashes across all four collections, a
  poor-hash dictionary, and a mixed four-collection workload. Every run logs
  its seed, thread count, iterations, collection, and operation mix;
  `STRESS_SEED` overrides the seed.
- The iterator policy is decided and documented: lock-holding enumeration for
  List, Deque, and HashSet and snapshot enumeration for Dictionary remain
  intentionally different, with the trade-offs recorded in
  [Thread-safety, iteration, and lock policy](Thread-Safety-and-Iteration.md).

## Verification

- FPC 3.2.2 on Win64: 146 tests, 0 errors, 0 failures, and 0 HeapTrc leaks.
- The three new suites were run repeatedly and completed deterministically;
  the full suite runs in CI on Windows and Linux.
- All 16 examples continue to compile, the Lazarus package builds as
  `ThreadSafeCollections 0.8.8`, and the documentation and release-metadata
  checks pass.

## Compatibility

This release is verification- and documentation-only and introduces no
intended source or runtime compatibility changes.
