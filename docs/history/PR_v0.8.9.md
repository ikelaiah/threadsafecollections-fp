# Pull Request: v0.8.9 API Consistency and 1.0 Preparation

[Documentation home](../index.md) ·
[Release notes](RELEASE-NOTES-v0.8.9.md) ·
[Changelog](../../CHANGELOG.md)

**Audience:** reviewers and maintainers preparing the v0.8.9 release.

## Summary

This PR implements the v0.8.9 roadmap milestone: API consistency and 1.0
preparation. It hardens the four existing collection APIs, finalizes the
`Lock()` policy, normalizes construction and exception contracts, resolves the
HashSet comparer naming, records the documentation-system work merged after
v0.8.8, and adds regression coverage for every changed contract. No new
collection family is introduced and no v1.1 atomic-workflow API is added.

## Changes

- **Lock policy (Task 2):** adds `TRecursiveCriticalSection` in
  `ThreadSafeCollections.Interfaces` and uses it for every collection lock and
  for `TLockToken`. Same-thread re-entry and nested tokens are now safe on all
  platforms; other threads remain excluded. The previously Windows-only
  re-entry test now runs everywhere, and new tests cover re-entrant public
  calls, nested tokens, compound check-then-update, and lock-holding iteration
  calling public methods.
- **Exception contracts (Task 6):** normalizes bounds/range/capacity errors to
  `EArgumentOutOfRangeException`, empty List/Deque access to `EListError`,
  Dictionary duplicate-key to `EArgumentException` (missing key stays
  `EKeyNotFoundException`), and invalid enumerator position to
  `EInvalidOperation` everywhere (List enumerator now guards `Current`).
- **Construction (Task 3):** generic HashSet constructor rejects nil equality
  and hash callbacks; Dictionary `Create(nil, nil)` continues to select
  built-in defaults (tested); capacity clamps and power-of-two rounding are
  documented and tested; Dictionary `Count` becomes a read-only property.
- **HashSet comparer naming (Task 4):** `THashSetEqualityComparer<T>` is the
  1.0-facing name; the legacy `TEqualityComparer<T>` alias is retained and
  marked deprecated for new code. New mixed-generics regression tests prove
  Dictionary + HashSet + `Generics.Defaults`/`Generics.Collections` bind to the
  correct units.
- **Dictionary concrete-only API (Task 5):** `First`, `Last`, `BucketCount`,
  and `ResizeBuckets` documented as intentional concrete-only members that are
  not part of `IThreadSafeDictionary`.
- **Stale comments/claims (Task 8):** Dictionary source header cleaned (removed
  stale version and the "Delphi's TDictionary interface" claim); iteration and
  locking doc comments corrected to the real snapshot and re-entrancy behavior.
- **Post-v0.8.8 docs work (Task 9):** the docs-as-code system, versioned
  GitHub Pages publishing, documentation tooling and checks, verified recipe
  examples, and the homepage-banner-width fix are recorded in the changelog and
  release notes.
- **Release engineering (Task 10):** Windows CI now runs the Lazarus
  `package-smoke` job through `smoke-package.ps1`; the Linux package smoke,
  example builds, FPCUnit suites, and documentation checks remain.
- **Tests:** 29 new FPCUnit tests across
  `ThreadSafeCollections.ApiConsistencyTests`,
  `ThreadSafeCollections.GenericsMixedTests`, and `ThreadSafeHashSetTests`
  (suite: 146 → 175).

## Compatibility

- `Dictionary.Count` changed from a method to a read-only property; source that
  reads `Dict.Count` keeps compiling unchanged. Interface usage is unchanged.
- Exception classes for bounds/empty/duplicate/invalid-enumerator errors became
  specific FPC subclasses instead of a bare `Exception`; `Union`-style handlers
  that catch `Exception` are unaffected, and `List.Extract` on a missing value
  still raises `EArgumentOutOfRangeException`.
- `Lock()` semantics expanded from "unsafe to call public methods under a
  token" to "safe, re-entrant for the owning thread". No previously supported
  pattern was removed.
- The HashSet legacy `TEqualityComparer<T>` name remains accepted.

## Verification

Run locally on Windows x86-64 with FPC 3.2.2:

- Full FPCUnit suite: 175 tests, 0 errors, 0 failures; HeapTrc reports 0
  unfreed memory blocks.
- All 16 tracked examples compile via `build-examples.ps1`.
- Documentation tooling tests, `check_built_docs`, recipe checks, PowerShell
  doc checks, and release-metadata checks all pass.