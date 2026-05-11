# ThreadSafeCollections-FP v0.8.3 Release Notes

**Release Date**: 2026-05-11

## Overview

Version 0.8.3 is a documentation, tooling, package metadata, and small correctness release.

The goal of this release is to make the current v0.8.x behaviour easier to understand and verify:
the Lazarus package metadata now reports 0.8.3, the documentation has been checked against the
source code, a generated cheat sheet has been added for quick API lookup, and sorted-list
`IndexOf` now preserves the documented first-match behaviour for duplicate values.

---

## What's New

### Generated API Cheat Sheet

Added [docs/CHEATSHEET.md](docs/CHEATSHEET.md), a generated quick reference covering:

- collection types and their storage/iteration models;
- shared interface members and complexity annotations;
- public constructors;
- concrete-only dictionary members;
- built-in comparers, hash functions, and hash helpers;
- locking notes and current caveats.

The cheat sheet is generated from source files and package metadata, so it can be refreshed without
using AI or manually copying API declarations.

### Cheat Sheet Generator

Added [tools/generate-cheatsheet.ps1](tools/generate-cheatsheet.ps1).

Run it from the repository root:

```powershell
powershell -ExecutionPolicy Bypass -File tools\generate-cheatsheet.ps1
```

Or, with PowerShell 7:

```powershell
pwsh tools/generate-cheatsheet.ps1
```

The generator reads:

- `src/ThreadSafeCollections.Interfaces.pas`
- `src/ThreadSafeCollections.List.pas`
- `src/ThreadSafeCollections.Deque.pas`
- `src/ThreadSafeCollections.Dictionary.pas`
- `src/ThreadSafeCollections.HashSet.pas`
- `src/HashFunctions.pas`
- `package/lazarus/ThreadSafeCollections.lpk`

---

## Bug Fixes

### `TThreadSafeList.IndexOf` with Sorted Duplicate Values

`TThreadSafeList.IndexOf` is documented as returning the first matching index. When a list was
already marked sorted, `IndexOf` used the binary-search fast path. That path returned whichever
matching index the binary search found first, which could be the middle duplicate instead of the
first duplicate.

The binary-search path now continues searching left after finding a match, so it returns the first
matching index while keeping the O(log n) search behaviour for ascending sorted lists.

---

## Documentation Updates

The collection documentation was refreshed to better match the current source code.

Updated topics include:

- List, HashSet, Deque, and Dictionary iterator behaviour.
- RAII-style locking through `ILockToken`.
- Manual `Lock()` caveats on platforms where `TCriticalSection` is not re-entrant.
- List binary-search behaviour after ascending `Sort(True)`.
- Dictionary snapshot iteration.
- Dictionary and HashSet allocator notes.
- Bulk-operation caveats where source collections may be locked and then called through public APIs.
- XXHash32 implementation notes.

The README was also updated to point to the generated cheat sheet and to present 0.8.3 as the
latest release.

---

## Lazarus Package Metadata

The Lazarus package file has been updated:

```xml
<Version Minor="8" Release="3"/>
```

This means Lazarus should now report the package as version `0.8.3`.

---

## Behaviour and API Compatibility

This release is backward compatible with v0.8.2.

No public API changes were introduced. Existing code that builds against v0.8.2 should continue to
build against v0.8.3 without modification.

---

## Test Results

The v0.8.3 release branch was validated with the full FPCUnit test runner:

| Metric | Result |
|--------|--------|
| **Total tests** | 116 |
| **Passed** | 116 |
| **Errors / Failures** | 0 / 0 |

See [tests/LatestTestOutput.md](tests/LatestTestOutput.md) for the current run summary.

---

## Current Caveats

The documentation now calls out several behaviours that users should understand:

- `TThreadSafeList` binary search is used by `IndexOf` and `Contains` when `FSorted = True`; the
  current binary-search path assumes ascending comparer order.
- `TThreadSafeDictionary.ContainsValue` scans entries and compares values bytewise with
  `CompareByte`.
- `TThreadSafeDictionary.AddRange(ADictionary)` and
  `TThreadSafeHashSet.AddRange(Collection)` / `RemoveRange(Collection)` currently lock the source
  collection and then call public methods on it; array snapshots are the safer portable option on
  non-reentrant `TCriticalSection` platforms.
- `TThreadSafeDeque.PushRangeFront` prepends values in input order, so the last input item becomes
  the front item.

---

## Upgrading from v0.8.2

Replace the source files and Lazarus package metadata as usual.

No code changes are required. If you use Lazarus, reopen or rebuild the package so the IDE picks up
the updated package version.

---

## Resources

- **Repository**: https://github.com/ikelaiah/threadsafecollections-fp
- **Full Changelog**: [CHANGELOG.md](CHANGELOG.md)
- **Generated Cheat Sheet**: [docs/CHEATSHEET.md](docs/CHEATSHEET.md)
- **Issues**: https://github.com/ikelaiah/threadsafecollections-fp/issues
