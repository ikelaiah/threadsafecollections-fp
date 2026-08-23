# ThreadSafeCollections-FP v0.8.4 Release Notes

[Documentation home](../index.md) · [Current changelog](../../CHANGELOG.md)

> **Historical release snapshot.** This document records v0.8.4 and may contain
> older APIs, measurements, toolchains, or support statements. Use the
> [current documentation](../index.md) for the present checkout.

**Audience:** users researching the v0.8.4 release and project maintainers.

**Release Date**: 2026-07-16

## Overview

Version 0.8.4 is a correctness and POSIX portability release. It resolves the known behavioral
caveats documented in v0.8.3: descending sorted-list lookup now follows the active sort direction,
Dictionary and HashSet collection bulk operations no longer nest locks on their source, and
Dictionary key and value lookup now use type-aware equality.

This release also repairs the relative documentation links that were broken when release and
maintenance documents moved into `docs/` for v0.8.3.

## Correctness Fixes

### Direction-aware sorted-list lookup

`TThreadSafeList` now stores both whether it is sorted and which direction is active.

- `IndexOf` and `Contains` use O(log n) binary search after ascending or descending `Sort`.
- Duplicate lookup returns the first matching index in either direction.
- `Add`, `AddRange`, and `Replace` preserve the sorted flag only when new values respect the
  active direction.

### Portable collection bulk operations

The following collection overloads now obtain a source snapshot by calling `ToArray`, release the
source lock, and then mutate the destination:

- `TThreadSafeDictionary.AddRange(ADictionary)`
- `TThreadSafeHashSet.AddRange(Collection)`
- `TThreadSafeHashSet.RemoveRange(Collection)`

The previous implementation manually locked the source and then called public source methods that
acquired the same lock again. Because `TCriticalSection` is non-reentrant on POSIX, that could
deadlock. The snapshot-first implementation is portable and also makes self-source operations safe.

### Type-aware Dictionary key and value equality

`TThreadSafeDictionary.ContainsValue` now compares values with
`Generics.Defaults.TEqualityComparer<TValue>.Default`. This replaces raw `CompareByte` equality,
which compared the pointer fields of managed types and could fail for equal strings stored in
different allocations.

Default key equality now uses the corresponding `TEqualityComparer<TKey>.Default` implementation
as well. Custom key equality functions still take precedence. This fixes lookup, containment, and
removal when an equal managed key is supplied from a different allocation.

## Documentation Link Repairs

Corrected relative links in:

- the root [README](../../README.md);
- the v0.8.0 through v0.8.3 release notes;
- the maintainability guide;
- links to source files, tests, the changelog, the cheat sheet, and the generator script.

## Compatibility

Version 0.8.4 introduces no breaking public API changes. Existing v0.8.3 code should compile
without modification. `ContainsValue` now follows semantic default equality instead of the old
bytewise behavior.

## Validation

The release is built with Free Pascal 3.2.2 for Win64 and validated with the FPCUnit test runner:

| Metric | Result |
|---|---:|
| Tests | 117 |
| Errors | 0 |
| Failures | 0 |

Regression coverage includes descending sorted lookup, managed-string key/value equality, and
self-source bulk operations.

The Lazarus package also compiles successfully as `ThreadSafeCollections 0.8.4`.

See [LatestTestOutput.md](../../tests/LatestTestOutput.md) for the final test summary.

## Upgrading from v0.8.3

Replace the source files and rebuild the Lazarus package. No application code changes are required.

If your application worked around the old bulk-operation locking caveat by creating arrays before
calling `AddRange` or `RemoveRange`, that workaround remains valid but is no longer necessary.

## Resources

- **Repository**: https://github.com/ikelaiah/threadsafecollections-fp
- **Full Changelog**: [CHANGELOG.md](../../CHANGELOG.md)
- **Generated Cheat Sheet**: [../start/cheat-sheet.md](../start/cheat-sheet.md)
- **Issues**: https://github.com/ikelaiah/threadsafecollections-fp/issues
