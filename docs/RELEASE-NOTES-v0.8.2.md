# ThreadSafeCollections-FP v0.8.2 Release Notes

[Documentation home](README.md) · [Current changelog](../CHANGELOG.md)

> **Historical release snapshot.** This document records v0.8.2 and may contain
> older APIs, measurements, toolchains, or support statements. Use the
> [current documentation](README.md) for the present checkout.

**Audience:** users researching the v0.8.2 release and project maintainers.

**Release Date**: 2026-04-14

## Overview

Version 0.8.2 is a correctness and performance release. It resolves a set of serious concurrency
and memory-safety bugs discovered during code review, while also delivering measurable runtime
performance improvements across all collection types and a new standalone benchmark program.

All changes are fully backward compatible — no public API modifications.

---

## Critical Bug Fixes

### Re-entrant Lock Deadlocks (POSIX)

Free Pascal's `TCriticalSection` is **not re-entrant** on Linux and macOS. Several public methods
were acquiring the lock and then calling other public methods that also acquire it, causing
deadlocks on POSIX platforms.

**Fix**: Private internal helpers (`InternalAdd`, `InternalDelete`, `InternalIndexOf`,
`InternalSetCapacity`, `InternalRemove`) were introduced across List, HashSet, and Dictionary.
Public locked methods now delegate to these unlocked internals instead of calling each other.

Affected methods:

| Collection | Affected Methods |
|------------|-----------------|
| **List** | `Extract`, `ExtractAt`, `AddRange`, `InsertRange`, `FromArray`, `TrimExcess` |
| **HashSet** | `AddRange`, `RemoveRange` |
| **Dictionary** | `AddOrSetValue`, `AddRange` |
| **Dictionary `TEnumerator`** | `MoveNext` was double-acquiring the lock it already held |

### Memory Safety — Managed Types

Raw `Move()` and `FillChar()` bypass Free Pascal's reference-counting mechanism. Using them on
managed types (`string`, `interface`, dynamic arrays) causes use-after-free or double-free errors.

**Fix**: All affected operations in `TThreadSafeList` now use element-wise assignment and
`Default(T)` to zero vacated slots, ensuring reference counts are always maintained correctly.
`TThreadSafeDeque.Clear` received the same fix.

Affected methods: `ToArray`, `FromArray`, `InsertRange`, `DeleteRange`, `MoveItem`, `Insert`,
`Delete` (List); `Clear` (Deque).

---

## High-Severity Bug Fixes

### ABBA Deadlock in `HashSet.IntersectWith`

Two simultaneous calls `A.IntersectWith(B)` and `B.IntersectWith(A)` produced a classic ABBA
deadlock: each call acquired its own lock then attempted to acquire the other's lock.

**Fix**: The other collection is now snapshotted via `ToArray` *before* `Self`'s lock is acquired,
eliminating the lock ordering problem entirely.

### `HashSet.IntersectWith` — Incorrect Item Removal

The `ToRemove` array was pre-allocated to full capacity. Unfilled slots defaulted to `Default(T)`,
so `RemoveRange` could accidentally remove the zero-value or empty-string element from the set.

**Fix**: Live buckets are iterated directly and only real candidate items are collected, using an
exact count.

---

## Medium and Low-Severity Bug Fixes

| Severity | Location | Issue |
|----------|----------|-------|
| Medium | **List `IntegerComparer`** | `A - B` overflows when `A = MaxInt` and `B < 0`. Replaced with safe three-way comparison. |
| Medium | **List sorted-state docs** | The current code stores a Boolean `FSorted` flag, not sort direction. Binary-search lookups assume ascending comparer order, so documentation now scopes the O(log n) path to `Sort(True)`. |
| Low | **`HashFunctions.XXHash32`** | `@Key[1]` formed an invalid pointer on empty string. Added early-exit guard before dereferencing. |
| Low | **Dictionary** | Removed ~20 dead `DEBUG_LOGGING` `WriteLn` blocks and the `DEBUG_LOGGING` constant itself. |
| Low | **Dictionary** | Standardised locking calls from `FLock.Enter`/`Leave` to `FLock.Acquire`/`Release`, consistent with all other collections. |

---

## Performance Improvements

### Hashing — XXHash32 4-Lane Path

`XXHash32` now uses a full 4-lane accumulator path for strings ≥ 16 bytes, processing 16 bytes
per iteration. Strings < 16 bytes retain the original single-lane path.

**Measured improvement**: 19–23% faster on Dictionary key operations at 1M items.

### Dictionary — Cached Key-Kind Dispatch

`TypeInfo(TKey)` is now evaluated once at construction time and stored as a `TKeyKind` enum,
replacing two pointer comparisons on every hash call with a single `case` branch.

**Affects**: `Add`, `TryGetValue`, `Remove`, `ContainsKey` — all operations that hash a key.

### Dictionary and HashSet — Slab Allocator

Per-entry `New`/`Dispose` calls have been replaced with a `TEntryAllocator` slab allocator that
hands out `TEntry` records from flat blocks of 256. Freed entries go onto an intrusive freelist
and are reused before the bump pointer advances. `Clear`/`Destroy` bulk-free all backing blocks.

**Measured improvements at 1M items:**
- Dictionary operations: **19–41% faster**
- HashSet `Add`: **15–19% faster**

### List — Binary Search on Sorted Lists

`InternalIndexOf` now routes through `InternalBinarySearch` whenever `FSorted = True`, reducing
`Contains` and `IndexOf` from **O(n) to O(log n)** for ascending sorted lists.

The current binary-search implementation assumes ascending comparer order. Use `Sort(True)`
before relying on the O(log n) lookup path.

### Deque — `PushRangeBack` / `PushRangeFront` (Fix + Performance)

The previous implementation computed the required capacity in an outer loop but called `Grow`
(which copies the full buffer) in a separate inner loop — O(n·k) copies for k doublings.
Now the required capacity is computed in a single pass and one new buffer is allocated, reducing
to **O(n)**.

---

## New: Benchmark Program

A standalone benchmark program has been added at `examples/Benchmark/`.

**Coverage**: All four collection types — `TThreadSafeList`, `TThreadSafeDictionary`,
`TThreadSafeHashSet`, `TThreadSafeDeque`.

**Features**:
- Single-threaded and multi-threaded scenarios for each collection
- 5 runs per scenario; best and worst trimmed before averaging (trimmed mean)
- Four collection sizes: 1k, 10k, 100k, 1M items — shows scaling behaviour
- Microsecond-level timing via `QueryPerformanceCounter` (Windows)
- `--affinity` flag pins the timing thread to CPU core 0 via `SetThreadAffinityMask`,
  eliminating Windows scheduler noise at 1M-item scale while leaving worker threads free
- Results written to CSV for offline analysis

**Build note**: Use a Release build for meaningful numbers; `HeapTrc` skews timing.

---

## Test Results

| Metric | Result |
|--------|--------|
| **Total tests** | 116 |
| **Passed** | 116 |
| **Errors / Failures** | 0 / 0 |
| **Memory leaks** | 0 |

---

## Backward Compatibility

**100% backward compatible.** No public API changes. All existing code continues to work without
modification.

---

## Upgrading from v0.8.1

Replace the `src/` source files. No code changes are required.

If you use `TThreadSafeList<string>` (or any managed-type list) and previously observed crashes
or corrupted data under concurrent access, the managed-type memory-safety fix in this release
directly addresses that class of bug.

---

## Resources

- **Repository**: https://github.com/ikelaiah/threadsafecollections-fp
- **Full Changelog**: [CHANGELOG.md](../CHANGELOG.md)
- **Issues**: https://github.com/ikelaiah/threadsafecollections-fp/issues
