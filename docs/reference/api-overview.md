# API Overview

The library ships four concrete thread-safe collections plus shared interface
forms. Every concrete collection owns one `TCriticalSection` and each public
method is synchronized on it. See [Thread-Safety and Iteration](../guides/thread-safety-and-iteration.md)
for the exact guarantees.

## Units

| Unit | Contents |
|---|---|
| `ThreadSafeCollections.List` | `TThreadSafeList<T>`, `IThreadSafeList<T>`, built-in comparers |
| `ThreadSafeCollections.Deque` | `TThreadSafeDeque<T>`, `IThreadSafeDeque<T>` |
| `ThreadSafeCollections.Dictionary` | `TThreadSafeDictionary<TKey, TValue>`, `IThreadSafeDictionary<TKey, TValue>` |
| `ThreadSafeCollections.HashSet` | `TThreadSafeHashSet<T>` and specialized forms, `IThreadSafeHashSet<T>`, helper types |
| `ThreadSafeCollections.Interfaces` | `ILockToken` and the shared `IThreadSafeCollection<T>` interface |
| `ThreadSafeCollections.ErrorMessages` | shared error-message constants |
| `HashFunctions` | hash primitives: `XXHash32`, `FNV1aHash`, `MultiplicativeHash`, `DefaultHash` |

The generated [Cheat Sheet](../start/cheat-sheet.md) lists every public
declaration with its complexity annotation.

## Public generic types

| Type | Element | Requires | Construct |
|---|---|---|---|
| `specialize TThreadSafeList<T>` | Elements | a comparer `specialize TComparer<T>` | `TThreadSafeList<Integer>.Create(@IntegerComparer)` |
| `specialize TThreadSafeDeque<T>` | Elements | nothing | `TThreadSafeDeque<Integer>.Create` |
| `specialize TThreadSafeDictionary<TKey, TValue>` | Key/value pairs | nothing by default; hash/equality functions for custom keys | `TThreadSafeDictionary<string, integer>.Create` |
| `specialize TThreadSafeHashSet<T>` | Unique elements | equality and hash functions | `TThreadSafeHashSetInteger.Create` |

Generic classes implement matching interface forms, so a concrete instance can
be assigned to `IThreadSafeList<T>`, `IThreadSafeDeque<T>`, and so on. Interface
values are reference-counted by Free Pascal and are freed automatically.

## Shared interface

`IThreadSafeCollection<T>` declares the operations shared by the list, deque,
and hash set:

- `Count` (read-only), `IsEmpty`, `Clear`, `Lock`.

`IThreadSafeDictionary<TKey, TValue>` declares the dictionary surface directly
(interface inheritance is not used there because of the two generic
parameters).

## Support types

- `ILockToken` — an interface whose reference lifetime bounds a held lock.
  See [Lock Tokens (RAII)](../guides/lock-tokens.md).
- Array views for dictionary snapshots (declared in
  `ThreadSafeCollections.Interfaces`):
  - `TKeyArray<T>`, `TValueArray<T>`, `TPairArray<TKey, TValue>`.
- Hash/equality callbacks:
  - Dictionary: `THashFunction<T>`, `TEqualityComparison<T>`.
  - Hash set: `THashSetEqualityComparer<T>`, `THashFunction<T>`.
  - List: `specialize TComparer<T>` from `Generics.Defaults`.

## Built-in helpers

`ThreadSafeCollections.List` provides comparers for common element types:

```pascal
function IntegerComparer(const A, B: Integer): Integer;
function StringComparer(const A, B: string): Integer;
function BooleanComparer(const A, B: Boolean): Integer;
function RealComparer(const A, B: Real): Integer;
```

`ThreadSafeCollections.HashSet` provides hash and equality helpers for its
specialized classes (`IntegerHash`, `StringHash`, `RealHash`, and matching
equality functions). `HashFunctions` exposes the underlying hash primitives.

## Configuration decisions

Choices that affect behaviour (capacity, hash selection, equality) are
described in [Configuration](config.md). The exact behavioural contracts and
limitations are in [Contracts & Limitations](contracts-and-limitations.md), and
the differences between collections are summarized in the
[Feature Matrix](feature-matrix.md).