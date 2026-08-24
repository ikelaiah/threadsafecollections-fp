# Contracts & Limitations

This page collects the behavioural contracts that matter when you rely on a
thread-safe collection, and the limitations that are intentionally out of scope.
The authoritative discussion lives in
[Thread-Safety and Iteration](../guides/thread-safety-and-iteration.md).

## Thread-safety contract

Each collection instance owns exactly one `TCriticalSection`.

- **Method-level atomicity.** A single public method call is synchronized.
  A check followed by a separate update (`if Contains(X) then Add(X)`) is not
  atomic. Prefer combined operations such as `TryAdd`, `AddOrSetValue`, or
  `TryPop*`, or guard the sequence yourself with a
  [Lock token](../guides/lock-tokens.md).
- **Iteration is either lock-holding or snapshot.** List, deque, and hash-set
  `for..in` enumerators hold the collection lock for the enumerator's lifetime;
  dictionary iteration snapshots the pairs first and then releases the lock.
- **Lifetime.** A collection must outlive every thread, enumerator, and lock
  token that uses it.
- **Structure, not contents.** Synchronization protects collection structure.
  Mutable objects referenced by stored pointers, classes, or interfaces are not
  protected, and class instances stored as elements are never freed by the
  collection.

The implementation uses mutual exclusion; it is not lock-free and does not use
reader/writer locks.

## Manual locking policy

`Lock()` returns an `ILockToken`. The underlying lock is **re-entrant for the
owning thread**, so:

- calling public methods while holding a token is safe and does not deadlock on
  any platform;
- nested `Lock()` calls from the same thread nest rather than deadlock;
- other threads are excluded until the last token is released, which makes a
  guarded multi-call sequence atomic for the owning thread;
- a token must not be passed to another thread (it is not cross-thread
  re-entrant).

See [Lock Tokens (RAII)](../guides/lock-tokens.md) and the
[thread-safety guide](../guides/thread-safety-and-iteration.md).

## Exception and edge contracts

Errors follow FPC's standard exception conventions, raised only on the owning
call while the collection lock is held:

- Invalid indices/ranges, capacities below `Count`, and `ResizeBuckets` below
  the current load raise `EArgumentOutOfRangeException`.
- Empty list `First`/`Last` and empty deque `Pop*`/`Peek*` (non-`Try` variants)
  raise `EListError`.
- Dictionary reads of a missing key raise `EKeyNotFoundException`; adding a
  duplicate key raises `EArgumentException`.
- `Try*` variants (`TryPop*`, `TryPeek*`, `TryGetValue`, `TryAdd`) return
  `False` and never raise for the empty/missing cases they exist for.
- Reading `Current` before the first `MoveNext` raises `EInvalidOperation` on
  every enumerator.
- Null callbacks are rejected at construction for the HashSet (equality
  comparer and hash function). Note that on FPC 3.2.2 a `nil` literal or
  variable passed directly to those generic function-typed constructor
  parameters can raise an access violation in the compiler-generated call
  before validation; callers should pass real callbacks.
- Null source collections/arrays in bulk operations are no-ops; self-source
  bulk operations copy through a snapshot first and never deadlock.
- Bulk operations are best-effort, not transactional: if a user callback
  (hash/equality) raises part-way, the items already applied remain and the
  collection stays usable.

## Complexity

The collection guides annotate every public operation:

- List indexing, first/last, replace: O(1). Insert/delete/shift operations:
  O(n). Sort: O(n log n) average. Searching: O(n) unsorted, O(log n) after
  `Sort`.
- Deque push/pop/peek at both ends: O(1) amortized. Range operations: O(m).
- Dictionary and hash set single-item operations: O(1) average, O(n) during a
  resize. Dictionary key/value snapshots: O(n). Hash-set set algebra:
  O(n + m).

Ranges, growth, and `TrimExcess` behave like the underlying dynamic arrays or
bucket arrays of the standard library collections.

## Supported and verified environments

| Environment | What is verified |
|---|---|
| Windows x86-64, FPC 3.2.2 | Current 175-test FPCUnit run with HeapTrc, documented examples, and both all-example scripts |
| Windows x86-64, Lazarus 4.8 | Command-line build of the Lazarus package and the package smoke consumer |
| Windows CI (`windows-latest`), FPC bundled with Lazarus 4.0.0 | All tracked examples compile and the full FPCUnit suite, including the concurrency, deadlock, and stress suites, runs through the PowerShell test script |
| Linux CI (`ubuntu-latest`) | All tracked examples compile with the distribution FPC package, the full FPCUnit suite runs, the Lazarus package builds with `lazbuild`, and the documentation checks pass |
| macOS and other FPC targets | Out of scope for now; the maintained focus is Windows x86-64 and Linux |

The source is written for FPC's `objfpc` mode. A platform being supported by
FPC does not by itself mean this repository has tested that platform.

## Known limitations

- No atomic compound operations above the method level; combine operations
  yourself or hold a lock token (see the [lock policy](#manual-locking-policy)).
- Iteration boundary differences between collections are intentional and
  tested; do not assume a dictionary snapshot reflects later updates.
- `Count` is O(1) everywhere; `ContainsValue` on a dictionary is O(n).
- A dictionary's `Add` raises on an existing key; use `TryAdd` or
  `AddOrSetValue` for upsert workflows.
- Hash-set equality/hash functions must be consistent; violations degrade
  correctness.
- Dictionary `First`/`Last`, `BucketCount`, and `ResizeBuckets` are concrete-only
  API (not on `IThreadSafeDictionary`). They are deliberately kept as
  diagnostics/advanced tuning: `First`/`Last` return an implementation-dependent
  pair, and the bucket members expose the internal hash table. They are not a
  v1.0 interface promise.
- The library is learning-focused. Evaluate behaviour, performance, and test
  coverage against your application's requirements before production use.

## What is not guaranteed

- Lock-free or wait-free progress guarantees.
- Cross-thread re-entrancy of a manual lock token.
- Automatic memory management of stored object elements.
- Automatic lifecycle management: the collection must outlive its users.
- Platform coverage beyond the environments listed above.