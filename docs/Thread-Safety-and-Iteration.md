# Thread-safety, iteration, and lock policy

[Documentation home](README.md) · [Project README](../README.md) ·
[Build and verify](BUILDING.md)

**Audience:** developers who need the precise rules for concurrent access,
enumeration, and manual locking, and maintainers who own those contracts.

This document records the v0.8.8 policy decision on iterator behavior. It is
the reference for what the concurrency regression suites enforce.

## Synchronization model

Each collection instance owns one `TCriticalSection`. Every public operation
acquires it, performs its work, and releases it. There is no reader/writer
distinction and no lock-free path.

Thread safety is per-operation. A check followed by a separate update is not
automatically atomic:

```pascal
if List.Contains(X) then
  List.Remove(X);  // Not atomic: another thread can run between the calls.
```

Prefer a combined operation (`TryAdd`, `AddOrSetValue`, `TryPop*`, `AddRange`)
or coordinate multi-step sequences with external synchronization.

## Iterator policy — decided in v0.8.8

The two iterator models are intentional and remain different:

| Collection | Iterator model | Writer behavior during iteration |
|---|---|---|
| `TThreadSafeList<T>` | Lock-holding for the enumerator's lifetime | Writers block until the loop ends |
| `TThreadSafeDeque<T>` | Lock-holding for the enumerator's lifetime | Writers block until the loop ends |
| `TThreadSafeHashSet<T>` | Lock-holding for the enumerator's lifetime | Writers block until the loop ends |
| `TThreadSafeDictionary<TKey, TValue>` | Snapshot taken at enumerator creation | Writers proceed; the loop sees the snapshot |

The v0.8.8 milestone asked whether this difference should remain intentional
or converge to a common policy. The decision is to **keep the difference
intentional**. The trade-offs were measured by contract, not guessed:

- Lock-holding enumeration costs no extra memory, cannot observe a torn
  structure, and reflects the collection exactly for the loop's duration. Its
  cost is that slow loop bodies stall writers.
- Snapshot enumeration for the dictionary bounds memory to the pairs captured
  at enumerator creation and never stalls writers. Its cost is that later
  updates are invisible to the running loop, and a snapshot of a very large
  dictionary is a copy of that dictionary's pairs.
- Converging both families on one model would change documented behavior for
  existing users without a measured safety, compatibility, memory, or
  performance gain. The policy will only be revisited before 1.0 if a
  benchmark or migration case demonstrates that a change pays for itself.

Both models are covered by tests: `ThreadSafeCollections.ConcurrencyTests`
asserts that list, deque, and hash-set mutations block while a lock-holding
enumerator is active and that dictionary mutations proceed during snapshot
iteration, and `ThreadSafeCollections.DeadlockTests` keeps both behaviors
bounded.

### Rules for enumeration

- Keep loop bodies short. A long body under a lock-holding enumerator stalls
  every other thread on that collection.
- Do not start new threads from inside a lock-holding loop and wait for them
  inside the loop; that can deadlock when the new thread needs the collection.
- Do not call public methods of the same collection from inside a
  lock-holding loop. On POSIX `TCriticalSection` is not re-entrant, so the
  nested call deadlocks; on Windows re-entry happens to work and must not be
  relied on.
- Do not hand an enumerator to another thread. `for..in` manages the
  enumerator on the calling thread.
- The collection must outlive every enumerator; finish all loops before
  freeing the collection.

## Manual lock tokens

`Lock` returns an `ILockToken` that releases the collection lock when the
token leaves scope or is set to `nil`. Valid uses are short, self-contained
regions that only need the lock held:

```pascal
var
  Token: ILockToken;
begin
  Token := List.Lock;
  try
    // Any public call here is unsafe on POSIX and is not portable.
  finally
    Token := nil;
  end;
end.
```

Holding a token and then calling a public method of the same collection is
**not supported**: Free Pascal's `TCriticalSection` is not re-entrant on
POSIX platforms, so the nested lock attempt deadlocks. On Windows the same
sequence happens to complete, and the regression suite records that platform
difference rather than endorsing it. Redesigning or deprecating `Lock` is
scheduled for v0.8.9.

## Lifetime rules

- A collection must outlive every thread, enumerator, and lock token that
  uses it.
- Finish and join worker threads before freeing the collection they used.
  Destroying a collection while a worker still uses it is invalid.
- Class instances stored as elements are not owned by the collection and are
  not freed by it.
- Interface-backed collections are reference-counted; release the interface
  instead of calling `Free` once the interface owns the instance.

## How the guarantees are exercised

| Concern | Where it is tested |
|---|---|
| Concurrent add/remove/lookup per collection | `ThreadSafeCollections.ConcurrencyTests` |
| Resize and bulk operations under concurrency | `ThreadSafeCollections.ConcurrencyTests` |
| Collision and poor-hash lookups | `ThreadSafeCollections.ConcurrencyTests`, `ThreadSafeCollections.StressTests` |
| Lock-holding versus snapshot iteration | `ThreadSafeCollections.ConcurrencyTests` |
| Opposite lock order and self-source bulk ops | `ThreadSafeCollections.DeadlockTests` |
| Bounded completion for enumeration and locks | `ThreadSafeCollections.DeadlockTests` |
| Manual lock serialization and Windows re-entry | `ThreadSafeCollections.DeadlockTests` |
| Destruction after concurrent work | `ThreadSafeCollections.DeadlockTests` |
| Seeded randomized stress (seed recorded per run) | `ThreadSafeCollections.StressTests` |

All suites run in CI on Windows and Linux with HeapTrc leak verification; see
[Building and verification](BUILDING.md#running-the-tests).
