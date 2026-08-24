# RAII-Style Locking Through Interface Counting

[Documentation home](../index.md) · [Project README](../../README.md) ·
[Thread-safety model](../../README.md#thread-safety-model)

**Audience:** advanced users considering the public `Lock` method and
contributors reviewing lock-token and iterator lifetime behavior.

This project uses `ILockToken` and `TLockToken` to tie a collection lock to an interface reference lifetime.

Since v0.8.9 the collection lock is **re-entrant for the owning thread**: it is
a `TRecursiveCriticalSection` (a small wrapper around `TCriticalSection` that
tracks the owning thread). The same thread may therefore hold a manual
`Lock()` token **and** call public collection methods on the same instance;
other threads remain exclusively blocked for the whole sequence. This removes
the old POSIX deadlock trap and is pinned by regression tests.

`TLockToken.Create` acquires the lock. `TLockToken.Destroy` releases it if it is still held. `Release` can also release it explicitly before the interface reference is destroyed.

The implementation is in `src/ThreadSafeCollections.Interfaces.pas`:

```pascal
TRecursiveCriticalSection = class
private
  FSection: TCriticalSection;
  FOwnerThread: TThreadID;
  FDepth: Integer;
public
  constructor Create;
  destructor Destroy; override;
  procedure Acquire;   // re-entrant for the current thread
  procedure Release;   // released once the last nested acquire is dropped
end;

TLockToken = class(TInterfacedObject, ILockToken)
private
  FLock: TRecursiveCriticalSection;
public
  constructor Create(ALock: TRecursiveCriticalSection);
  destructor Destroy; override;
  procedure Release;
end;

constructor TLockToken.Create(ALock: TRecursiveCriticalSection);
begin
  inherited Create;
  FLock := ALock;
  FLock.Acquire;
end;

destructor TLockToken.Destroy;
begin
  if Assigned(FLock) then
    FLock.Release;
  inherited;
end;
```

## Current Iteration Models

Not every collection uses the token in the same way.

| Collection | Iteration model | Concurrent modification during `for..in` |
|---|---|---|
| `TThreadSafeList` | Enumerator holds `ILockToken` for the full loop | Blocked until enumeration ends |
| `TThreadSafeHashSet` | Enumerator holds `ILockToken` for the full loop | Blocked until enumeration ends |
| `TThreadSafeDeque` | Enumerator holds `ILockToken` for the full loop | Blocked until enumeration ends |
| `TThreadSafeDictionary` | Enumerator snapshots entries, then releases the lock | Allowed, but changes are not visible to that iterator |

## List, HashSet, and Deque Iterators

These enumerators acquire the collection lock in the enumerator constructor and release it in the enumerator destructor.

Current `TThreadSafeDeque` example:

```pascal
constructor TThreadSafeDeque.TEnumerator.Create(ADeque: TThreadSafeDeque);
begin
  inherited Create;
  FDeque := ADeque;
  FLockToken := FDeque.Lock;
  FCurrentIndex := -1;
end;

destructor TThreadSafeDeque.TEnumerator.Destroy;
begin
  FLockToken := nil;
  inherited;
end;
```

The deque is circular-array based. The old linked-list `FCurrentNode` field is no longer present.

Current `TThreadSafeList` example:

```pascal
constructor TThreadSafeList.TEnumerator.Create(AList: specialize TThreadSafeList<T>);
begin
  inherited Create;
  FList := AList;
  FLockToken := FList.Lock;
  FIndex := -1;
end;

destructor TThreadSafeList.TEnumerator.Destroy;
begin
  FLockToken := nil;
  inherited;
end;
```

While such an enumerator exists, other public methods that need the same collection lock must wait.

## Dictionary Snapshot Iteration

`TThreadSafeDictionary` does not hold the lock for the full loop. Its enumerator copies all entries into an array while locked, then releases the lock immediately:

```pascal
constructor TThreadSafeDictionary.TEnumerator.Create(ADictionary: TThreadSafeDictionary);
var
  LockToken: ILockToken;
begin
  inherited Create;
  FSnapshotIndex := -1;

  LockToken := ADictionary.Lock;
  try
    // Copy key-value pairs into FSnapshot.
  finally
    LockToken := nil;
  end;
end;
```

Consequences:

- `MoveNext` and `Current` read from the snapshot, not the live dictionary.
- Other threads may add, remove, clear, or resize the dictionary during iteration.
- Changes made after the snapshot are not visible to the active iterator.
- The iterator is safe against dangling entry pointers because it does not retain bucket-entry pointers.

## Manual Lock Usage

Each collection's `Lock()` returns an `ILockToken` that is **re-entrant for the
calling thread** (v0.8.9). Public methods may be called while the token is held,
and a token may be nested. Other threads are excluded for the whole sequence,
so a guarded check-then-update sequence is atomic with respect to other threads:

```pascal
Token := List.Lock;
try
  if not List.Contains(5) then
    List.Add(5);
finally
  Token := nil;
end;
```

Two practical rules:

- Release the token before it leaves scope with `Token := nil`, or let the
  interface reference fall out of scope. Reusing a local token variable in a
  loop requires the previous token to be released first (a new `Lock()` while
  the old token is still held just nests, so it will not deadlock, but it will
  keep the lock held for the whole loop unless you release).
- Do not hand a token to another thread. A token is owned by the thread that
  created it; releasing it from a different thread is undefined.

```pascal
for I := 1 to Iterations do
begin
  LockToken := List.Lock;
  try
    // Work under the lock, including other public List methods.
  finally
    LockToken := nil;
  end;
end;
```

The collections still use private unlocked helpers, such as `InternalAdd`,
`InternalRemove`, `InternalDelete`, and `InternalSetCapacity`, when a public
method already holds the lock. That keeps each method's acquire/release pairing
obvious and avoids pointless nested lock entry.

## What This Pattern Provides

- Exception-safe lock release when the token is released or destroyed.
- A clean way for enumerators to hold a lock for their lifetime.
- Consistent exclusive locking across the collections.
- A safe, re-entrant per-thread lock so that compound public-method sequences
  can be made atomic while a manual token is held (v0.8.9 lock policy).

It does not provide concurrent reads, lock-free behavior, or cross-thread
atomicity beyond what a single token owns. It is **not** the v1.1 atomic
workflow API (`GetOrAdd`, `AddOrUpdate`, scoped access); those remain a
post-1.0 milestone.
