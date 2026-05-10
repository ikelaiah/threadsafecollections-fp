# RAII-Style Locking Through Interface Counting

This project uses `ILockToken` and `TLockToken` to tie a `TCriticalSection` lock to an interface reference lifetime.

`TLockToken.Create` acquires the critical section. `TLockToken.Destroy` releases it if it is still held. `Release` can also release it explicitly before the interface reference is destroyed.

The implementation is in `src/ThreadSafeCollections.Interfaces.pas`:

```pascal
TLockToken = class(TInterfacedObject, ILockToken)
private
  FLock: TCriticalSection;
public
  constructor Create(ALock: TCriticalSection);
  destructor Destroy; override;
  procedure Release;
end;

constructor TLockToken.Create(ALock: TCriticalSection);
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

Use manual `Lock()` with care. Free Pascal's `TCriticalSection` is not re-entrant on POSIX platforms, and the public collection methods already acquire the same lock internally.

Do not acquire a token and then call public methods on the same collection while that token is held:

```pascal
Token := List.Lock;
try
  // Avoid this pattern: Add also tries to acquire List's lock.
  List.Add(42);
finally
  Token := nil;
end;
```

If a local token variable is reused in a loop, release it before the next iteration. Creating a new token while the old token still holds the same lock can deadlock on non-reentrant implementations.

```pascal
for I := 1 to Iterations do
begin
  LockToken := List.Lock;
  try
    // Work that does not call List's locking public methods.
  finally
    LockToken := nil;
  end;
end;
```

The collections themselves use private unlocked helpers, such as `InternalAdd`, `InternalRemove`, `InternalDelete`, and `InternalSetCapacity`, when a public method already holds the lock. That is how the implementation avoids re-acquiring the same critical section internally.

## What This Pattern Provides

- Exception-safe lock release when the token is released or destroyed.
- A clean way for enumerators to hold a lock for their lifetime.
- Consistent exclusive locking across the collections.

It does not provide concurrent reads, lock-free behavior, or safe nested calls into the same collection's public API while a manual token is held.
