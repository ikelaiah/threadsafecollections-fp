# ThreadSafeCollections.HashSet Documentation

`TThreadSafeHashSet<T>` is a generic hash set protected by one `TCriticalSection` per set instance.

The current implementation lives in `src/ThreadSafeCollections.HashSet.pas`.

## Dependencies

- Free Pascal 3.2.2 or later
- `SyncObjs`
- `HashFunctions`
- `TypInfo`
- `ThreadSafeCollections.Interfaces`
- `ThreadSafeCollections.ErrorMessages`

## Public Types

```pascal
generic TEqualityComparer<T> = function(const A, B: T): Boolean;
generic THashFunction<T> = function(const Value: T): Cardinal;
```

Specialized classes:

```pascal
TThreadSafeHashSetInteger;
TThreadSafeHashSetString;
TThreadSafeHashSetBoolean;
TThreadSafeHashSetReal;
```

Primitive comparer/hash helpers:

```pascal
function IntegerEquals(const A, B: Integer): Boolean;
function StringEquals(const A, B: string): Boolean;
function BooleanEquals(const A, B: Boolean): Boolean;
function RealEquals(const A, B: Real): Boolean;

function IntegerHash(const Value: Integer): Cardinal;
function StringHash(const Value: string): Cardinal;
function BooleanHash(const Value: Boolean): Cardinal;
function RealHash(const Value: Real): Cardinal;
```

## Storage Model

The set uses separate chaining:

```pascal
FBuckets: array of PEntry;

TEntry = record
  Value: T;
  Hash: Cardinal;
  Next: PEntry;
end;
```

Entries are allocated by a slab allocator:

```pascal
ENTRY_BLOCK_SIZE = 256;
```

Freed entries are recycled through a freelist. The allocator has no lock of its own; callers must hold the set lock.

## Construction

Generic constructor:

```pascal
constructor Create(
  AEqualityComparer: specialize TEqualityComparer<T>;
  AHashFunction: specialize THashFunction<T>;
  AInitialCapacity: Integer = INITIAL_BUCKET_COUNT);
```

Specialized constructors:

```pascal
constructor TThreadSafeHashSetInteger.Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT);
constructor TThreadSafeHashSetString.Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT);
constructor TThreadSafeHashSetString.Create(
  AHashFunction: specialize THashFunction<string>;
  AInitialCapacity: Integer = INITIAL_BUCKET_COUNT);
constructor TThreadSafeHashSetBoolean.Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT);
constructor TThreadSafeHashSetReal.Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT);
```

Use the specialized classes for common primitive types. Use the generic class with explicit equality and hash functions for custom types.

```pascal
type
  TPoint = record
    X: Integer;
    Y: Integer;
  end;

function PointEquals(const A, B: TPoint): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

function PointHash(const Value: TPoint): Cardinal;
begin
  Result := MultiplicativeHash(Cardinal(Value.X xor Value.Y));
end;

var
  Points: specialize TThreadSafeHashSet<TPoint>;
  P: TPoint;
begin
  Points := specialize TThreadSafeHashSet<TPoint>.Create(@PointEquals, @PointHash);
  try
    P.X := 1;
    P.Y := 1;
    Points.Add(P);
  finally
    Points.Free;
  end;
end;
```

## Public API

Core operations:

```pascal
function Add(const Item: T): Boolean;
function Remove(const Item: T): Boolean;
function Contains(const Item: T): Boolean;
procedure Clear;
property Count: Integer read GetCount;
function IsEmpty: Boolean;
```

Array and bulk operations:

```pascal
function ToArray: specialize TArray<T>;
procedure AddRange(const Items: array of T);
procedure AddRange(const Collection: specialize IThreadSafeHashSet<T>);
function RemoveRange(const Items: array of T): Integer;
function RemoveRange(const Collection: specialize IThreadSafeHashSet<T>): Integer;
```

Set operations:

```pascal
function TryGetValue(const Item: T; out Value: T): Boolean;
procedure IntersectWith(const Collection: specialize IThreadSafeHashSet<T>);
procedure UnionWith(const Collection: specialize IThreadSafeHashSet<T>);
procedure ExceptWith(const Collection: specialize IThreadSafeHashSet<T>);
function Overlaps(const Collection: specialize IThreadSafeHashSet<T>): Boolean;
function SetEquals(const Collection: specialize IThreadSafeHashSet<T>): Boolean;
```

Iteration and locking:

```pascal
function GetEnumerator: TEnumerator;
function Lock: ILockToken;
```

## Thread Safety

Most public operations acquire `FLock` directly and release it in `finally`.

Private helpers such as `InternalAdd` and `InternalRemove` assume the caller already holds `FLock`.

Manual `Lock()` is advanced usage. Do not hold a token and then call public methods on the same set, because those methods try to acquire the same critical section again.

`AddRange(Collection)` and `RemoveRange(Collection)` call `Collection.ToArray` directly. The source lock is held only while creating the snapshot and is released before this set is mutated, avoiding nested source-lock acquisition on non-reentrant POSIX critical sections.

## Iteration

The hash set enumerator holds an `ILockToken` for the full enumeration:

```pascal
constructor TThreadSafeHashSet.TEnumerator.Create(ASet: TThreadSafeHashSet);
begin
  inherited Create;
  FSet := ASet;
  FLockToken := FSet.Lock;
  FCurrentBucket := -1;
  FCurrentEntry := nil;
end;
```

During a `for..in` loop, other public operations on the same set wait until enumeration ends.

## Set Operations and Snapshots

`IntersectWith` snapshots the other collection with `ToArray` before acquiring this set's lock. That avoids the ABBA lock-order deadlock that would happen if two threads called `A.IntersectWith(B)` and `B.IntersectWith(A)`.

`AddRange`, `RemoveRange`, `ExceptWith`, `Overlaps`, and `SetEquals` also use `ToArray` snapshots of the other collection.

## Hashing

Specialized hash functions:

- `Integer`: multiplicative hash
- `string`: `XXHash32`
- `Boolean`: direct value hash
- `Real`: fixed-point conversion hash

The generic `TThreadSafeHashSet<T>` does not auto-select a hash function for arbitrary `T`; callers provide one.

## Load Factor and Resizing

Constants:

```pascal
INITIAL_BUCKET_COUNT = 16;
LOAD_FACTOR = 0.75;
MIN_BUCKET_COUNT = 4;
ENTRY_BLOCK_SIZE = 256;
```

`CheckLoadFactor` resizes when `FCount / Length(FBuckets)` exceeds `LOAD_FACTOR`.

`AddRange(const Items: array of T)` pre-calculates the required bucket count from the current count plus input length, then resizes once if needed.

The set does not shrink automatically after removals.

## Complexity

| Operation | Complexity |
|---|---|
| `Add`, `Remove`, `Contains`, `TryGetValue` | O(1) average, O(n) worst under collisions |
| `Clear` | O(n) |
| `ToArray`, iteration | O(n) |
| `AddRange(array)` | O(m) average, plus resize costs |
| `RemoveRange(array)` | O(m) average |
| `IntersectWith` | O(n * m) in the current snapshot scan implementation |
| `UnionWith` | Delegates to `AddRange(Collection)` |
| `ExceptWith` | O(m) average after source snapshot |
| `Overlaps` | O(m) average after source snapshot |
| `SetEquals` | O(m) average after source snapshot |

## Usage Examples

```pascal
var
  Set: TThreadSafeHashSetInteger;
begin
  Set := TThreadSafeHashSetInteger.Create;
  try
    Set.Add(42);
    Set.Add(17);

    if Set.Contains(42) then
      WriteLn('Found 42');

    Set.Remove(17);
  finally
    Set.Free;
  end;
end;
```

```pascal
var
  SetA, SetB: TThreadSafeHashSetInteger;
begin
  SetA := TThreadSafeHashSetInteger.Create;
  SetB := TThreadSafeHashSetInteger.Create;
  try
    SetA.AddRange([1, 2, 3]);
    SetB.AddRange([2, 3, 4]);

    SetA.IntersectWith(SetB); // SetA contains 2 and 3.
  finally
    SetA.Free;
    SetB.Free;
  end;
end;
```

## Notes and Limitations

- One exclusive lock protects the whole set; there are no reader/writer locks.
- Iteration blocks other public operations until the enumerator is destroyed.
- Ordering is bucket/chaining order and is not stable API.
- Heavy collisions degrade lookup operations to linear chain scans.
- There is no built-in serialization support.
- There is no `DEBUG_LOGGING` constant or runtime debug logging switch in this unit.
