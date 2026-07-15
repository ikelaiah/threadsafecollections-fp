# ThreadSafeCollections.Dictionary Documentation

`TThreadSafeDictionary<TKey, TValue>` is a generic hash table protected by one `TCriticalSection` per dictionary instance.

The current implementation lives in `src/ThreadSafeCollections.Dictionary.pas`.

## Dependencies

- Free Pascal 3.2.2 or later
- `Generics.Collections` for `TPair`
- `SyncObjs`
- `HashFunctions`
- `ThreadSafeCollections.Interfaces`
- `ThreadSafeCollections.ErrorMessages`

## Storage Model

The dictionary uses separate chaining:

```pascal
FBuckets: array of PEntry;

TEntry = record
  Key: TKey;
  Value: TValue;
  Hash: Cardinal;
  Next: PEntry;
end;
```

Entries are allocated by a slab allocator:

```pascal
ENTRY_BLOCK_SIZE = 256;
```

Freed entries are recycled through a freelist. The allocator has no lock of its own; callers must hold the dictionary lock.

## Construction

```pascal
constructor Create;
constructor Create(InitialCapacity: integer);
constructor Create(
  const AHashFunc: specialize THashFunction<TKey>;
  const AEqualityComparer: specialize TEqualityComparison<TKey>);
constructor Create(
  InitialCapacity: integer;
  const AHashFunc: specialize THashFunction<TKey>;
  const AEqualityComparer: specialize TEqualityComparison<TKey>);
```

Default bucket count is 16. Capacity is rounded to a power of two and is at least 4.

For `string` and `integer` keys, the default constructor selects a built-in hash path. Default key equality uses `Generics.Defaults.TEqualityComparer<TKey>.Default`, so managed keys such as separately allocated equal strings compare semantically. For custom key types, provide both a hash function and an equality comparer when the RTL default is not the desired contract.

```pascal
type
  TPersonKey = record
    FirstName: string;
    LastName: string;
  end;

function HashPerson(const Key: TPersonKey): Cardinal;
begin
  Result := XXHash32(Key.FirstName + '|' + Key.LastName);
end;

function ComparePerson(const Left, Right: TPersonKey): Boolean;
begin
  Result := (Left.FirstName = Right.FirstName) and
            (Left.LastName = Right.LastName);
end;

var
  Dict: specialize TThreadSafeDictionary<TPersonKey, Integer>;
begin
  Dict := specialize TThreadSafeDictionary<TPersonKey, Integer>.Create(@HashPerson, @ComparePerson);
  try
    // Use dictionary.
  finally
    Dict.Free;
  end;
end;
```

## Public API

Core operations:

```pascal
procedure Add(const Key: TKey; const Value: TValue);
function Remove(const Key: TKey): Boolean;
function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
procedure AddOrSetValue(const Key: TKey; const Value: TValue);
function ContainsKey(const Key: TKey): Boolean;
function GetItem(const Key: TKey): TValue;
procedure SetItem(const Key: TKey; const Value: TValue);
procedure Clear;
function Count: integer;
function GetCount: Integer;
property Items[const Key: TKey]: TValue read GetItem write AddOrSetValue; default;
```

Navigation and maintenance:

```pascal
function First(out Key: TKey; out Value: TValue): Boolean;
function Last(out Key: TKey; out Value: TValue): Boolean;
function GetBucketCount: integer;
procedure ResizeBuckets(NewSize: integer);
property BucketCount: integer read GetBucketCount;
procedure TrimExcess;
```

Bulk and query operations:

```pascal
function GetKeys: specialize TKeyArray<TKey>;
function GetValues: specialize TValueArray<TValue>;
function TryAdd(const Key: TKey; const Value: TValue): Boolean;
procedure AddRange(const ADictionary: specialize IThreadSafeDictionary<TKey, TValue>);
procedure AddRange(const AArray: specialize TPairArray<TKey, TValue>);
function ToArray: specialize TPairArray<TKey, TValue>;
function ContainsValue(const Value: TValue): Boolean;
function GetEnumerator: TEnumerator;
function Lock: ILockToken;
```

## Hashing

The current key-dispatch cache is:

```pascal
TKeyKind = (kkString, kkInteger, kkOther);
```

- `string`: `XXHash32`
- `integer`: `MultiplicativeHash`
- other key types: custom hash if provided, otherwise `DefaultHash`

`XXHash32` uses a 4-lane path for strings of at least 16 bytes and a single-lane path for shorter strings.

## Thread Safety

Most public operations acquire `FLock` directly and release it in `finally`.

`Lock()` returns an `ILockToken`, but manual use is advanced. Do not hold a token and then call public methods on the same dictionary, because those methods try to acquire the same critical section again.

`AddRange(ADictionary)` calls `ADictionary.ToArray` to create a source snapshot, releases the source lock, and then applies that snapshot to the destination. It does not hold a source lock while calling other public source methods, so the collection overload is safe with non-reentrant `TCriticalSection` implementations on POSIX.

## Iteration

Dictionary iteration is snapshot-based.

The enumerator copies all key-value pairs into `FSnapshot` while holding the lock, then releases the lock before iteration begins:

```pascal
type
  TSnapshot = array of specialize TPair<TKey, TValue>;

constructor TThreadSafeDictionary.TEnumerator.Create(ADictionary: TThreadSafeDictionary);
var
  LockToken: ILockToken;
begin
  inherited Create;
  FSnapshotIndex := -1;

  LockToken := ADictionary.Lock;
  try
    // Copy entries into FSnapshot.
  finally
    LockToken := nil;
  end;
end;
```

Consequences:

- Other threads may modify the dictionary during iteration.
- The iterator does not see changes made after the snapshot.
- The iterator is safe if the dictionary resizes or clears after the snapshot.
- Multiple dictionary iterators can proceed independently after their snapshots are built.

## Load Factor and Resizing

Constants:

```pascal
INITIAL_BUCKET_COUNT = 16;
LOAD_FACTOR = 0.75;
MIN_BUCKET_COUNT = 4;
ENTRY_BLOCK_SIZE = 256;
```

`CheckLoadFactor` doubles the bucket array when `FCount / Length(FBuckets)` exceeds `LOAD_FACTOR`.

`ResizeBuckets(NewSize)` is public. It validates that the requested size can hold the current count at the configured load factor, rounds to the next power of two, then resizes.

`TrimExcess` resizes down to a power-of-two bucket count based on the current item count and load factor, but never below the minimum.

## ContainsValue

`ContainsValue` scans every entry and delegates to `FindValue`. Values are compared with `Generics.Defaults.TEqualityComparer<TValue>.Default`, which provides type-aware equality for managed values such as strings as well as scalar values.

## Complexity

| Operation | Complexity |
|---|---|
| `Add`, `TryAdd`, `TryGetValue`, `ContainsKey`, `Remove`, `GetItem`, `SetItem` | O(1) average, O(n) worst under collisions |
| `First`, `Last` | O(bucket count) worst case |
| `Clear` | O(n) |
| `Resize`, `ResizeBuckets`, `TrimExcess` | O(n) |
| `GetKeys`, `GetValues`, `ToArray`, snapshot enumerator construction | O(n) |
| `ContainsValue` | O(n) |
| `AddRange(AArray)` | O(m) average, plus resize costs |
| `AddRange(ADictionary)` | O(m) average after taking a source snapshot |

## Usage Examples

Basic usage:

```pascal
var
  Dict: specialize TThreadSafeDictionary<string, Integer>;
  Value: Integer;
begin
  Dict := specialize TThreadSafeDictionary<string, Integer>.Create;
  try
    Dict.Add('one', 1);
    Dict.AddOrSetValue('two', 2);

    if Dict.TryGetValue('one', Value) then
      WriteLn(Value);

    if Dict.ContainsKey('two') then
      Dict.Remove('two');
  finally
    Dict.Free;
  end;
end;
```

Array bulk add:

```pascal
var
  Dict: specialize TThreadSafeDictionary<string, Integer>;
  Pairs: array[0..1] of specialize TPair<string, Integer>;
begin
  Dict := specialize TThreadSafeDictionary<string, Integer>.Create;
  try
    Pairs[0].Key := 'one';
    Pairs[0].Value := 1;
    Pairs[1].Key := 'two';
    Pairs[1].Value := 2;

    Dict.AddRange(Pairs);
  finally
    Dict.Free;
  end;
end;
```

Snapshot iteration:

```pascal
var
  Pair: specialize TPair<string, Integer>;
begin
  for Pair in Dict do
    WriteLn(Pair.Key, ': ', Pair.Value);
end;
```

## Notes and Limitations

- One exclusive lock protects the dictionary; there are no reader/writer locks.
- Iteration is snapshot-based, not live.
- Entry order is implementation-dependent and follows bucket/chaining layout.
- `First` and `Last` are not insertion-order operations.
- `ContainsValue` is a full scan using the RTL's type-aware default equality comparer.
- There is no `DEBUG_LOGGING` constant or runtime debug logging switch in this unit.
