# ThreadSafeCollections.List Documentation

`TThreadSafeList<T>` is a generic dynamic-array list protected by one `TCriticalSection` per list instance.

The current implementation lives in `src/ThreadSafeCollections.List.pas`.

## Dependencies

- Free Pascal 3.2.2 or later
- `SyncObjs`
- `ThreadSafeCollections.Interfaces`
- `ThreadSafeCollections.ErrorMessages`

## Public Types

```pascal
generic TComparer<T> = function(const A, B: T): Integer;
generic TItemArray<T> = array of T;
```

Built-in comparers:

```pascal
function IntegerComparer(const A, B: Integer): Integer;
function StringComparer(const A, B: string): Integer;
function BooleanComparer(const A, B: Boolean): Integer;
function RealComparer(const A, B: Real): Integer;
```

## Construction

```pascal
constructor Create(AComparer: specialize TComparer<T>);
constructor Create(AComparer: specialize TComparer<T>; AInitialCapacity: Integer);
```

The comparer is required. Passing `nil` raises `ERR_COMPARER_REQUIRED`.

Default initial capacity is 16. Explicit initial capacity is rounded up to at least 4.

```pascal
var
  List: specialize TThreadSafeList<Integer>;
begin
  List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer, 1000);
  try
    List.Add(42);
  finally
    List.Free;
  end;
end;
```

## Public API

Core operations:

```pascal
function Add(const Item: T): Integer;
procedure Delete(Index: Integer);
function IndexOf(const Item: T): Integer;
function First: T;
function Last: T;
procedure Sort(Ascending: Boolean = True);
function IsSorted: Boolean;
procedure Replace(Index: Integer; const Item: T);
procedure Clear;
function IsEmpty: Boolean;
```

Indexed access:

```pascal
property Items[Index: Integer]: T read GetItem write SetItem; default;
property Count: Integer read GetCount;
property Capacity: Integer read GetCapacity write SetCapacity;
```

Array and range operations:

```pascal
function ToArray: specialize TArray<T>;
procedure FromArray(const Values: array of T);
procedure AddRange(const Values: array of T);
procedure AddRange(const Collection: specialize IThreadSafeList<T>);
procedure InsertRange(Index: Integer; const Values: array of T);
procedure InsertRange(Index: Integer; const Collection: specialize IThreadSafeList<T>);
procedure DeleteRange(AIndex, ACount: Integer);
```

Search and utility operations:

```pascal
function Contains(const Value: T): Boolean;
function IndexOfItem(const Item: T; StartIndex: Integer): Integer;
function IndexOfItem(const Item: T; StartIndex, ACount: Integer): Integer;
function LastIndexOf(const Item: T): Integer;
function LastIndexOf(const Item: T; StartIndex: Integer): Integer;
function LastIndexOf(const Item: T; StartIndex, ACount: Integer): Integer;
procedure Insert(Index: Integer; const Item: T);
procedure Exchange(Index1, Index2: Integer);
procedure MoveItem(CurIndex, NewIndex: Integer);
procedure Reverse;
function Extract(const Item: T): T;
function ExtractAt(Index: Integer): T;
procedure TrimExcess;
function Lock: ILockToken;
function GetEnumerator: TEnumerator;
```

## Thread Safety

Every public operation acquires the list's `TCriticalSection` and releases it in a `finally` block.

Several private helpers operate without locking and must only be called while the lock is already held:

- `InternalSetCapacity`
- `InternalDelete`
- `InternalIndexOf`
- `InternalBinarySearch`

These helpers avoid re-acquiring the same lock from an already locked public method.

Manual `Lock()` is advanced usage. Do not hold a token and then call public methods on the same list, because those methods try to acquire the same critical section again.

## Iteration

The enumerator holds an `ILockToken` for its entire lifetime:

```pascal
constructor TThreadSafeList.TEnumerator.Create(AList: specialize TThreadSafeList<T>);
begin
  inherited Create;
  FList := AList;
  FLockToken := FList.Lock;
  FIndex := -1;
end;
```

During a `for..in` loop, other threads attempting to mutate or read through public list methods must wait until enumeration ends.

```pascal
var
  Item: Integer;
begin
  for Item in List do
    WriteLn(Item);
end;
```

## Sorting and Searching

`Sort(True)` sorts ascending. `Sort(False)` sorts descending. `IsSorted` returns the current `FSorted` flag, while the private `FSortAscending` field records the active direction.

Current binary-search behavior:

- `IndexOf` calls `InternalIndexOf`.
- `Contains` calls `IndexOf`.
- `InternalIndexOf` uses `InternalBinarySearch` when `FSorted = True`.
- `IndexOfItem` overloads remain linear scans.
- `LastIndexOf` overloads remain linear scans.

`InternalBinarySearch` uses `FSortAscending` to select the correct comparison direction. It returns the first matching index for duplicate values in either direction. Appending or replacing an item preserves `FSorted` only when the new value respects the active sort direction.

## Capacity and Memory

Growth constants in the current code:

```pascal
DEFAULT_INITIAL_CAPACITY = 16;
MIN_CAPACITY = 4;
SMALL_LIST_THRESHOLD = 64;
GROWTH_FACTOR_DOUBLE = 2;
GROWTH_FACTOR_LARGE_NUMERATOR = 3;
GROWTH_FACTOR_LARGE_DENOMINATOR = 2;
```

Growth strategy:

- Capacity starts at 16 by default.
- Small lists double until the 64-item threshold.
- Larger lists grow by roughly 1.5x.
- `TrimExcess` shrinks capacity to `Count`.
- Assignments and shifts are element-wise so managed types such as `string` and interfaces keep correct reference counts.

## Complexity

| Operation | Complexity |
|---|---|
| `Add` | O(1) amortized, O(n) when growing |
| `Delete`, `Insert`, `DeleteRange`, `InsertRange` | O(n) due to shifting |
| `First`, `Last`, indexed get/set, `Count`, `IsEmpty` | O(1) |
| `Sort` | O(n log n) average |
| `IndexOf`, `Contains` unsorted | O(n) |
| `IndexOf`, `Contains` after `Sort(True)` or `Sort(False)` | O(log n) |
| `IndexOfItem`, `LastIndexOf` | O(n) |
| `ToArray`, `FromArray`, `Reverse`, `Clear` | O(n) |
| `TrimExcess` | O(n) when shrinking |

## Notes and Limitations

- One exclusive lock protects the entire list; there are no reader/writer locks.
- Iteration blocks other public operations until the enumerator is destroyed.
- The list does not detect concurrent modification during iteration because concurrent public modification is blocked.
- Sorted search supports both ascending and descending comparer order.
- There is no `DEBUG_LOGGING` constant or runtime debug logging switch in this unit.
