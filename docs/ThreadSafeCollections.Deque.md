# ThreadSafeCollections.Deque Documentation

[Documentation home](README.md) · [Project README](../README.md) ·
[API cheat sheet](CHEATSHEET.md)

**Audience:** application developers and contributors who need the deque API,
behavior, complexity notes, and implementation boundaries.

`TThreadSafeDeque<T>` is a generic double-ended queue protected by one `TCriticalSection` per deque instance.

The current implementation lives in `src/ThreadSafeCollections.Deque.pas`.

## Start with a working example

Build [all examples](BUILDING.md#building-all-examples), then run
`SimpleDeque` from `example-bin/`. Its source demonstrates `PushFront`,
`PushBack`, `TryPopFront`, and `TryPopBack` before the reference material below.

## Dependencies

- Free Pascal 3.2.2 is verified; newer compatible releases are expected but are
  not tested by this repository
- `SyncObjs`
- `ThreadSafeCollections.Interfaces`
- `ThreadSafeCollections.ErrorMessages`

## Storage Model

The deque is circular-array based. It no longer uses linked-list nodes.

Internal fields:

```pascal
FBuffer: array of T;
FHead: Integer;
FTail: Integer;
FCount: Integer;
FCapacity: Integer;
FLock: TCriticalSection;
```

Capacity is always rounded to a power of two, so index wrapping can use bitwise `and` instead of modulo.

Constants:

```pascal
DEFAULT_INITIAL_CAPACITY = 16;
MIN_CAPACITY = 4;
GROWTH_FACTOR = 2;
```

## Construction

```pascal
constructor Create;
constructor Create(AInitialCapacity: Integer);
```

`Create` uses default capacity 16. `Create(AInitialCapacity)` rounds up to the next power of two and uses at least 4.

```pascal
var
  Deque: specialize TThreadSafeDeque<Integer>;
begin
  Deque := specialize TThreadSafeDeque<Integer>.Create(1000);
  try
    Deque.PushBack(1);
    Deque.PushFront(2);
  finally
    Deque.Free;
  end;
end;
```

## Public API

Core operations:

```pascal
procedure PushFront(const AItem: T);
procedure PushBack(const AItem: T);
function PopFront: T;
function PopBack: T;
function PeekFront: T;
function PeekBack: T;
procedure Clear;
```

Safe operations:

```pascal
function TryPopFront(out AValue: T): Boolean;
function TryPopBack(out AValue: T): Boolean;
function TryPeekFront(out AValue: T): Boolean;
function TryPeekBack(out AValue: T): Boolean;
```

Array and bulk operations:

```pascal
function ToArray: specialize TArray<T>;
procedure CopyTo(var AArray: array of T; AStartIndex: Integer = 0);
procedure PushRangeBack(const AItems: array of T);
procedure PushRangeFront(const AItems: array of T);
```

Other operations:

```pascal
property Count: Integer read GetCount;
function IsEmpty: Boolean;
function GetEnumerator: TEnumerator;
function Lock: ILockToken;
```

There is no `Contains` method in the current deque API.

## Thread Safety

Every public operation acquires the deque's `TCriticalSection` and releases it in a `finally` block.

Manual `Lock()` is advanced usage. Do not hold a token and then call public methods on the same deque, because those methods try to acquire the same critical section again.

## Iteration

The enumerator holds an `ILockToken` for the full enumeration:

```pascal
constructor TThreadSafeDeque.TEnumerator.Create(ADeque: TThreadSafeDeque);
begin
  inherited Create;
  FDeque := ADeque;
  FLockToken := FDeque.Lock;
  FCurrentIndex := -1;
end;
```

`MoveNext` maps the logical iteration index to the circular buffer:

```pascal
Idx := (FDeque.FHead + FCurrentIndex) and (FDeque.FCapacity - 1);
FCurrent := FDeque.FBuffer[Idx];
```

During a `for..in` loop, other public operations wait until the enumerator is destroyed.

## Bulk Operation Ordering

`PushRangeBack` appends values in input order.

`PushRangeFront` loops forward and prepends each value. That means the last value in the input array becomes the front element:

```pascal
Deque.PushRangeFront([1, 2, 3]);
// Logical front-to-back order starts with 3, then 2, then 1.
```

This matches the current implementation.

## Clear and Managed Types

`Clear` assigns `Default(T)` to every buffer slot from `0` to `FCapacity - 1`, then resets `FHead`, `FTail`, and `FCount`.

This is intentional. It releases references for managed types such as `string`, interfaces, and dynamic arrays. It also means `Clear` is O(capacity), not O(1).

## Complexity

| Operation | Complexity |
|---|---|
| `PushFront`, `PushBack` | O(1) amortized, O(n) when growing |
| `PopFront`, `PopBack` | O(1) |
| `TryPopFront`, `TryPopBack` | O(1) |
| `PeekFront`, `PeekBack` | O(1) |
| `TryPeekFront`, `TryPeekBack` | O(1) |
| `Count`, `IsEmpty` | O(1) |
| `Clear` | O(capacity) |
| `ToArray`, `CopyTo` | O(n) |
| `PushRangeBack`, `PushRangeFront` | O(n + existing items copied if resize is needed) |
| Iteration | O(n) |

## Notes and Limitations

- One exclusive lock protects the whole deque; there are no reader/writer locks.
- Iteration blocks other public operations until the `for..in` loop finishes.
- The deque does not shrink automatically after `Clear`.
- There is no random access by index.
- There are no search, `Contains`, or bulk remove operations.
- The deque does not own or automatically free class instances stored as values.
- There is no `DEBUG_LOGGING` constant or runtime debug logging switch in this unit.
