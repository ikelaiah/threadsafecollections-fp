# ThreadSafeCollections-FP Cheat Sheet

[Documentation home](../index.md) · [Project README](../../README.md) · [Build and verify](../project/building.md)

**Audience:** developers who need a generated, compact reference to the current public API and source complexity annotations.

> Generated file. Do not edit manually.
>
> Regenerate from the repository root with: `pwsh -File ./tools/generate-cheatsheet.ps1`

Package version: `0.8.9`

## Source Inputs

- `src/ThreadSafeCollections.Interfaces.pas`
- `src/ThreadSafeCollections.List.pas`
- `src/ThreadSafeCollections.Deque.pas`
- `src/ThreadSafeCollections.Dictionary.pas`
- `src/ThreadSafeCollections.HashSet.pas`
- `src/HashFunctions.pas`
- `package/lazarus/ThreadSafeCollections.lpk`

## Collection Types

| Collection | Unit | Primary storage | Iteration model |
|---|---|---|---|
| `TThreadSafeList<T>` | `ThreadSafeCollections.List` | Dynamic array | Holds lock for full `for..in` loop |
| `TThreadSafeDeque<T>` | `ThreadSafeCollections.Deque` | Circular array | Holds lock for full `for..in` loop |
| `TThreadSafeDictionary<TKey, TValue>` | `ThreadSafeCollections.Dictionary` | Bucket array with chained entries | Snapshot at enumerator construction |
| `TThreadSafeHashSet<T>` | `ThreadSafeCollections.HashSet` | Bucket array with chained entries | Holds lock for full `for..in` loop |

## Shared Interfaces

### IThreadSafeCollection<T>

| Complexity | Declaration |
|---|---|
| O(1) | `function GetCount: Integer;` |
| O(1) | `function IsEmpty: Boolean;` |
| O(n) where n is the number of elements | `procedure Clear;` |
| O(1) | `function Lock: ILockToken;` |
| O(1) | `property Count: Integer read GetCount;` |

### IThreadSafeList<T>

| Complexity | Declaration |
|---|---|
| O(1) amortized, O(n) worst case during resize | `function Add(const Item: T): Integer;` |
| O(n) due to element shifting | `procedure Delete(Index: Integer);` |
| O(n) unsorted; O(log n) after ascending or descending Sort | `function IndexOf(const Item: T): Integer;` |
| O(1) | `function First: T;` |
| O(1) | `function Last: T;` |
| O(n log n) average case, O(n²) worst case | `procedure Sort(Ascending: Boolean = True);` |
| O(1) | `function IsSorted: Boolean;` |
| O(1) | `procedure Replace(Index: Integer; const Item: T);` |
| O(1) | `function GetItem(Index: Integer): T;` |
| O(1) | `procedure SetItem(Index: Integer; const Value: T);` |
| O(1) | `function GetCapacity: Integer;` |
| O(n) when resizing | `procedure SetCapacity(const Value: Integer);` |
| O(n) | `function ToArray: specialize TArray<T>;` |
| O(n) | `procedure FromArray(const Values: array of T);` |
| O(m) where m is array length, O(n+m) if resize needed | `procedure AddRange(const Values: array of T); overload;` |
| O(m) where m is collection size, O(n+m) if resize needed | `procedure AddRange(const Collection: specialize IThreadSafeList<T>); overload;` |
| O(n+m) due to element shifting | `procedure InsertRange(Index: Integer; const Values: array of T); overload;` |
| O(n+m) due to element shifting | `procedure InsertRange(Index: Integer; const Collection: specialize IThreadSafeList<T>); overload;` |
| O(n) due to element shifting | `procedure DeleteRange(AIndex, ACount: Integer);` |
| O(n) unsorted; O(log n) after ascending or descending Sort | `function Contains(const Value: T): Boolean;` |
| O(n) | `function IndexOfItem(const Item: T; StartIndex: Integer): Integer; overload;` |
| O(n) | `function IndexOfItem(const Item: T; StartIndex, ACount: Integer): Integer; overload;` |
| O(n) | `function LastIndexOf(const Item: T): Integer; overload;` |
| O(n) | `function LastIndexOf(const Item: T; StartIndex: Integer): Integer; overload;` |
| O(n) | `function LastIndexOf(const Item: T; StartIndex, ACount: Integer): Integer; overload;` |
| O(n) due to element shifting | `procedure Insert(Index: Integer; const Item: T);` |
| O(1) | `procedure Exchange(Index1, Index2: Integer);` |
| O(n) due to element shifting | `procedure MoveItem(CurIndex, NewIndex: Integer);` |
| O(n) | `procedure Reverse;` |
| O(n) search + O(n) deletion | `function Extract(const Item: T): T;` |
| O(n) due to element shifting | `function ExtractAt(Index: Integer): T;` |
| O(n) when resizing | `procedure TrimExcess;` |
| O(1) | `property Items[Index: Integer]: T read GetItem write SetItem; default;` |
| read O(1), write O(n) when resizing | `property Capacity: Integer read GetCapacity write SetCapacity;` |

### IThreadSafeDeque<T>

| Complexity | Declaration |
|---|---|
| O(1) amortized, O(n) during resize | `procedure PushFront(const Item: T);` |
| O(1) amortized, O(n) during resize | `procedure PushBack(const Item: T);` |
| O(1) | `function PopFront: T;` |
| O(1) | `function PopBack: T;` |
| O(1) | `function TryPopFront(out Value: T): Boolean;` |
| O(1) | `function TryPopBack(out Value: T): Boolean;` |
| O(1) | `function PeekFront: T;` |
| O(1) | `function PeekBack: T;` |
| O(1) | `function TryPeekFront(out Value: T): Boolean;` |
| O(1) | `function TryPeekBack(out Value: T): Boolean;` |
| O(m) where m is array length, O(n+m) if resize needed | `procedure PushRangeFront(const Items: array of T);` |
| O(m) where m is array length, O(n+m) if resize needed | `procedure PushRangeBack(const Items: array of T);` |

### IThreadSafeDictionary<TKey, TValue>

| Complexity | Declaration |
|---|---|
| O(1) average case, O(n) during resize | `procedure Add(const Key: TKey; const Value: TValue);` |
| O(1) average case | `function Remove(const Key: TKey): Boolean;` |
| O(1) average case | `function TryGetValue(const Key: TKey; out Value: TValue): Boolean;` |
| O(1) average case, O(n) during resize | `procedure AddOrSetValue(const Key: TKey; const Value: TValue);` |
| O(1) average case | `function ContainsKey(const Key: TKey): Boolean;` |
| O(1) average case | `function GetItem(const Key: TKey): TValue;` |
| O(1) average case | `procedure SetItem(const Key: TKey; const Value: TValue);` |
| O(1) | `function GetCount: Integer;` |
| O(n) | `procedure Clear;` |
| O(1) | `function Lock: ILockToken;` |
| O(n) | `function GetKeys: specialize TKeyArray<TKey>;` |
| O(n) | `function GetValues: specialize TValueArray<TValue>;` |
| O(n) when resizing | `procedure TrimExcess;` |
| O(1) average case | `function TryAdd(const Key: TKey; const Value: TValue): Boolean;` |
| O(m) where m is dictionary size | `procedure AddRange(const ADictionary: specialize IThreadSafeDictionary<TKey, TValue>); overload;` |
| O(m) where m is array length | `procedure AddRange(const AArray: specialize TPairArray<TKey, TValue>); overload;` |
| O(n) | `function ToArray: specialize TPairArray<TKey, TValue>;` |
| O(n) | `function ContainsValue(const Value: TValue): Boolean;` |
| O(1) average case | `property Items[const Key: TKey]: TValue read GetItem write SetItem; default;` |
| O(1) | `property Count: Integer read GetCount;` |

### IThreadSafeHashSet<T>

| Complexity | Declaration |
|---|---|
| O(1) average case, O(n) during resize | `function Add(const Item: T): Boolean;` |
| O(1) average case | `function Remove(const Item: T): Boolean;` |
| O(1) average case | `function Contains(const Item: T): Boolean;` |
| O(n) | `function ToArray: specialize TArray<T>;` |
| O(m) where m is array length, O(n+m) if resize needed | `procedure AddRange(const Items: array of T); overload;` |
| O(m) where m is collection size, O(n+m) if resize needed | `procedure AddRange(const Collection: specialize IThreadSafeHashSet<T>); overload;` |
| O(m) average case | `function RemoveRange(const Items: array of T): Integer; overload;` |
| O(m) average case | `function RemoveRange(const Collection: specialize IThreadSafeHashSet<T>): Integer; overload;` |
| O(1) average case | `function TryGetValue(const Item: T; out Value: T): Boolean;` |
| O(n+m) where n and m are set sizes | `procedure IntersectWith(const Collection: specialize IThreadSafeHashSet<T>);` |
| O(m) where m is other set size | `procedure UnionWith(const Collection: specialize IThreadSafeHashSet<T>);` |
| O(m) where m is other set size | `procedure ExceptWith(const Collection: specialize IThreadSafeHashSet<T>);` |
| O(min(n,m)) where n and m are set sizes | `function Overlaps(const Collection: specialize IThreadSafeHashSet<T>): Boolean;` |
| O(n) where n is set size | `function SetEquals(const Collection: specialize IThreadSafeHashSet<T>): Boolean;` |

## Constructors

### List constructors

- `constructor Create(AComparer: specialize TComparer<T>);`
- `constructor Create(AComparer: specialize TComparer<T>; AInitialCapacity: Integer);`

### Deque constructors

- `constructor Create;`
- `constructor Create(AInitialCapacity: Integer);`

### Dictionary constructors

- `constructor Create;`
- `constructor Create(InitialCapacity: integer);`
- `constructor Create(const AHashFunc: specialize THashFunction<TKey>; const AEqualityComparer: specialize TEqualityComparison<TKey>);`
- `constructor Create(InitialCapacity: integer; const AHashFunc: specialize THashFunction<TKey>; const AEqualityComparer: specialize TEqualityComparison<TKey>);`

### HashSet constructors

- `constructor Create(AEqualityComparer: specialize THashSetEqualityComparer<T>; AHashFunction: specialize THashFunction<T>; AInitialCapacity: Integer = INITIAL_BUCKET_COUNT);`
- `constructor Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); overload;`
- `constructor Create(AHashFunction: specialize THashFunction<string>; AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); overload;`

## Concrete-Only Dictionary API

### Dictionary concrete members not present in IThreadSafeDictionary

- `function First(out Key: TKey; out Value: TValue): boolean;`
- `function Last(out Key: TKey; out Value: TValue): boolean;`
- `property Count: Integer read GetCount;`
- `procedure ResizeBuckets(NewSize: integer);`
- `function GetBucketCount: integer;`
- `property BucketCount: integer read GetBucketCount;`

## Built-In Comparers and Hash Helpers

### List comparers

- `function IntegerComparer(const A, B: Integer): Integer;`
- `function StringComparer(const A, B: string): Integer;`
- `function BooleanComparer(const A, B: Boolean): Integer;`
- `function RealComparer(const A, B: Real): Integer;`

### HashSet equality comparers

- `function IntegerEquals(const A, B: Integer): Boolean;`
- `function StringEquals(const A, B: string): Boolean;`
- `function BooleanEquals(const A, B: Boolean): Boolean;`
- `function RealEquals(const A, B: Real): Boolean;`

### HashSet primitive hash functions

- `function IntegerHash(const Value: Integer): Cardinal;`
- `function StringHash(const Value: string): Cardinal;`
- `function BooleanHash(const Value: Boolean): Cardinal;`
- `function RealHash(const Value: Real): Cardinal;`

### HashFunctions unit

- `function XXHash32(const Key: string): Cardinal;`
- `function FNV1aHash(const Key: string): Cardinal;`
- `function MultiplicativeHash(Key: Cardinal): Cardinal;`
- `function DefaultHash(const Key): Cardinal;`

## Locking Notes

- Every collection uses one `TCriticalSection` per collection instance.
- Public operations are internally synchronized.
- Manual `Lock()` returns `ILockToken`, but public methods on the same collection also acquire the same lock.
- Do not hold a manual token and then call public methods on the same collection on non-reentrant `TCriticalSection` platforms.
- List, HashSet, and Deque iterators hold the lock for the full loop.
- Dictionary iterators copy a snapshot, release the lock, then iterate over the snapshot.

## Behaviour Notes

- `TThreadSafeList` uses direction-aware binary search for `IndexOf` and `Contains` after either ascending or descending `Sort`.
- Dictionary default key equality and `ContainsValue` use RTL type-aware default equality comparers.
- Dictionary and HashSet collection bulk overloads snapshot the source before mutating the destination, avoiding nested source locks on POSIX.
- `TThreadSafeDeque.PushRangeFront` prepends values in input order, so the last input item becomes the front item.
