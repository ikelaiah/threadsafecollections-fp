# ThreadSafeCollections.List API Documentation

# Table of Contents
 
- [ThreadSafeCollections.List API Documentation](#threadsafecollectionslist-api-documentation)
- [Table of Contents](#table-of-contents)
  - [Component Diagram](#component-diagram)
  - [Core Components](#core-components)
    - [TComparer](#tcomparer)
    - [TThreadSafeList](#tthreadsafelist)
      - [Properties](#properties)
  - [Thread Safety](#thread-safety)
    - [Guarantees](#guarantees)
    - [Implementation Details](#implementation-details)
  - [Methods](#methods)
    - [Constructor](#constructor)
      - [Built-in Comparers](#built-in-comparers)
    - [Basic Operations](#basic-operations)
    - [Sorting and Order](#sorting-and-order)
    - [Sort Order with Custom Types](#sort-order-with-custom-types)
    - [Range Operations](#range-operations)
    - [Search Operations](#search-operations)
    - [Additional Utility Methods](#additional-utility-methods)
    - [Array Operations](#array-operations)
    - [Capacity Management](#capacity-management)
  - [Iterator Support](#iterator-support)
    - [Usage Example - Iterators](#usage-example---iterators)
    - [Iterator Characteristics](#iterator-characteristics)
  - [Performance](#performance)
    - [Complexity Analysis](#complexity-analysis)
    - [Memory Management](#memory-management)
    - [Optimization Tips](#optimization-tips)
  - [Usage Examples](#usage-examples)
    - [Basic Integer List](#basic-integer-list)
    - [Custom Type with Custom Comparer](#custom-type-with-custom-comparer)
  - [Best Practices](#best-practices)
  - [Known Limitations](#known-limitations)


## Component Diagram

```mermaid
classDiagram
    class IThreadSafeCollection~T~ {
        <<interface>>
        +GetCount(): Integer
        +IsEmpty(): Boolean
        +Clear()
        +Lock(): ILockToken
        +Count: Integer
    }

    class IThreadSafeList~T~ {
        <<interface>>
        +Add(Item: T): Integer
        +Delete(Index: Integer)
        +IndexOf(Item: T): Integer
        +First(): T
        +Last(): T
        +Sort(Ascending: Boolean)
        +IsSorted(): Boolean
        +Replace(Index: Integer, Item: T)
        +GetItem(Index: Integer): T
        +SetItem(Index: Integer, Value: T)
        +Items[Index: Integer]: T
    }

    class TThreadSafeList~T~ {
        -FList: array of T
        -FCount: Integer
        -FCapacity: Integer
        -FLock: TCriticalSection
        -FComparer: TComparer~T~
        -FSorted: Boolean
        +Create(AComparer)
        +Destroy()
        +ToArray(): TArray~T~
        +FromArray(Values: array of T)
        +AddRange(Values: array of T)
        +AddRange(Collection: IThreadSafeList~T~)
        +InsertRange(Index: Integer, Values: array of T)
        +InsertRange(Index: Integer, Collection: IThreadSafeList~T~)
        +DeleteRange(AIndex, ACount: Integer)
        +Contains(Value: T): Boolean
        +IndexOfItem(Item: T, StartIndex: Integer): Integer
        +IndexOfItem(Item: T, StartIndex, ACount: Integer): Integer
        +LastIndexOf(Item: T): Integer
        +LastIndexOf(Item: T, StartIndex: Integer): Integer
        +LastIndexOf(Item: T, StartIndex, ACount: Integer): Integer
        +Insert(Index: Integer, Item: T)
        +Exchange(Index1, Index2: Integer)
        +MoveItem(CurIndex, NewIndex: Integer)
        +Reverse()
        +Extract(Item: T): T
        +ExtractAt(Index: Integer): T
        +TrimExcess()
        -Grow()
        -QuickSort(Left, Right: Integer, Ascending: Boolean)
        +Add(Item: T): Integer
        +Delete(Index: Integer)
        +IndexOf(Item: T): Integer
        +Sort(Ascending: Boolean)
        +Replace(Index, Item)
        +First(): T
        +Last(): T
        +IsSorted(): Boolean
        +Clear()
        +IsEmpty(): Boolean
        +Items[Index: Integer]: T
        +GetEnumerator(): TEnumerator
        +Lock(): ILockToken
    }

    class TEnumerator~T~ {
        -FList: TThreadSafeList~T~
        -FIndex: Integer
        -FCurrent: T
        -FLockToken: ILockToken
        +Create(AList: TThreadSafeList~T~)
        +Destroy()
        +MoveNext(): Boolean
        +Current: T
    }

    class ILockToken {
        <<interface>>
        +Release()
    }

    class TLockToken {
        -FLock: TCriticalSection
        +Create(ALock: TCriticalSection)
        +Destroy()
        +Release()
    }

    class TComparer~T~ {
        <<function>>
        +Compare(A: T, B: T): Integer
    }

    IThreadSafeCollection~T~ <|-- IThreadSafeList~T~
    IThreadSafeList~T~ <|.. TThreadSafeList~T~
    TThreadSafeList~T~ ..> TComparer~T~ : uses
    TThreadSafeList~T~ *-- TEnumerator~T~ : contains
    TEnumerator~T~ --> ILockToken : uses
    TThreadSafeList~T~ --> ILockToken : creates
    ILockToken <|.. TLockToken : implements
```
    
## Core Components

### TComparer<T>
Generic function type for comparing elements:
- Input: Two elements of type T (A and B)
- Returns: Integer (-1, 0, or 1)
- Usage: Determines element ordering for sorting and finding

### TThreadSafeList<T>
Thread-safe generic list implementation with built-in synchronization.

#### Properties
- `Count: Integer` - Number of elements in the list
- `Items[Index: Integer]: T` - Default array property for access

## Thread Safety

### Guarantees
The TThreadSafeList<T> provides comprehensive thread safety through TCriticalSection:

1. **Atomic Operations**
   - All public methods are atomic
   - State consistency maintained across method calls
   - No partial updates visible to other threads

2. **Memory Visibility**
   - Changes in one thread visible to other threads
   - Memory barriers ensure proper synchronization
   - No stale reads or writes

3. **Exception Safety**
   - Strong exception guarantee for most operations
   - Basic exception guarantee for bulk operations
   - Lock always released even if exception occurs

4. **Deadlock Prevention**
   - Single lock acquisition order
   - No nested locks within implementation
   - Safe for multi-threaded environments

5. **Iterator Safety**
   - Iterators hold lock during enumeration
   - Other threads block until iteration completes
   - No concurrent modifications during iteration

### Implementation Details
- All public methods protected by TCriticalSection
- Automatic locking/unlocking for all operations
- Exception-safe lock management through try-finally blocks
- Lock acquisition order prevents deadlocks
- Memory barriers ensure visibility across threads
- Sorted status maintained consistently

## Methods

### Constructor

```pascal
constructor Create(AComparer: specialize TComparer<T>);
```

- Creates new list with specified comparer
- Throws exception if comparer is nil

#### Built-in Comparers

```pascal
function IntegerComparer(const A, B: Integer): Integer;
function StringComparer(const A, B: string): Integer;
function BooleanComparer(const A, B: Boolean): Integer;
function RealComparer(const A, B: Real): Integer;
```

### Basic Operations

```pascal
function Add(const Item: T): Integer;
procedure Delete(Index: Integer);
function IndexOf(const Item: T): Integer;
procedure Replace(Index: Integer; const Item: T);
```


### Sorting and Order

```pascal
procedure Sort(Ascending: Boolean = True);
function IsSorted: Boolean;
function First: T;
function Last: T;
```

Sorts the list elements using QuickSort algorithm:
- `Ascending = True` (default): Elements sorted in ascending order (A to Z, 1 to 9)
- `Ascending = False`: Elements sorted in descending order (Z to A, 9 to 1)
- Thread-safe: Protected by critical section
- Time complexity: O(n log n)

Examples:
```pascal
// Ascending order (default)
List.Sort;  // or List.Sort(True);
// [1, 2, 3, 4, 5]

// Descending order
List.Sort(False);
// [5, 4, 3, 2, 1]
```

### Sort Order with Custom Types
When using custom comparers, the sort order follows the comparer's logic:

```pascal
// Custom student record sorting
type
  TStudent = record
    Name: string;
    ID: Integer;
  end;

// Sort by name
function StudentNameComparer(const A, B: TStudent): Integer;
begin
  Result := CompareStr(A.Name, B.Name);
end;

var
  Students: specialize TThreadSafeList<TStudent>;
begin
  Students := specialize TThreadSafeList<TStudent>.Create(@StudentNameComparer);
  try
    // ... add students ...
    
    Students.Sort;      // A to Z order
    Students.Sort(False); // Z to A order
  finally
    Students.Free;
  end;
end;
```

### Range Operations

```pascal
procedure AddRange(const Values: array of T);
procedure AddRange(const Collection: specialize IThreadSafeList<T>);
procedure InsertRange(Index: Integer; const Values: array of T);
procedure InsertRange(Index: Integer; const Collection: specialize IThreadSafeList<T>);
procedure DeleteRange(AIndex, ACount: Integer);
```

Provides bulk operations for adding, inserting, and deleting multiple elements:
- Thread-safe operations with single lock acquisition
- Efficient memory management for bulk changes
- Maintains internal array consistency

Examples:
```pascal
var
  List1, List2: specialize TThreadSafeList<Integer>;
begin
  List1 := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  List2 := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    // Add multiple items at once
    List1.AddRange([1, 2, 3, 4, 5]);
    
    // Add items from another thread-safe list
    List2.AddRange(List1);
    
    // Insert items at specific position
    List1.InsertRange(2, [10, 11, 12]);
    
    // Delete multiple items
    List1.DeleteRange(1, 3); // Removes 3 items starting at index 1
  finally
    List1.Free;
    List2.Free;
  end;
end;
```

### Search Operations

```pascal
function Contains(const Value: T): Boolean;
function IndexOfItem(const Item: T; StartIndex: Integer): Integer;
function IndexOfItem(const Item: T; StartIndex, ACount: Integer): Integer;
function LastIndexOf(const Item: T): Integer;
function LastIndexOf(const Item: T; StartIndex: Integer): Integer;
function LastIndexOf(const Item: T; StartIndex, ACount: Integer): Integer;
```

Provides various search capabilities:
- Thread-safe search operations
- Forward and backward searching
- Range-limited searches
- Uses the provided comparer for equality checks

Examples:
```pascal
var
  List: specialize TThreadSafeList<Integer>;
  Index: Integer;
begin
  List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    List.AddRange([1, 2, 3, 2, 4, 2, 5]);
    
    // Check if value exists
    if List.Contains(2) then
      WriteLn('Found 2');
    
    // Find first occurrence after index 2
    Index := List.IndexOfItem(2, 2);
    
    // Find last occurrence
    Index := List.LastIndexOf(2);
    
    // Find last occurrence before index 4
    Index := List.LastIndexOf(2, 4);
  finally
    List.Free;
  end;
end;
```

### Additional Utility Methods

```pascal
procedure Insert(Index: Integer; const Item: T);
procedure Exchange(Index1, Index2: Integer);
procedure MoveItem(CurIndex, NewIndex: Integer);
procedure Reverse;
function Extract(const Item: T): T;
function ExtractAt(Index: Integer): T;
procedure TrimExcess;
```

Provides additional list manipulation capabilities:
- Thread-safe operations
- Position-based insertions
- Item extraction
- List reversal
- Memory optimization

Examples:
```pascal
var
  List: specialize TThreadSafeList<Integer>;
  Value: Integer;
begin
  List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    List.AddRange([1, 2, 3, 4, 5]);
    
    // Insert at specific position
    List.Insert(2, 10);  // [1, 2, 10, 3, 4, 5]
    
    // Swap two elements
    List.Exchange(1, 4); // [1, 4, 10, 3, 2, 5]
    
    // Move element to new position
    List.MoveItem(2, 5); // [1, 4, 3, 2, 5, 10]
    
    // Reverse the entire list
    List.Reverse;        // [10, 5, 2, 3, 4, 1]
    
    // Extract and remove item
    Value := List.Extract(5); // Removes and returns 5
    
    // Optimize memory usage
    List.TrimExcess;
  finally
    List.Free;
  end;
end;
```

### Array Operations

```pascal
function ToArray: specialize TArray<T>;
procedure FromArray(const Values: array of T);
```

Provides conversion between list and array:
- Thread-safe conversion operations
- Efficient memory copying
- Maintains data consistency

Examples:
```pascal
var
  List: specialize TThreadSafeList<Integer>;
  Arr: specialize TArray<Integer>;
begin
  List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    // Convert array to list
    List.FromArray([1, 2, 3, 4, 5]);
    
    // Convert list to array
    Arr := List.ToArray;
  finally
    List.Free;
  end;
end;
```

### Capacity Management

```pascal
property Capacity: Integer read GetCapacity write SetCapacity;
procedure TrimExcess;
```

Provides control over internal array capacity:
- Get/Set the current capacity
- Optimize memory usage
- Thread-safe capacity modifications
 
```pascal
var
  List: specialize TThreadSafeList<Integer>;

begin
  List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    // Pre-allocate space for 1000 items
    List.Capacity := 1000;
    // ... use list ...
    // Trim unused capacity
    List.TrimExcess;
  finally
    List.Free;
  end;
end;
```

## Iterator Support

```pascal
type
  TIterator = class(TObject)
  public
    function MoveNext: Boolean;
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

function GetEnumerator: TIterator;
```

### Usage Example - Iterators
```pascal
var
  List: specialize TThreadSafeList<Integer>;
  Item: Integer;
begin
  List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    List.Add(1);
    List.Add(2);
    List.Add(3);
    
    // Using iterator
    for Item in List do
      WriteLn(Item);
  finally
    List.Free;
  end;
end;
```

### Iterator Characteristics
- Thread-safe through RAII locking
- Automatic lock acquisition and release
- Exception-safe lock management
- Forward-only iteration
- Protected from modifications during iteration (via RAII lock)
- Other threads must wait for iteration to complete before modifying


## Performance

### Complexity Analysis
- Add: O(1) amortized
- Delete: O(n)
- IndexOf: O(n)
- Sort: O(n log n)
- Range operations: O(n) with single lock acquisition
- Search operations: O(n) in unsorted lists
- Extract operations: O(n) combining find and remove in single lock

### Memory Management
- Growing strategy: Double capacity when full (4, 8, 16, etc.)
- Memory moves optimized for bulk operations
- TrimExcess optimizes memory usage after removals

### Optimization Tips
1. **Use Bulk Operations**
   ```pascal
   // Instead of:
   for I := 0 to High(Values) do
     List.Add(Values[I]);
   
   // Use:
   List.AddRange(Values);
   ```

2. **Manage Capacity**
   ```pascal
   // Pre-allocate if size is known
   List.Capacity := ExpectedSize;
   
   // Trim after bulk removals
   List.DeleteRange(0, RemoveCount);
   List.TrimExcess;
   ```

3. **Optimize Searches**
   ```pascal
   // For frequent searches, keep list sorted
   List.Sort;
   ```

4. **Minimize Lock Contention**
   ```pascal
   // Use Lock() for multiple operations
   var
     Token: ILockToken;
   begin
     Token := List.Lock;
     try
       // Multiple operations here
     finally
       Token := nil; // Release lock
     end;
   end;
   ```



## Usage Examples

### Basic Integer List

```pascal
var
    List: specialize TThreadSafeList<Integer>;

begin
    List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
    try
        List.Add(42);
        List.Add(17);
        List.Sort;
    finally
        List.Free;
    end;
end;
```

### Custom Type with Custom Comparer


```pascal
type
    TStudent = record
    Name: string;
    ID: Integer;
end;

function StudentNameComparer(const A, B: TStudent): Integer;
begin
    Result := CompareStr(A.Name, B.Name);
end;

var
    Students: specialize TThreadSafeList<TStudent>;

begin
    Students := specialize TThreadSafeList<TStudent>.Create(@StudentNameComparer);
    try 
        // Use the list...
    finally
    Students.Free;
end;
```



## Best Practices

1. **Memory Management**
   - Use TrimExcess periodically in long-running applications
   - Consider initial capacity when known
   - Use bulk operations instead of multiple single operations

2. **Search Operations**
   - Use Contains for simple existence checks
   - Use IndexOfItem for forward searches
   - Use LastIndexOf for backward searches
   - Consider sorting for better search performance

3. **Range Operations**
   - Prefer AddRange over multiple Add calls
   - Use InsertRange for bulk insertions
   - Use DeleteRange for bulk deletions
   - Check bounds before range operations


## Known Limitations

1. **Iterator Limitations**
   - Forward-only iteration
   - No concurrent modification detection
   - Must wait for iteration to complete before modifying

2. **Bulk Operation Characteristics**
   - Single lock acquisition per bulk operation
   - Memory allocation may grow in steps
   - No parallel processing of bulk operations