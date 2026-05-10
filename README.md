# 🔒 ThreadSafeCollections-FP

[![License: MIT](https://img.shields.io/badge/License-MIT-1E3A8A.svg)](https://opensource.org/licenses/MIT)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2+-3B82F6.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.0+-60A5FA.svg)](https://www.lazarus-ide.org/)
![Supports Windows](https://img.shields.io/badge/support-Windows-F59E0B?logo=Windows)
![Supports Linux](https://img.shields.io/badge/support-Linux-F59E0B?logo=Linux)
[![Version](https://img.shields.io/badge/version-0.8.3-8B5CF6.svg)](CHANGELOG.md)
![No Dependencies](https://img.shields.io/badge/dependencies-none-10B981.svg)
[![Documentation](https://img.shields.io/badge/Docs-Available-brightgreen.svg)](docs/)
[![Status](https://img.shields.io/badge/Status-Stable-brightgreen.svg)]()


A thread-safe generic collections library for Free Pascal, designed for learning and experimentation.

> [!NOTE]
> 📚 **Library Maturity**: This is a learning-focused project with stable core functionality.
> 
> For production applications requiring battle-tested code, consider these more mature alternatives:
> 
> 1. [FPC Generics.Collections](https://gitlab.com/freepascal.org/fpc/source/-/blob/main/packages/rtl-generics/src/generics.collections.pas) - Official FPC generic collections
> 2. [FCL-STL](https://gitlab.com/freepascal.org/fpc/source/-/tree/main/packages/fcl-stl) - FPC's template library
> 3. [LGenerics](https://github.com/avk959/LGenerics) - Comprehensive generics library


## 🚧 Development Status

**Latest Release: v0.8.3** - Documentation and Package Metadata Release

Current State:

- ✅ Basic operations working (Add, Remove, GetItem)
- ✅ Thread safety verified through testing
- ✅ Memory management stable
- ✅ Thread-Safe Iterator Support
   - List, HashSet, Deque: RAII-style locking — lock held for the full `for…in` loop
   - Dictionary (v0.8.2): snapshot-based — lock released immediately after entry copy; concurrent modifications are safe but not visible to the iterator
- ✅ Bulk operations support
- ✅ **NEW in v0.8.3**: Documentation and tooling refresh
  - Lazarus package metadata updated to 0.8.3
  - Generated API cheat sheet available at [docs/CHEATSHEET.md](docs/CHEATSHEET.md)
  - PowerShell generator available at [tools/generate-cheatsheet.ps1](tools/generate-cheatsheet.ps1)
  - Documentation refreshed against the current source code
- ✅ **v0.8.2**: Critical bug fixes and performance optimisations
  - Fixed several re-entrant lock deadlocks in List, HashSet, and Dictionary on POSIX platforms
  - Fixed managed-type memory safety (`string`/`interface`) in List and Deque
  - Fixed ABBA cross-collection deadlock in `HashSet.IntersectWith`
  - Fixed `IntersectWith` incorrect item removal
  - Fixed `IntegerComparer` overflow
  - Removed dead `DEBUG_LOGGING` code from Dictionary; unified locking API
  - **Slab allocator** for Dictionary and HashSet `TEntry` records — 256-entry blocks with freelist recycling; 19–41% faster Dictionary ops, 15–19% faster HashSet Add at 1 M items
  - **4-lane XXHash32** — strings ≥ 16 bytes processed across four independent accumulators; 19–23% faster for long string keys
  - **Dictionary type dispatch caching** — `TKeyKind` enum cached at construction, eliminating per-call `TypeInfo` comparisons
  - **Binary search for ascending sorted lists** — `Contains`/`IndexOf` use O(log n) binary search after `Sort(True)`
  - **Dictionary iterator is now snapshot-based** — lock released immediately after copying; other threads may modify concurrently
- ✅ **v0.8.1**: Code maintainability improvements
  - Algorithm complexity annotations (Big-O) on all 80+ methods
  - Centralized error messages (14 constants)
  - Named constants replacing magic numbers
  - Zero memory leaks (100% cleanup rate)
- ✅ **v0.8.0**: Performance optimisations implemented
  - Circular array-based Deque (5-10x faster)
  - Pre-allocation strategies for List
  - Optimised hash table resizing

Planned Features:

- 🔄 Read-write lock support (concurrent reads)
- 🔄 Lock-free operations for simple checks
- 🔄 More specialized types

## 🎯 Why Use This?

- 💡 **Learning Tool**: Perfect for understanding thread-safe collections
- 🔒 **Simple Thread Safety**: Just like regular collections, but thread-safe
- 🚀 **Easy to Use**: Specialized types for common data (Integer, String, Boolean, Real)
- ⚡ **Good for Prototypes**: Ideal for quick multi-threaded demos

## 🎓 Getting Started

If you are new to Object Pascal generics, start with a complete program like this:

```pascal
program HelloThreadSafeList;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  ThreadSafeCollections.List;

var
  Numbers: specialize TThreadSafeList<Integer>;
begin
  Numbers := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    Numbers.Add(42);
    Numbers.Add(17);
    Numbers.Sort;

    WriteLn('First number: ', Numbers[0]);
    WriteLn('Count: ', Numbers.Count);
  finally
    Numbers.Free;
  end;
end.
```

Two Object Pascal details matter in most examples:

- `specialize TThreadSafeList<Integer>` creates a concrete list type from the generic list.
- Lists need a comparer, such as `@IntegerComparer`, because sorting and searching depend on type-specific comparison.

This library provides four main collection types:

1. **ThreadSafeList**: Like an array that can grow
```pascal
uses 
  ThreadSafeCollections.List;  // Built-in comparers included!

var
  List: specialize TThreadSafeList<Integer>;
begin
  // Basic creation using a built-in comparer
  List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);    // For integers
  // List := specialize TThreadSafeList<string>.Create(@StringComparer);   // For strings
  // List := specialize TThreadSafeList<Boolean>.Create(@BooleanComparer); // For booleans
  // List := specialize TThreadSafeList<Real>.Create(@RealComparer);       // For reals
  // List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer, 1000); // With initial capacity
  
  try
    List.Add(42);  // Simple to use!
    List.Sort;     // Automatic sorting with the comparer
  finally
    List.Free;
  end;
end;
```

> [!TIP]
> Built-in comparers in ThreadSafeCollections.List:
> - `IntegerComparer`: For Integer types
> - `StringComparer`: For string types
> - `BooleanComparer`: For Boolean types
> - `RealComparer`: For Real types
> 
> For custom types, implement your own comparer: `function MyComparer(const A, B: TMyType): Integer;`

2. **ThreadSafeDeque**: A double-ended queue (v0.8: Now circular array-based!)

```pascal
var
  Deque: specialize TThreadSafeDeque<Integer>;
begin
  // Create with default capacity (16) or specify initial capacity
  Deque := specialize TThreadSafeDeque<Integer>.Create;
  // Deque := specialize TThreadSafeDeque<Integer>.Create(1000); // For better performance
  try
    Deque.PushBack(1);
    Deque.PushFront(2);
    WriteLn('Front item: ', Deque.PopFront);
    WriteLn('Back item: ', Deque.PopBack);
  finally
    Deque.Free;
  end;
end;
```

3. **ThreadSafeDictionary**: Store key-value pairs
```pascal
uses 
  ThreadSafeCollections.Dictionary;

var
  Dict: specialize TThreadSafeDictionary<string, integer>;
begin
  Dict := specialize TThreadSafeDictionary<string, integer>.Create;
  try
    Dict.Add('one', 1);
    Dict.Add('two', 2);
    
    if Dict.ContainsKey('one') then
      WriteLn('Found: ', Dict['one']);
  finally
    Dict.Free;
  end;
end;
```

> [!TIP]
> - For basic types (integer, string, etc.), use `Create` or `Create(capacity)`
> - For custom types, use `Create(hashFunc, equalityFunc)` or `Create(capacity, hashFunc, equalityFunc)`

4. **ThreadSafeHashSet**: Store unique values
```pascal
var
  UniqueNames: TThreadSafeHashSetString;
begin
  UniqueNames := TThreadSafeHashSetString.Create;
  try
    UniqueNames.Add('unique');  // Duplicates handled automatically
  finally
    UniqueNames.Free;
  end;
end;
```

> [!TIP]
> Always use try-finally blocks to ensure proper cleanup:
> ```pascal
> try
>   // Your code here
> finally
>   Collection.Free;
> end;
> ```

## 🚀 Quick Start

### 📋 Requirements

- Free Pascal 3.2.2 or later
- No external dependencies

### Using ThreadSafeList

```pascal
uses ThreadSafeCollections.List;

// Create a thread-safe list of integers
var
  Numbers: specialize TThreadSafeList<Integer>;
begin
  Numbers := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    // Multiple threads can safely add/remove items
    Numbers.Add(42);
    Numbers.Add(17);
    Numbers.Sort;  // Thread-safe sorting
    
    WriteLn(Numbers[0]); // Thread-safe access
  finally
    Numbers.Free;
  end;
end;
```

### ThreadSafeList with Custom Types

```pascal
uses ThreadSafeCollections.List;

type
  TStudent = record
      Name: string;
      StudentId: Integer;
end;

// Custom comparer for sorting
function StudentNameComparer(const A, B: TStudent): Integer;
begin
  Result := CompareStr(A.Name, B.Name);
end;

var
  Students: specialize TThreadSafeList<TStudent>;
begin
  Students := specialize TThreadSafeList<TStudent>.Create(@StudentNameComparer);
  try 
      // ... use the list
  finally
      Students.Free;
  end;
end;
```

### Using ThreadSafeDeque

```pascal
uses
  ThreadSafeCollections.Deque;

var
  Deque: specialize TThreadSafeDeque<string>;
  Name: string;
begin
  Deque := specialize TThreadSafeDeque<string>.Create;
  try
    // Add items to the front and back
    Deque.PushFront('Obed');
    Deque.PushFront('Jesse');
    Deque.PushBack('David');

    // Remove items from the front and back
    if Deque.TryPopFront(Name) then
      WriteLn('Popped from front: ', Name);

    if Deque.TryPopBack(Name) then
      WriteLn('Popped from back: ', Name);
  finally
    Deque.Free;
  end;

// Other code

end.
```


### Using ThreadSafeDeque with Custom Types

```pascal
{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

uses
  ThreadSafeCollections.Deque;

type
  TPerson = record
    Name: string;
    Age: Integer;
    public
    constructor Create(NewName: string; NewAge: Integer);
  end;

constructor TPerson.Create(NewName: string; NewAge: Integer);
begin
  Name := NewName;
  Age := NewAge;
end;

var
  Deque: specialize TThreadSafeDeque<TPerson>;
  Person: TPerson;
begin
  Deque := specialize TThreadSafeDeque<TPerson>.Create;
  try
    // Add items to the front and back
    Deque.PushFront(TPerson.Create('Alice', 30));
    Deque.PushBack(TPerson.Create('Bob', 25));

    // Remove items from the front and back
    if Deque.TryPopFront(Person) then
      WriteLn('Popped from front: ', Person.Name);

    if Deque.TryPopBack(Person) then
      WriteLn('Popped from back: ', Person.Name);
  finally
    Deque.Free;
  end;
end;
```

### Using ThreadSafeHashSet

#### 1. Basic String Set (Using Built-in Type)
```pascal
uses 
  ThreadSafeCollections.HashSet;

var
  UniqueNames: TThreadSafeHashSetString;
begin
  UniqueNames := TThreadSafeHashSetString.Create;
  try
    // Add items (duplicates are ignored)
    UniqueNames.Add('Alice');    // Returns True (added)
    UniqueNames.Add('Bob');      // Returns True (added)
    UniqueNames.Add('Alice');    // Returns False (already exists)
    
    // Check existence
    if UniqueNames.Contains('Alice') then
      WriteLn('Alice is in the set');
      
    // Remove items
    UniqueNames.Remove('Bob');   // Returns True (was removed)
    
    WriteLn('Count: ', UniqueNames.Count); // Outputs: 1
  finally
    UniqueNames.Free;
  end;
end;
```

#### 2. Custom Type Set (Advanced Usage)
```pascal
uses 
  ThreadSafeCollections.HashSet;

type
  TPoint = record
    X, Y: Integer;
  end;

// Compare two points for equality
function PointEquals(const A, B: TPoint): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

// Generate hash code for a point
function PointHash(const Value: TPoint): Cardinal;
begin
  Result := Cardinal(Value.X xor Value.Y);
end;

var
  UniquePoints: specialize TThreadSafeHashSet<TPoint>;
  Point: TPoint;
begin
  UniquePoints := specialize TThreadSafeHashSet<TPoint>.Create(@PointEquals, @PointHash);
  try
    // Add unique points
    Point.X := 1;
    Point.Y := 1;
    UniquePoints.Add(Point);

    Point.X := 2;
    Point.Y := 2;
    UniquePoints.Add(Point);
    
    // Check for existence
    Point.X := 1;
    Point.Y := 1;
    if UniquePoints.Contains(Point) then
      WriteLn('Point (1,1) exists');
  finally
    UniquePoints.Free;
  end;
end;
```

### Using ThreadSafeHashSet with Set Operations

```pascal
var
  SetA, SetB: TThreadSafeHashSetInteger;
  Numbers: array of Integer;
begin
  SetA := TThreadSafeHashSetInteger.Create;
  SetB := TThreadSafeHashSetInteger.Create;
  try
    // Setup sets
    SetA.Add(1);
    SetA.Add(2);
    SetA.Add(3);
    
    SetB.Add(2);
    SetB.Add(3);
    SetB.Add(4);
    
    // Intersection: Keep only items in both sets
    SetA.IntersectWith(SetB);  // SetA now contains {2, 3}
    
    // Union: Add all unique items from both sets
    SetA.UnionWith(SetB);      // SetA now contains {1, 2, 3, 4}
    
    // Difference: Remove items that exist in SetB
    SetA.ExceptWith(SetB);     // SetA now contains {1}
    
    // Bulk operations
    SetLength(Numbers, 3);
    Numbers[0] := 5;
    Numbers[1] := 6;
    Numbers[2] := 7;
    
    SetA.AddRange(Numbers);    // Add multiple items at once
    SetA.AddRange(SetB);       // Add all items from another set
  finally
    SetA.Free;
    SetB.Free;
  end;
end;
```

### Performance Characteristics

Benchmarks on **Dell Inspiron 15 7510** (Intel i7-11800H @ 2.30 GHz, 8 cores, 16 GB RAM, Windows 11).

> [!NOTE]
> v0.8.2 introduced three performance improvements that affect these figures:
> slab allocator (19–41% faster Dictionary ops, 15–19% faster HashSet Add at 1 M items),
> 4-lane XXHash32 (19–23% faster for long string keys), and
> binary search for ascending sorted lists (`Contains`/`IndexOf` become O(log n) after `Sort(True)`).

**List Operations:**

| Operation            | Time (ms) | Items   | Notes                                  |
|----------------------|-----------|---------|----------------------------------------|
| Sort Integers        | 47        | 100,000 | QuickSort                              |
| Sort Strings         | 235       | 100,000 | QuickSort                              |
| Sort Students (Name) | 312       | 100,000 | Custom comparer                        |
| Sort Students (ID)   | 234       | 100,000 | Custom comparer                        |
| Contains (unsorted)  | O(n)      | —       | Linear scan                            |
| Contains (ascending sorted) | O(log n)  | —       | Binary search — automatic after Sort(True) |

**Dictionary Operations:**

| Operation | Time (ms) | Items   | Notes                         |
|-----------|-----------|---------|-------------------------------|
| Add       | 672       | 100,000 | Bulk insert (v0.8.2 allocator)|
| Find      | 63        | 100,000 | Sequential lookups            |

**HashSet Operations:**

| Operation       | Time (ms) | Items   | Notes             |
|-----------------|-----------|---------|-------------------|
| Add             | 31        | 100,000 | Bulk insert       |
| Find            | 47        | 100,000 | Contains checks   |
| Stress Test     | 172       | 100,000 | Mixed operations  |
| Hash Collisions | 3,468     | 10,000  | Forced collisions |

> [!TIP]
> Use bulk operations (AddRange, RemoveRange) for better performance when working with multiple items.

## 📥 Installation

### Method 1: Lazarus package

1. Clone or download this repository.
2. In Lazarus, open `package/lazarus/ThreadSafeCollections.lpk`.
3. Click **Compile**.
4. Open your project, then use **Project → Project Inspector → Add → New Requirement** and select `ThreadSafeCollections`.

You can then add the units you need in a `uses` clause, for example:

```pascal
uses
  ThreadSafeCollections.List,
  ThreadSafeCollections.Dictionary;
```

### Method 2: Using Git and FPC

1. Clone the repository:
   ```bash
   git clone https://github.com/ikelaiah/ThreadSafeCollections-FP.git
   ```

2. Add to Your Project:
   - In Lazarus IDE:
     1. Project → Project Inspector
     2. Add Unit → Browse to `src` directory
     3. Select needed units (e.g., ThreadSafeCollections.List.pas)

   - In FPC Project:
     ```pascal
     {$UNITPATH your/path/to/ThreadSafeCollections-FP/src}
     ```

   Or compile with `fpc` by adding the source path:
   ```bash
   fpc -Fu/path/to/ThreadSafeCollections-FP/src yourprogram.pas
   ```

### Method 3: Manual Installation

1. Download ZIP from GitHub
2. Extract to your preferred location
3. Add `src` directory to your project's search path:
   ```pascal
   program YourProject;
   
   {$mode objfpc}{$H+}{$J-}
   {$UNITPATH path/to/ThreadSafeCollections-FP/src}
   
   uses
     ThreadSafeCollections.List,  // For List
     ThreadSafeCollections.Deque, // For Deque
     // ... other units as needed
   ```

### Verify Installation

Create a simple test program:
```pascal
program TestInstall;

{$mode objfpc}{$H+}{$J-}

uses
  ThreadSafeCollections.List;

var
  List: specialize TThreadSafeList<Integer>;
begin
  List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    List.Add(42);
    WriteLn('Installation successful!');
  finally
    List.Free;
  end;
end.
```

### Troubleshooting

1. **Compilation Errors**:
   - Ensure FPC 3.2.2 or later
   - Check unit path is correct
   - Verify all required files are present

2. **Runtime Errors**:
   - Check memory management (use try-finally)
   - Verify comparers are provided where needed

## 🧪 Thread Safety Examples

### Safe Iteration
```pascal
var
  List: specialize TThreadSafeList<Integer>;
  Item: Integer;
begin
  List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    // Iterator automatically acquires lock through RAII
    for Item in List do
    begin
      // Other threads wait until iteration completes
      WriteLn(Item);
    end; // Lock automatically released here
  finally
    List.Free;
  end;
end;
```

### Concurrent Access
```pascal
// Thread 1
procedure Thread1;
begin
  ThreadSafeList.Add(42);  // Automatically locked
end;

// Thread 2
procedure Thread2;
begin
  if ThreadSafeList.Contains(42) then  // Automatically locked
    WriteLn('Found it!');
end;
```

### Safe Resource Management
```pascal
var
  Dict: specialize TThreadSafeDictionary<string, integer>;
begin
  Dict := specialize TThreadSafeDictionary<string, integer>.Create;
  try
    // Multiple threads can safely access
    Dict.Add('one', 1);    // Thread 1
    Dict.Add('two', 2);    // Thread 2
    Dict.Remove('one');    // Thread 3
    
    // Snapshot-based iteration (v0.8.2): lock released immediately after copy;
    // other threads may modify the dictionary during the loop
    for Pair in Dict do
      WriteLn(Pair.Key, ': ', Pair.Value);
  finally
    Dict.Free;
  end;
end;
```

## ✨ Features

- 🛡️ Thread-safe List, Deque, Dictionary and HashSet implementations
- 🚀 Generic type support (Integer, String, Real, Boolean, Records)
- 📦 Built-in comparers and hash functions
- 🔐 Automatic locking mechanism with `TCriticalSection`
- 🎯 Exception-safe resource management
- 🧪 Comprehensive test suite with collision testing
- ⚡ Optimized performance for common operations
- 📊 Load factor based automatic resizing

## 🔄 Feature Comparison

| Feature                   | List | Deque | Dictionary       | HashSet |
|---------------------------|------|-------|------------------|---------|
| Thread-Safe Operations    |  ✅  |  ✅   |       ✅         |   ✅    |
| Iterator Locking          |  ✅  |  ✅   | Snapshot (v0.8.2)|   ✅    |
| Automatic Resizing        |  ✅  |  ✅   |       ✅         |   ✅    |
| Collision Resolution      |  N/A |  N/A  |       ✅         |   ✅    |
| Specialized Types         |  ✅  |  ❌   |       ❌         |   ✅    |
| Custom Comparers          |  ✅  |  ❌   |       ✅         |   ✅    |
| Bulk Operations           |  ✅  |  ✅   |       ✅         |   ✅    |
| Set Operations            |  N/A |  N/A  |      N/A         |   ✅    |

## 🧪 Testing

1. Go to `tests/` directory
2. Open `TestRunner.lpi` in Lazarus IDE and compile
3. Run `./TestRunner.exe -a -p --format=plain` to see the test results.

## 📚 Documentation

- [ThreadSafeCollections.List.md](docs/ThreadSafeCollections.List.md)
- [ThreadSafeCollections.Deque.md](docs/ThreadSafeCollections.Deque.md)
- [ThreadSafeCollections.Dictionary.md](docs/ThreadSafeCollections.Dictionary.md)
- [ThreadSafeCollections.HashSet.md](docs/ThreadSafeCollections.HashSet.md)
- [RAII-style locking through interface counting](docs/RAII-style-locking-through-interface-counting.md)
- [Generated Cheat Sheet](docs/CHEATSHEET.md)
- [Latest Test Output](tests/LatestTestOutput.md)

## 📁 Examples

- [SimpleNumberList](examples/SimpleNumberList/SimpleNumberList.lpr) - Shows basic operations in `TThreadSafeList`; Add, Remove, Sort with the built-in integer comparer.
- [SimpleShoppingCart](examples/SimpleShoppingCart/SimpleShoppingCart.lpr) - Shows how to use `TThreadSafeList` with a custom type and a custom comparer.
- [SimpleToDoList](examples/SimpleToDoList/SimpleToDoList.lpr) - Shows how to use `TThreadSafeList` with the built-in string comparer.   
- [ChatMessageQueue](examples/ChatMessageQueue/ChatMessageQueue.lpr) - Demonstrates using `TThreadSafeList` for a multi-threaded chat system.
- [DictionaryIterator](examples/DictionaryIterator/DictionaryIterator.lpr) - Demonstrates using `TThreadSafeDictionary` with an iterator.
- [DictionaryWithCustomType](examples/DictionaryWithCustomType/DictionaryWithCustomType.lpr) - Demonstrates using `TThreadSafeDictionary` with a custom key type, hash function, and equality function.
- [SimpleHashSet](examples/SimpleHashSet/SimpleHashSet.lpr) - Demonstrates using `TThreadSafeHashSet` with the built-in integer comparer.
- [HashSetClientDemo](examples/HashSetClientDemo/HashSetClientDemo.lpr) - Demonstrates using `TThreadSafeHashSet` with a custom type, hash function, and equality function.
- [SimpleDeque](examples/SimpleDeque/SimpleDeque.lpr) - Demonstrates using `TThreadSafeDeque` with basic push/pop operations.
- [DequeWithCustomType](examples/DequeWithCustomType/DequeWithCustomType.lpr) - Demonstrates using `TThreadSafeDeque` with a custom type.
- [Benchmark](examples/Benchmark/Benchmark.lpr) - Microsecond-precision benchmark suite covering all four collections at 1k, 10k, 100k and 1M items. Supports `--affinity` flag to pin the timing thread to CPU core 0 for stable measurements.

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## 👏 Acknowledgments

- 🎯 Free Pascal and Lazarus community
- 🧪 FPCUnit testing framework



