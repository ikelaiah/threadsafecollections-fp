# 🔒 ThreadSafeCollections-FP

A thread-safe generic collections library for Free Pascal, designed for learning and experimentation.

> [!IMPORTANT] 
> 🚧 **Development Status**: This library is a learning/experimental project and not yet production-ready.
> 
> For production use, please consider these mature alternatives:
> 
> 1. [FPC Generics.Collections](https://gitlab.com/freepascal.org/fpc/source/-/blob/main/packages/rtl-generics/src/generics.collections.pas) - Official FPC generic collections
> 2. [FCL-STL](https://gitlab.com/freepascal.org/fpc/source/-/tree/main/packages/fcl-stl) - FPC's template library
> 3. [LGenerics](https://github.com/avk959/LGenerics) - Comprehensive generics library

## 🚧 Development Status

Current State:

- ✅ Basic operations working (Add, Remove, GetItem)
- ✅ Thread safety verified through testing
- ✅ Memory management stable
- ✅ Thread-Safe Iterator Support
   - Iterators use RAII-style locking through interface counting
   - Thread-safe iteration with automatic lock management
   - Each iterator maintains its own lock token
- ✅ Bulk operations support
- ❌ Performance not yet optimized

Planned Features:

- 🔄 Performance optimizations
- 🔄 More specialized types

## 🎯 Why Use This?

- 💡 **Learning Tool**: Perfect for understanding thread-safe collections
- 🔒 **Simple Thread Safety**: Just like regular collections, but thread-safe
- 🚀 **Easy to Use**: Specialized types for common data (Integer, String, Boolean, Real)
- ⚡ **Good for Prototypes**: Ideal for quick multi-threaded demos

## 🎓 Getting Started

This library provides four main collection types:

1. **ThreadSafeList**: Like an array that can grow
```pascal
uses 
  ThreadSafeCollections.List;  // Built-in comparers included!

var
  List: specialize TThreadSafeList<Integer>;
begin
  // Two ways to create a list:
  
  // 1. Basic creation (using built-in comparers)
  List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);    // For integers
  // List := specialize TThreadSafeList<string>.Create(@StringComparer);   // For strings
  // List := specialize TThreadSafeList<Boolean>.Create(@BooleanComparer); // For booleans
  // List := specialize TThreadSafeList<Real>.Create(@RealComparer);       // For reals
  
  // 2. With initial capacity (for better performance)
  List := specialize TThreadSafeList<Integer>.Create(1000, @IntegerComparer);
  
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

2. **ThreadSafeDeque**: A double-ended queue
```pascal
var
  Deque: specialize TThreadSafeDeque<Integer>;
begin
  Deque := specialize TThreadSafeDeque<Integer>.Create;
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
    
    if Dict.Contains('one') then
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
begin
  UniquePoints := specialize TThreadSafeHashSet<TPoint>.Create(@PointEquals, @PointHash);
  try
    // Add unique points
    UniquePoints.Add(TPoint.Create(1, 1));
    UniquePoints.Add(TPoint.Create(2, 2));
    
    // Check for existence
    var Point := TPoint.Create(1, 1);
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
    var Numbers: array of Integer;
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

Recent test results show excellent performance for the HashSet implementation:

| Operation | Time (ms) | Items | Notes |
|-----------|-----------|-------|-------|
| Basic Ops | 0.006 | 10,000 | Add/Contains |
| Bulk Add | 0.032 | 100,000 | AddRange |
| Set Ops | < 0.001 | 1,000 | Union/Intersect |

> [!TIP]
> Use bulk operations (AddRange, RemoveRange) for better performance when working with multiple items.

## 📥 Installation

### Method 1: Using Git

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

### Method 2: Manual Installation

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
    
    // Safe iteration with RAII locking
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

| Feature                   | List | Deque | Dictionary  | HashSet |
|--------------------------|------|-------|-------------|---------|
| Thread-Safe Operations   |  ✅  |  ✅   |    ✅      |   ✅    |
| RAII Iterator Locking    |  ✅  |  ✅   |    ✅      |   ✅    |
| Automatic Resizing       |  ✅  |  ✅   |    ✅      |   ✅    |
| Collision Resolution     |  N/A |  N/A  |    ✅      |   ✅    |
| Specialized Types        |  ✅  |  ❌   |    ❌      |   ✅    |
| Custom Comparers         |  ✅  |  ❌   |    ✅      |   ✅    |
| Bulk Operations          |  ✅  |  ✅   |    ✅      |   ✅    |
| Set Operations          |  N/A |  N/A  |    N/A     |   ✅    |

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
- [Latest Test Output](docs/Latest-Test-Output.md)

## 📁 Examples

- [SimpleNumberList](examples/SimpleNumberList/SimpleNumberList.lpr) - Shows basic operations like Add, Remove, Sort with the built-in integer comparer.
- [SimpleShoppingCart](examples/SimpleShoppingCart/SimpleShoppingCart.lpr) - Shows how to use TThreadSafeList with a custom type and a custom comparer.
- [SimpleToDoList](examples/SimpleToDoList/SimpleToDoList.lpr) - Shows how to use TThreadSafeList with the built-in string comparer.   
- [ChatMessageQueue](examples/ChatMessageQueue/ChatMessageQueue.lpr) - Demonstrates using TThreadSafeList for a multi-threaded chat system.

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



