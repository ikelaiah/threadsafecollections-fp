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
- ✅ Basic operations working (Add, Remove, Find)
- ✅ Thread safety verified through testing
- ✅ Memory management stable
- ❌ No iterator support yet
- ❌ Limited bulk operations
- ❌ Performance not yet optimized

Planned Features:
- 🔄 Iterator support
- 🔄 Bulk operations
- 🔄 Performance optimizations
- 🔄 More specialized types

## 🎯 Why Use This?

- 💡 **Learning Tool**: Perfect for understanding thread-safe collections
- 🔒 **Simple Thread Safety**: Just like regular collections, but thread-safe
- 🚀 **Easy to Use**: Specialized types for common data (Integer, String, Boolean, Real)
- ⚡ **Good for Prototypes**: Ideal for quick multi-threaded demos

## 🎓 Getting Started

This library provides three main collection types:

1. **ThreadSafeList**: Like an array that can grow
   ```pascal
   var List := TThreadSafeList<Integer>.Create(@IntegerComparer);
   List.Add(42);  // That's it!
   ```

2. **ThreadSafeDictionary**: Store key-value pairs
   ```pascal
   var Dict := TThreadSafeDictionary<string,integer>.Create;
   Dict.Add('one', 1);  // Simple!
   ```

3. **ThreadSafeHashSet**: Store unique values
   ```pascal
   var Set := TThreadSafeHashSetString.Create;
   Set.Add('unique');  // Duplicates handled automatically
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

## ✨ Features

- 🛡️ Thread-safe List, Dictionary, and HashSet implementations
- 🚀 Generic type support (Integer, String, Real, Boolean, Records)
- 📦 Built-in comparers and hash functions
- 🔐 Automatic locking mechanism with TCriticalSection
- 🎯 Exception-safe resource management
- 🧪 Comprehensive test suite with collision testing
- ⚡ Optimized performance for common operations
- 📊 Load factor based automatic resizing

## 🎯 Why Use This?

- 🔒 Safe concurrent access from multiple threads
- 🚀 Fast operations with optimized implementations
- 💡 Simple to use - just like regular collections, but thread-safe
- ⚡ Perfect for multi-threaded applications

## 🚀 Quick Start

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
### Using ThreadSafeDictionary

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
    if Dict.TryGetValue('one', Value) then
        WriteLn(Value); // Outputs: 1
    Dict.Remove('two');
  finally
    Dict.Free;
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

## 📚 Collection Types

### TThreadSafeList<T>
- Thread-safe generic list
- Automatic growth and sorting capability
- Built-in comparers for Integer, String, Boolean, Real
- Exception-safe operations
- Index-based access

### TThreadSafeDictionary<TKey, TValue>
- Thread-safe generic dictionary
- Separate chaining for collision resolution
- Automatic resizing (load factor: 0.75)
- First/Last key-value pair access
- Manual bucket count control

### TThreadSafeHashSet<T>
- Thread-safe generic hash set
- Specialized versions for common types (Integer, String, Boolean, Real)
- Custom hash function support for testing
- Automatic resizing at 75% load factor
- Separate chaining for collisions

## ⚠️ Common Pitfalls and Best Practices

> [!WARNING]
> Common mistakes to avoid when using thread-safe lists:

1. 🔓 **Not Using Try-Finally**
```pascal
// ❌ Wrong: Resource leak possible
var
  List: specialize TThreadSafeList<Integer>;
begin
  List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  List.Add(42);  // If exception occurs, List won't be freed
end;

// ✅ Correct: Always use try-finally
begin
  List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    List.Add(42);
  finally
    List.Free;  // List will always be freed
  end;
end;
```

## ⚠️ Known Limitations

1. **No Iterator Support**
   - No built-in mechanism for safe iteration
   - No foreach-style enumeration
   - Must access items individually

2. **Concurrent Access Pattern**
   - Uses single-lock strategy with TCriticalSection
   - All operations are mutually exclusive
   - May have contention under heavy load

3. **Memory Management**
   - Collections only grow, never shrink
   - No manual capacity reduction
   - May hold excess memory after many removals

4. **Bulk Operations**
   - No batch Add/Remove operations
   - Each operation requires separate lock acquisition
   - Consider alternative if bulk operations are critical

## 📥 Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/ikelaiah/ThreadSafeCollections-FP.git
   ```
2. Add the source directory to your project's search path.

## 🧪 Testing

1. Go to `tests/` directory
2. Open `TestRunner.lpi` in Lazarus IDE and compile
3. Run `./TestRunner.exe -a -p --format=plain` to see the test results.

## 📚 Documentation

- [ThreadSafeCollections.List.md](docs/ThreadSafeCollections.List.md)
- [ThreadSafeCollections.Dictionary.md](docs/ThreadSafeCollections.Dictionary.md)
- [ThreadSafeCollections.HashSet.md](docs/ThreadSafeCollections.HashSet.md)

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

