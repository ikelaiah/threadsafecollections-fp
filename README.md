# 🔒 ThreadSafeCollections-FP

A thread-safe generic collections library for Free Pascal.

> [!Note]This library is still under development. 
> If you are after more mature and production-ready library, consider using:
> 
> 1. [FPC Generics.Collections unit](https://gitlab.com/freepascal.org/fpc/source/-/blob/main/packages/rtl-generics/src/generics.collections.pas)
> 2. [LGenerics.Collections unit](https://github.com/avk959/LGenerics)

## ✨ Features

- 🛡️ Thread-safe List implementation
- 🚀 Generic type support (Integer, String, Real, Boolean, Records)
- 🔍 Custom comparers for sorting
- 🔐 Automatic locking mechanism
- 🧪 Comprehensive test suite
- ⚡ Exception-safe operations

## 🎯 Why Use This?

- 🔒 Safe concurrent access to lists from multiple threads
- 🚀 Fast sorting with custom comparers
- 💡 Simple to use - just like regular lists, but thread-safe
- ⚡ Perfect for multi-threaded applications

## 🚀 Quick Start

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

## 🏗️ Quick Start with Custom Types

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

2. 🔄 **Iterating While Modifying**
```pascal
// ❌ Wrong: List might change during iteration
for I := 0 to List.Count - 1 do
begin
  // Other thread might modify list here!
  DoSomething(List[I]);
end;

// ✅ Correct: Use a local copy for iteration
var
  LocalCopy: TArray<Integer>;
begin
  LocalCopy := List.ToArray;  // Thread-safe copy
  for I := 0 to High(LocalCopy) do
    DoSomething(LocalCopy[I]);
end;
```

3. 🏃 **Unnecessary Locking**
```pascal
// ❌ Wrong: Excessive locking
for I := 0 to List.Count - 1 do
  WriteLn(List[I]);  // Each access locks/unlocks

// ✅ Correct: Single lock for bulk operations
var
  Values: TArray<Integer>;
begin
  Values := List.ToArray;  // Single lock operation
  for I := 0 to High(Values) do
    WriteLn(Values[I]);
end;
```

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

See [ThreadSafeCollections.List-API.md](docs/ThreadSafeCollections.List-API.md) for more details.   

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