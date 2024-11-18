# 🔒 ThreadSafeCollections-FP

A high-performance, thread-safe generic collections library for Free Pascal.

## ✨ Features

- 🛡️ Thread-safe List implementation
- 🚀 Generic type support (Integer, String, Real, Boolean, Records)
- 🔍 Custom comparers for sorting
- 🔐 Automatic locking mechanism
- 🧪 Comprehensive test suite

## 📥 Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/ikelaiah/ThreadSafeCollections-FP.git
   ```
2. Add the source directory to your project's search path.

## 📚 Usage

### 🔰 Basic List Operations

```pascal
uses ThreadSafeCollections.List;

var
    List: specialize TThreadSafeList<Integer>;
begin
    List := specialize TThreadSafeList<Integer>.Create;
    try
        List.Add(42);
        List.Add(17);
        List.Sort; // Thread-safe sorting
    finally
        List.Free;
    end;
end;
```

### 🏗️ Custom Types

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

## 🧪 Testing

1. Go to `tests/` directory
2. Open `TestRunner.lpi` in Lazarus IDE and compile
3. Run `./TestRunner.exe -a -p --format=plain` to see the test results.

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