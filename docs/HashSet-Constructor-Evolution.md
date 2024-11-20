# Making Generic HashSet More User-Friendly in Free Pascal

## The Challenge

We wanted to make a generic `TThreadSafeHashSet<T>` that would be easy to use with common types (Integer, String, Boolean, Real) while maintaining type safety and proper generic support.

## Evolution of the Solution

### 1. First Attempt: Generic Constructor with RTTI

We tried using RTTI (Run Time Type Information) to automatically select the right comparers:

```pascal   
constructor TThreadSafeHashSet.Create;
var
  TypeData: PTypeInfo;
begin
  TypeData := TypeInfo(T);                  // Get the type info for T - WORKS
  case TypeData^.Kind of                    // Check the type kind - WORKS
    tkInteger:                              // If it's an integer - WORKS
      Create(@IntegerEquals, @IntegerHash); // Use the integer comparer and hash function - DID NOT WORK!
    tkString:                               
      Create(@StringEquals, @StringHash);
    // ...
  end;
end;
```
Problem: Type compatibility issues with generic function types.

### 2. Second Attempt: Static Class Functions

We tried using static class functions to create specialized instances:


```pascal
type
generic TThreadSafeHashSet<T> = class
class static function CreateInteger: specialize TThreadSafeHashSet<Integer>;
class static function CreateString: specialize TThreadSafeHashSet<string>;
// ...
end;
```

Problem: Static methods were a workaround and didn't follow proper OOP patterns.

### 3. Third Attempt: Type-Specific Constructors

We tried adding constructors directly to the generic class:


```pascal
type
generic TThreadSafeHashSet<T> = class
constructor CreateInteger;
constructor CreateString;
// ...
end;
```

Problem: Can't mix generic and specific types in the same class definition.

### 4. Final Solution: Specialized Derived Classes

The working solution uses derived classes for each specific type:

```pascal
type
  // Base generic class
  generic TThreadSafeHashSet<T> = class
  constructor Create(AEqualityComparer: specialize TEqualityComparer<T>;
  AHashFunction: specialize THashFunction<T>);
  // ...
end;

// Specialized classes with simple constructors
TThreadSafeHashSetInteger = class(specialize TThreadSafeHashSet<Integer>)
  constructor Create; overload;
end;

TThreadSafeHashSetString = class(specialize TThreadSafeHashSet<string>)
  constructor Create; overload;
end;
```

## Why This Works

1. **Type Safety**: Each specialized class is a concrete type with its own constructor
2. **Inheritance**: Proper use of OOP inheritance patterns
3. **Encapsulation**: Hides the complexity of comparer and hash function setup
4. **User-Friendly**: Simple `Create` constructor for common types
5. **Maintainable**: Clear separation between generic and specialized code

## Usage Examples

```pascal
// Simple usage with integers
var
  Numbers: TThreadSafeHashSetInteger;

begin
  Numbers := TThreadSafeHashSetInteger.Create;
  try
    Numbers.Add(42);
    // ...
  finally
    Numbers.Free;
  end;
end;


// Custom type usage still possible with generic version
type
  TMyRecord = record
    // ...
  end;

var
  CustomSet: specialize TThreadSafeHashSet<TMyRecord>;

begin
  CustomSet := specialize TThreadSafeHashSet<TMyRecord>.Create(@MyRecordEquals, @MyRecordHash);
  try
    // ...
  finally
    CustomSet.Free;
  end;
end;
```

## Key Learnings

1. Free Pascal's generic system requires careful type matching
2. Inheritance can solve problems that generics alone cannot
3. Creating specialized classes is better than type aliases for adding functionality
4. RTTI-based solutions can be problematic with generics
5. Following OOP principles leads to cleaner solutions 👈

## References

- [Free Pascal Generics Documentation](https://www.freepascal.org/docs-html/ref/refch15.html)
- [Free Pascal RTL Source](https://gitlab.com/freepascal.org/fpc/source/-/blob/main/packages/rtl-generics/src/generics.collections.pas)