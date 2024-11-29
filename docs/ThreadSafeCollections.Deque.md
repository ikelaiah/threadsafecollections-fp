# ThreadSafeCollections.Deque API Documentation

## Overview

A thread-safe double-ended queue (deque) implementation for Free Pascal, supporting generic types and concurrent access.

## Design Decisions

### FPC 3.2.2 Limitations and Solutions

1. **Generic Type Forward Declarations**
   - FPC 3.2.2 does not support forward declarations of generic types
   - Solution: Nested type declarations within the main generic class
   ```pascal
   generic TThreadSafeDeque<T> = class
   private
     type
       TDequeNode = record
         Data: T;
         Next, Prev: ^TDequeNode;
       end;
       PNode = ^TDequeNode;
   ```

2. **Node Structure**
   - Simplified pointer declarations using nested types
   - Avoids complex generic type references
   - Self-contained type definitions

### Thread Safety Features

1. **Critical Section Locking**
   ```pascal
   FLock: TCriticalSection;
   ```
   - All public operations are protected
   - Automatic lock cleanup through try-finally blocks

2. **RAII Lock Pattern**
   ```pascal
   TLockToken = class(TInterfacedObject)
   private
     FLock: TCriticalSection;
   public
     constructor Create(ALock: TCriticalSection);
     destructor Destroy; override;
   end;
   ```
   - Automatic lock release through interface reference counting
   - Used for safe iteration

## Core Components

### Node Structure
```pascal
TDequeNode = record
  Data: T;
  Next, Prev: ^TDequeNode;
end;
```
- Doubly-linked structure
- Generic data storage
- Internal pointer management

### Iterator Support
```pascal
TEnumerator = class
private
  FDeque: TThreadSafeDeque;
  FCurrent: T;
  FCurrentNode: PNode;
public
  constructor Create(AList: TThreadSafeDeque);
  destructor Destroy; override;
  function MoveNext: Boolean;
  property Current: T read FCurrent;
end;
```
- Thread-safe iteration
- Automatic lock management
- Forward-only traversal

## Public Interface

### Basic Operations

```pascal
procedure PushFront(const AItem: T);
procedure PushBack(const AItem: T);
function PopFront: T;
function PopBack: T;
```

### Safe Operations
```pascal
function TryPopFront(out AValue: T): Boolean;
function TryPopBack(out AValue: T): Boolean;
function TryPeekFront(out AValue: T): Boolean;
function TryPeekBack(out AValue: T): Boolean;
```

### Bulk Operations
```pascal
procedure PushRangeBack(const AItems: array of T);
procedure PushRangeFront(const AItems: array of T);
procedure CopyTo(var AArray: array of T; AStartIndex: Integer = 0);
function ToArray: specialize TArray<T>;
```

### Query Operations
```pascal
function IsEmpty: Boolean;
function Contains(const AItem: T): Boolean;
property Count: Integer read FCount;
```

## Thread Safety Guarantees

1. **Operation Atomicity**
   - All public methods are atomic
   - Protected by critical section
   - Exception-safe locking

2. **Iterator Safety**
   - Thread-safe enumeration
   - Holds lock during iteration
   - Automatic lock release

3. **Memory Management**
   - Safe node allocation/deallocation
   - No memory leaks in error conditions
   - Clean cleanup in destructor

## Usage Examples

### Basic Usage
```pascal
var
  Deque: specialize TThreadSafeDeque<Integer>;
begin
  Deque := specialize TThreadSafeDeque<Integer>.Create;
  try
    Deque.PushBack(1);
    Deque.PushFront(2);
    // ... use deque ...
  finally
    Deque.Free;
  end;
end;
```

### Thread-Safe Iteration
```pascal
var
  Item: Integer;
begin
  for Item in Deque do
    WriteLn(Item);
end;
```

### Safe Operations
```pascal
var
  Value: Integer;
begin
  if Deque.TryPopFront(Value) then
    WriteLn('Got value: ', Value)
  else
    WriteLn('Deque is empty');
end;
```

## Performance Considerations

1. **Lock Contention**
   - Single lock for all operations
   - May impact performance under high concurrency
   - Consider operation grouping for better throughput

2. **Memory Allocation**
   - Dynamic node allocation for each item
   - No pre-allocation or pooling
   - Consider bulk operations for better performance

3. **Iterator Performance**
   - Holds lock during entire iteration
   - May block other threads
   - Use ToArray for snapshot iteration

## Known Limitations

1. **No Bulk Remove**
   - Individual removal only
   - No range removal operations
   - Consider clear and rebuild if needed

2. **No Capacity Control**
   - No maximum size limit
   - No memory usage control
   - Monitor usage in memory-constrained environments

3. **Single Lock**
   - All operations mutually exclusive
   - No reader/writer separation
   - May impact concurrent read performance

## Thread Safety Testing

The implementation includes comprehensive multi-threading tests:
- Concurrent push/pop operations
- Multiple threads accessing simultaneously
- Random delays to increase contention
- Data integrity verification
- Sum verification across operations

Example test output:
```
Test took 30819 ms with 4 threads doing 10 iterations each
All 11 tests passed successfully
```

## Version History

- 1.0.0: Initial implementation
  - Basic thread-safe operations
  - Iterator support
  - FPC 3.2.2 compatibility 