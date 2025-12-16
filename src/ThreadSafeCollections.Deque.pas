unit ThreadSafeCollections.Deque;

{$mode ObjFPC}{$H+}
{$modeswitch ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, SyncObjs, ThreadSafeCollections.Interfaces;

type

  {
    TThreadSafeDeque<T> is a generic, thread-safe double-ended queue (deque) implementation.

    v0.8 PERFORMANCE IMPROVEMENTS:
    - Converted from linked-list to circular array-based implementation
    - Dramatically improved cache locality and reduced memory fragmentation
    - Eliminated per-item memory allocations (New/Dispose)
    - Added capacity pre-allocation to reduce resizes
    - Optimized for both LIFO and FIFO access patterns

    It allows multiple threads to concurrently add or remove items from both the front and back
    of the queue without causing data corruption or race conditions.
  }
  generic TThreadSafeDeque<T> = class(TInterfacedObject, specialize IThreadSafeDeque<T>)
  private
    FLock: TCriticalSection;     // Synchronization object to ensure that deque operations are thread-safe
    FBuffer: array of T;          // v0.8: Circular array buffer for storing elements
    FHead: Integer;               // v0.8: Index of the first element (front)
    FTail: Integer;               // v0.8: Index where next element will be added at back
    FCount: Integer;              // Tracks the current number of elements in the deque
    FCapacity: Integer;           // v0.8: Current capacity of the circular buffer

    {
      Resizes the internal buffer when capacity is reached.
      Maintains the logical order of elements during resize.
    }
    procedure Grow;

    {
      Retrieves the current number of elements in the deque in a thread-safe manner.
      Utilizes the critical section to prevent race conditions during access.
    }
    function GetCount: Integer;

  public
    type
      {
        TEnumerator provides the functionality to iterate over the elements of the deque.
        It ensures that the deque remains in a consistent state during enumeration by locking it.
      }
      TEnumerator = class
      private
        FDeque: TThreadSafeDeque;     // Reference to the deque being enumerated
        FCurrentIndex: Integer;       // v0.8: Current index in the iteration
        FCurrent: T;                  // Holds the current value of type T during iteration
        FLockToken: ILockToken;       // Manages the lock to ensure thread safety throughout enumeration

        {
          Retrieves the current element in the enumeration.
        }
        function GetCurrent: T;
      public
        {
          Initializes the enumerator with a reference to the deque.
          Acquires a lock to ensure the deque remains unchanged during iteration.
        }
        constructor Create(ADeque: TThreadSafeDeque);

        {
          Cleans up resources used by the enumerator.
          Ensures that the lock is released when the enumerator is destroyed.
        }
        destructor Destroy; override;

        {
          Advances the enumerator to the next element in the deque.
        }
        function MoveNext: Boolean;

        {
          Provides read-only access to the current element in the enumeration.
        }
        property Current: T read GetCurrent;
      end;

    {
      Constructs a new instance of TThreadSafeDeque with default capacity.
    }
    constructor Create;

    {
      v0.8: Constructs a new instance with specified initial capacity.
      Parameters:
        AInitialCapacity: Initial size of the circular buffer (will be power of 2)
    }
    constructor Create(AInitialCapacity: Integer);

    {
      Destroys the deque instance.
      Ensures that all resources are properly freed.
    }
    destructor Destroy; override;

    {
      Adds an item to the front of the deque.
      This operation is thread-safe and can be performed concurrently by multiple threads.
    }
    procedure PushFront(const AItem: T);

    {
      Adds an item to the back of the deque.
      This operation is thread-safe and can be performed concurrently by multiple threads.
    }
    procedure PushBack(const AItem: T);

    {
      Removes and returns the item at the front of the deque.
      Ensures thread safety by locking the deque during the operation.
    }
    function PopFront: T;

    {
      Attempts to remove the item from the front of the deque without raising an exception if the deque is empty.
    }
    function TryPopFront(out AValue: T): Boolean;

    {
      Removes and returns the item at the back of the deque.
      Ensures thread safety by locking the deque during the operation.
    }
    function PopBack: T;

    {
      Attempts to remove the item from the back of the deque without raising an exception if the deque is empty.
    }
    function TryPopBack(out AValue: T): Boolean;

    {
      Retrieves the item at the front of the deque without removing it.
      Ensures thread safety by locking the deque during the operation.
    }
    function PeekFront: T;

    {
      Attempts to retrieve the item at the front of the deque without removing it and without raising an exception.
    }
    function TryPeekFront(out AValue: T): Boolean;

    {
      Retrieves the item at the back of the deque without removing it.
      Ensures thread safety by locking the deque during the operation.
    }
    function PeekBack: T;

    {
      Attempts to retrieve the item at the back of the deque without removing it and without raising an exception.
    }
    function TryPeekBack(out AValue: T): Boolean;

    {
      Removes all elements from the deque, effectively resetting it to an empty state.
      Ensures thread safety by locking the deque during the operation.
    }
    procedure Clear;

    {
      Provides read-only access to the number of elements currently in the deque.
      This property is thread-safe and reflects the real-time count of elements.
    }
    property Count: Integer read GetCount;

    {
      Creates and returns an enumerator for iterating over the deque's elements.
      The enumerator maintains thread safety by locking the deque during enumeration.
    }
    function GetEnumerator: TEnumerator;

    {
      Acquires a read lock on the deque to prevent other threads from modifying it.
      This is useful for performing multiple read operations atomically.
      The returned ILockToken automatically releases the lock when it goes out of scope,
      ensuring that locks are properly managed even if exceptions occur.
    }
    function Lock: ILockToken;

    {
      Checks whether the deque contains any elements.
    }
    function IsEmpty: Boolean;

    {
      Converts all elements of the deque into a dynamic array.
      This is useful for scenarios where array-based operations or interoperability is required.
      Ensures thread safety by locking the deque during the conversion process.
    }
    function ToArray: specialize TArray<T>;

    {
      Copies the elements of the deque into a provided array starting at a specified index.
      Ensures that the destination array has sufficient capacity to hold the copied elements.
    }
    procedure CopyTo(var AArray: array of T; AStartIndex: Integer = 0);

    {
      Adds a range of items to the back of the deque in a single operation.
      This method enhances performance by reducing the overhead of multiple individual push operations.
    }
    procedure PushRangeBack(const AItems: array of T);

    {
      Adds a range of items to the front of the deque in a single operation.
      This method enhances performance by reducing the overhead of multiple individual push operations.
    }
    procedure PushRangeFront(const AItems: array of T);
  end;

implementation

{ TThreadSafeDeque }

constructor TThreadSafeDeque.Create;
begin
  Create(16);  // v0.8: Default to reasonable initial capacity
end;

constructor TThreadSafeDeque.Create(AInitialCapacity: Integer);
var
  PowerOfTwo: Integer;
begin
  inherited Create;
  FLock := TCriticalSection.Create;

  // v0.8: Ensure capacity is power of 2 for efficient modulo operations
  PowerOfTwo := 4;
  while PowerOfTwo < AInitialCapacity do
    PowerOfTwo := PowerOfTwo * 2;

  FCapacity := PowerOfTwo;
  SetLength(FBuffer, FCapacity);
  FHead := 0;
  FTail := 0;
  FCount := 0;
end;

destructor TThreadSafeDeque.Destroy;
begin
  Clear;
  FLock.Free;
  inherited Destroy;
end;

procedure TThreadSafeDeque.Grow;
var
  NewCapacity, I, Idx: Integer;
  NewBuffer: array of T;
begin
  // v0.8: Double the capacity
  NewCapacity := FCapacity * 2;
  SetLength(NewBuffer, NewCapacity);

  // Copy elements in order from old buffer to new buffer
  for I := 0 to FCount - 1 do
  begin
    Idx := (FHead + I) and (FCapacity - 1);  // Efficient modulo for power of 2
    NewBuffer[I] := FBuffer[Idx];
  end;

  FBuffer := NewBuffer;
  FCapacity := NewCapacity;
  FHead := 0;
  FTail := FCount;
end;

procedure TThreadSafeDeque.PushFront(const AItem: T);
begin
  FLock.Acquire;
  try
    if FCount = FCapacity then
      Grow;

    // v0.8: Decrement head with wraparound
    FHead := (FHead - 1 + FCapacity) and (FCapacity - 1);
    FBuffer[FHead] := AItem;
    Inc(FCount);
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeDeque.PushBack(const AItem: T);
begin
  FLock.Acquire;
  try
    if FCount = FCapacity then
      Grow;

    // v0.8: Add at tail and increment with wraparound
    FBuffer[FTail] := AItem;
    FTail := (FTail + 1) and (FCapacity - 1);
    Inc(FCount);
  finally
    FLock.Release;
  end;
end;

function TThreadSafeDeque.TryPopFront(out AValue: T): Boolean;
begin
  Result := False;
  FLock.Acquire;
  try
    if FCount > 0 then
    begin
      // v0.8: Get item at head and increment with wraparound
      AValue := FBuffer[FHead];
      FHead := (FHead + 1) and (FCapacity - 1);
      Dec(FCount);
      Result := True;
    end;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeDeque.PopFront: T;
begin
  if not TryPopFront(Result) then
    raise EListError.Create('Deque is empty');
end;

function TThreadSafeDeque.TryPopBack(out AValue: T): Boolean;
begin
  Result := False;
  FLock.Acquire;
  try
    if FCount > 0 then
    begin
      // v0.8: Decrement tail with wraparound and get item
      FTail := (FTail - 1 + FCapacity) and (FCapacity - 1);
      AValue := FBuffer[FTail];
      Dec(FCount);
      Result := True;
    end;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeDeque.PopBack: T;
begin
  if not TryPopBack(Result) then
    raise EListError.Create('Deque is empty');
end;

function TThreadSafeDeque.TryPeekFront(out AValue: T): Boolean;
begin
  Result := False;
  FLock.Acquire;
  try
    if FCount > 0 then
    begin
      AValue := FBuffer[FHead];
      Result := True;
    end;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeDeque.PeekFront: T;
begin
  if not TryPeekFront(Result) then
    raise EListError.Create('Deque is empty');
end;

function TThreadSafeDeque.TryPeekBack(out AValue: T): Boolean;
var
  BackIdx: Integer;
begin
  Result := False;
  FLock.Acquire;
  try
    if FCount > 0 then
    begin
      BackIdx := (FTail - 1 + FCapacity) and (FCapacity - 1);
      AValue := FBuffer[BackIdx];
      Result := True;
    end;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeDeque.PeekBack: T;
begin
  if not TryPeekBack(Result) then
    raise EListError.Create('Deque is empty');
end;

procedure TThreadSafeDeque.Clear;
begin
  FLock.Acquire;
  try
    // v0.8: Simply reset indices - no need to free individual nodes
    FHead := 0;
    FTail := 0;
    FCount := 0;
    // Clear buffer to release any references (for managed types)
    FillChar(FBuffer[0], FCapacity * SizeOf(T), 0);
  finally
    FLock.Release;
  end;
end;

{ TThreadSafeDeque.TEnumerator }

constructor TThreadSafeDeque.TEnumerator.Create(ADeque: TThreadSafeDeque);
begin
  inherited Create;
  FDeque := ADeque;
  FLockToken := FDeque.Lock;
  FCurrentIndex := -1;
end;

destructor TThreadSafeDeque.TEnumerator.Destroy;
begin
  FLockToken := nil;
  inherited;
end;

function TThreadSafeDeque.TEnumerator.MoveNext: Boolean;
var
  Idx: Integer;
begin
  Inc(FCurrentIndex);
  Result := FCurrentIndex < FDeque.FCount;

  if Result then
  begin
    // v0.8: Calculate actual buffer index using circular array logic
    Idx := (FDeque.FHead + FCurrentIndex) and (FDeque.FCapacity - 1);
    FCurrent := FDeque.FBuffer[Idx];
  end;
end;

function TThreadSafeDeque.TEnumerator.GetCurrent: T;
begin
  if FCurrentIndex < 0 then
    raise Exception.Create('Invalid enumerator position');
  Result := FCurrent;
end;

function TThreadSafeDeque.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TThreadSafeDeque.Lock: ILockToken;
begin
  Result := TLockToken.Create(FLock);
end;

function TThreadSafeDeque.IsEmpty: Boolean;
begin
  FLock.Acquire;
  try
    Result := FCount = 0;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeDeque.ToArray: specialize TArray<T>;
var
  I, Idx: Integer;
begin
  FLock.Acquire;
  try
    SetLength(Result, FCount);
    for I := 0 to FCount - 1 do
    begin
      Idx := (FHead + I) and (FCapacity - 1);
      Result[I] := FBuffer[Idx];
    end;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeDeque.CopyTo(var AArray: array of T; AStartIndex: Integer = 0);
var
  I, Idx: Integer;
begin
  if AStartIndex < 0 then
    raise EArgumentOutOfRangeException.Create('AStartIndex must be non-negative');

  FLock.Acquire;
  try
    if Length(AArray) - AStartIndex < FCount then
      raise EArgumentException.Create('Destination array is too small');

    for I := 0 to FCount - 1 do
    begin
      Idx := (FHead + I) and (FCapacity - 1);
      AArray[AStartIndex + I] := FBuffer[Idx];
    end;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeDeque.PushRangeBack(const AItems: array of T);
var
  I, RequiredCapacity: Integer;
begin
  if Length(AItems) = 0 then
    Exit;

  FLock.Acquire;
  try
    // v0.8: Pre-allocate capacity if needed
    if FCount + Length(AItems) > FCapacity then
    begin
      RequiredCapacity := FCapacity;
      while RequiredCapacity < FCount + Length(AItems) do
        RequiredCapacity := RequiredCapacity * 2;

      // Resize to required capacity
      while FCapacity < RequiredCapacity do
        Grow;
    end;

    // Add items efficiently
    for I := Low(AItems) to High(AItems) do
    begin
      FBuffer[FTail] := AItems[I];
      FTail := (FTail + 1) and (FCapacity - 1);
      Inc(FCount);
    end;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeDeque.PushRangeFront(const AItems: array of T);
var
  I, RequiredCapacity: Integer;
begin
  if Length(AItems) = 0 then
    Exit;

  FLock.Acquire;
  try
    // v0.8: Pre-allocate capacity if needed
    if FCount + Length(AItems) > FCapacity then
    begin
      RequiredCapacity := FCapacity;
      while RequiredCapacity < FCount + Length(AItems) do
        RequiredCapacity := RequiredCapacity * 2;

      // Resize to required capacity
      while FCapacity < RequiredCapacity do
        Grow;
    end;

    // Push in forward order - each push makes the new item the front element
    for I := Low(AItems) to High(AItems) do
    begin
      FHead := (FHead - 1 + FCapacity) and (FCapacity - 1);
      FBuffer[FHead] := AItems[I];
      Inc(FCount);
    end;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeDeque.GetCount: Integer;
begin
  FLock.Acquire;
  try
    Result := FCount;
  finally
    FLock.Release;
  end;
end;

end.
