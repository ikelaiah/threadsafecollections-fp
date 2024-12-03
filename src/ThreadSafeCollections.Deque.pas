unit ThreadSafeCollections.Deque;

{$mode ObjFPC}{$H+}
{$modeswitch ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, SyncObjs, ThreadSafeCollections.Interfaces;

type
  
  {
    TThreadSafeDeque<T> is a generic, thread-safe double-ended queue (deque) implementation.
    It allows multiple threads to concurrently add or remove items from both the front and back
    of the queue without causing data corruption or race conditions.
  }
  generic TThreadSafeDeque<T> = class(TInterfacedObject, specialize IThreadSafeDeque<T>)
  private
    type
      {
        TDequeNode represents a single node within the deque.
        Each node holds the data of type T and pointers to both the next and previous nodes,
        facilitating efficient insertion and removal from either end of the deque.
      }
      TDequeNode = record
        Data: T;                     // The data stored in the deque node.
        Next, Prev: ^TDequeNode;     // Pointers to the next and previous nodes in the deque.
      end;

      {
        PNode is a pointer type to a TDequeNode.
        It simplifies node manipulations by providing a direct reference to deque nodes.
      }
      PNode = ^TDequeNode;
  private
    FLock: TCriticalSection; // Synchronization object to ensure that deque operations are thread-safe.
    FHead: PNode;            // Pointer to the first node (front) of the deque.
    FTail: PNode;            // Pointer to the last node (back) of the deque.
    FCount: Integer;         // Tracks the current number of elements in the deque.

    {
      Frees the memory allocated for a deque node.
      This is essential for preventing memory leaks when nodes are removed from the deque.
    }
    procedure FreeNode(ANode: PNode);

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
        FDeque: TThreadSafeDeque;     // Reference to the deque being enumerated.
        FCurrentNode: PNode;          // Pointer to the current node in the iteration.
        FCurrent: T;                  // Holds the current value of type T during iteration.
        FLockToken: ILockToken;       // Manages the lock to ensure thread safety throughout enumeration.

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
      Constructs a new instance of TThreadSafeDeque.
      Initializes the critical section and sets the head and tail pointers to nil,
      indicating that the deque is initially empty.
    }
    constructor Create;

    {
      Destroys the deque instance.
      Ensures that all nodes are properly freed to prevent memory leaks
      and releases the critical section resource.
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
    procedure AddRange(const AItems: array of T);

    {
      Adds a range of items to the front of the deque in a single operation.
      This method enhances performance by reducing the overhead of multiple individual push operations.
    }
    procedure InsertRange(const AItems: array of T);
  end;

implementation

{ TThreadSafeDeque }

constructor TThreadSafeDeque.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FHead := nil;
  FTail := nil;
  FCount := 0;
end;

destructor TThreadSafeDeque.Destroy;
begin
  Clear;
  FLock.Free;
  inherited Destroy;
end;

procedure TThreadSafeDeque.FreeNode(ANode: PNode);
begin
  if Assigned(ANode) then
    Dispose(ANode);
end;

procedure TThreadSafeDeque.PushFront(const AItem: T);
var
  NewNode: PNode;
begin
  New(NewNode);
  NewNode^.Data := AItem;
  NewNode^.Prev := nil;
  
  FLock.Acquire;
  try
    NewNode^.Next := FHead;
    if FHead = nil then
    begin
      FHead := NewNode;
      FTail := NewNode;
    end
    else
    begin
      FHead^.Prev := NewNode;
      FHead := NewNode;
    end;
    Inc(FCount);
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeDeque.PushBack(const AItem: T);
var
  NewNode: PNode;
begin
  New(NewNode);
  NewNode^.Data := AItem;
  NewNode^.Next := nil;
  
  FLock.Acquire;
  try
    NewNode^.Prev := FTail;
    if FTail = nil then
    begin
      FHead := NewNode;
      FTail := NewNode;
    end
    else
    begin
      FTail^.Next := NewNode;
      FTail := NewNode;
    end;
    Inc(FCount);
  finally
    FLock.Release;
  end;
end;

function TThreadSafeDeque.TryPopFront(out AValue: T): Boolean;
var
  OldHead: PNode;
begin
  Result := False;
  FLock.Acquire;
  try
    if FHead <> nil then
    begin
      AValue := FHead^.Data;
      OldHead := FHead;
      FHead := FHead^.Next;
      if FHead = nil then
        FTail := nil
      else
        FHead^.Prev := nil;
      Dec(FCount);
      FreeNode(OldHead);
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
var
  OldTail: PNode;
begin
  Result := False;
  FLock.Acquire;
  try
    if FTail <> nil then
    begin
      AValue := FTail^.Data;
      OldTail := FTail;
      FTail := FTail^.Prev;
      if FTail = nil then
        FHead := nil
      else
        FTail^.Next := nil;
      Dec(FCount);
      FreeNode(OldTail);
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
    if FHead <> nil then
    begin
      AValue := FHead^.Data;
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
begin
  Result := False;
  FLock.Acquire;
  try
    if FTail <> nil then
    begin
      AValue := FTail^.Data;
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
var
  Current, Next: PNode;
begin
  FLock.Acquire;
  try
    Current := FHead;
    while Current <> nil do
    begin
      Next := Current^.Next;
      FreeNode(Current);
      Current := Next;
    end;
    FHead := nil;
    FTail := nil;
    FCount := 0;
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
  FCurrentNode := nil;
end;

destructor TThreadSafeDeque.TEnumerator.Destroy;
begin
  FLockToken := nil;
  inherited;
end;

function TThreadSafeDeque.TEnumerator.MoveNext: Boolean;
begin
  if FCurrentNode = nil then
    FCurrentNode := FDeque.FHead
  else
    FCurrentNode := FCurrentNode^.Next;
    
  Result := FCurrentNode <> nil;
  if Result then
    FCurrent := FCurrentNode^.Data;
end;

function TThreadSafeDeque.TEnumerator.GetCurrent: T;
begin
  if FCurrentNode = nil then
    raise Exception.Create('Invalid enumerator position');
  Result := FCurrentNode^.Data;
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
  Current: PNode;
  I: Integer;
begin
  FLock.Acquire;
  try
    SetLength(Result, FCount);
    Current := FHead;
    I := 0;
    while Current <> nil do
    begin
      Result[I] := Current^.Data;
      Current := Current^.Next;
      Inc(I);
    end;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeDeque.CopyTo(var AArray: array of T; AStartIndex: Integer = 0);
var
  Current: PNode;
  I: Integer;
begin
  if AStartIndex < 0 then
    raise EArgumentOutOfRangeException.Create('AStartIndex must be non-negative');
    
  FLock.Acquire;
  try
    if Length(AArray) - AStartIndex < FCount then
      raise EArgumentException.Create('Destination array is too small');
      
    Current := FHead;
    I := AStartIndex;
    while Current <> nil do
    begin
      AArray[I] := Current^.Data;
      Current := Current^.Next;
      Inc(I);
    end;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeDeque.AddRange(const AItems: array of T);
var
  I: Integer;
begin
  for I := Low(AItems) to High(AItems) do
    PushBack(AItems[I]);
end;

procedure TThreadSafeDeque.InsertRange(const AItems: array of T);
var
  I: Integer;
begin
  for I := Low(AItems) to High(AItems) do
    PushFront(AItems[I]);
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
