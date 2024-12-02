unit ThreadSafeCollections.Deque;

{$mode ObjFPC}{$H+}
{$modeswitch ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, SyncObjs, ThreadSafeCollections.Interfaces;

type
  { TThreadSafeDeque }
  generic TThreadSafeDeque<T> = class(TInterfacedObject, specialize IThreadSafeDeque<T>)
  private
    type
      TDequeNode = record
        Data: T;
        Next, Prev: ^TDequeNode;
      end;
      PNode = ^TDequeNode;
  private
    FLock: TCriticalSection;
    FHead: PNode;
    FTail: PNode;
    FCount: Integer;
    
    procedure FreeNode(ANode: PNode);
    function GetCount: Integer;
  public
    type
      // Enumerator Class
      TEnumerator = class
      private
        FDeque: TThreadSafeDeque;
        FCurrentNode: PNode;
        FCurrent: T;
        FLockToken: ILockToken;
        function GetCurrent: T;
      public
        constructor Create(ADeque: TThreadSafeDeque);
        destructor Destroy; override;
        function MoveNext: Boolean;
        property Current: T read GetCurrent;
      end;

    constructor Create;
    destructor Destroy; override;
    
    // Add elements
    procedure PushFront(const AItem: T);
    procedure PushBack(const AItem: T);
    
    // Remove and return elements
    function PopFront: T;
    function TryPopFront(out AValue: T): Boolean;
    function PopBack: T;
    function TryPopBack(out AValue: T): Boolean;
    
    // Peek without removing
    function PeekFront: T;
    function TryPeekFront(out AValue: T): Boolean;
    function PeekBack: T;
    function TryPeekBack(out AValue: T): Boolean;
    
    // Clear all elements
    procedure Clear;
    
    // Properties
    property Count: Integer read GetCount;
    
    // Iterator support
    function GetEnumerator: TEnumerator;
    function Lock: ILockToken;
    
    // Check if deque is empty
    function IsEmpty: Boolean;
    
    // Convert to array
    function ToArray: specialize TArray<T>;
    
    // Copy all items to an array
    procedure CopyTo(var AArray: array of T; AStartIndex: Integer = 0);
    
    // Add multiple items
    procedure PushRangeBack(const AItems: array of T);
    procedure PushRangeFront(const AItems: array of T);
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

procedure TThreadSafeDeque.PushRangeBack(const AItems: array of T);
var
  I: Integer;
begin
  for I := Low(AItems) to High(AItems) do
    PushBack(AItems[I]);
end;

procedure TThreadSafeDeque.PushRangeFront(const AItems: array of T);
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
