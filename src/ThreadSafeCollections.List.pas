unit ThreadSafeCollections.List;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, SyncObjs, ThreadSafeCollections.Interfaces, Math;

type
  // Generic comparer type
  generic TComparer<T> = function(const A, B: T): Integer;

    { Array type declaration }
  generic TItemArray<T> = array of T;

  // Generic ThreadSafe List
  // This class provides a thread-safe implementation of a generic list.
  // It allows concurrent access and modifications by multiple threads without data corruption.
  generic TThreadSafeList<T> = class(TInterfacedObject, specialize IThreadSafeList<T>)
  private
    FList: array of T;                             // Internal dynamic array to store list items
    FCount: Integer;                               // Current number of items in the list
    FCapacity: Integer;                            // Current capacity of the internal array
    FLock: TCriticalSection;                       // Critical section to manage thread synchronization
    FComparer: specialize TComparer<T>;            // Comparer function to define the sorting logic
    FSorted: Boolean;                              // Indicates whether the list is currently sorted

    // Increases the capacity of the internal array when needed
    procedure Grow;

    // Performs the QuickSort algorithm on the list
    // Parameters:
    //   Left: The starting index of the segment to sort
    //   Right: The ending index of the segment to sort
    //   Ascending: Determines the sort order (True for ascending, False for descending)
    procedure QuickSort(Left, Right: Integer; Ascending: Boolean);

    // Retrieves an item at a specific index with thread safety
    // Parameters:
    //   Index: The position of the item to retrieve
    // Returns:
    //   The item at the specified index
    function GetItem(Index: Integer): T;

    // Sets the value of an item at a specific index with thread safety
    // Parameters:
    //   Index: The position of the item to set
    //   Value: The new value to assign to the item
    procedure SetItem(Index: Integer; const Value: T);

    // Retrieves the current count of items with thread safety
    // Returns:
    //   The number of items in the list
    function GetCount: Integer;

    // Add these to existing private section
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
    procedure RaiseIfOutOfBounds(Index: Integer);

  public
    // Constructor
    // Initializes the thread-safe list with a provided comparer for sorting
    // Parameters:
    //   AComparer: A comparer function to determine the order of elements
    constructor Create(AComparer: specialize TComparer<T>);

    // Destructor
    // Cleans up resources used by the list, including the critical section
    destructor Destroy; override;

    // Adds a new item to the list in a thread-safe manner
    // Parameters:
    //   Item: The item to add to the list
    // Returns:
    //   The index at which the item was added
    function Add(const Item: T): Integer;

    // Deletes an item from the list at a specified index in a thread-safe manner
    // Parameters:
    //   Index: The position of the item to delete
    procedure Delete(Index: Integer);

    // Finds the index of a specific item in the list
    // Parameters:
    //   Item: The item to search for
    // Returns:
    //   The index of the item if found; otherwise, -1
    function IndexOf(const Item: T): Integer;

    // Retrieves the first item in the list in a thread-safe manner
    // Returns:
    //   The first item in the list
    function First: T;

    // Retrieves the last item in the list in a thread-safe manner
    // Returns:
    //   The last item in the list
    function Last: T;

    // Sorts the list using the provided comparer
    // Parameters:
    //   Ascending: Optional parameter to sort in ascending order (default is True)
    procedure Sort(Ascending: Boolean = True);

    // Checks if the list is currently sorted
    // Returns:
    //   True if the list is sorted; otherwise, False
    function IsSorted: Boolean;

    // Replaces an item at a specified index with a new item in a thread-safe manner
    // Parameters:
    //   Index: The position of the item to replace
    //   Item: The new item to assign to the specified index
    procedure Replace(Index: Integer; const Item: T);

    // Clears all items from the list in a thread-safe manner
    procedure Clear;

    // Checks if the list is empty in a thread-safe manner
    // Returns:
    //   True if the list contains no items; otherwise, False
    function IsEmpty: Boolean;

    // Default property to access items by index with thread safety
    property Items[Index: Integer]: T read GetItem write SetItem; default;

    // Property to get the current count of items with thread safety
    property Count: Integer read GetCount;

    // Enumerator Class
    // Provides iteration capabilities for the thread-safe list
    type
      TEnumerator = class
      private
        FList: specialize TThreadSafeList<T>;      // Reference to the thread-safe list being enumerated
        FIndex: Integer;                           // Current index in the enumeration
        FCurrent: T;                               // Current element in the enumeration
        FLockToken: ILockToken;                    // Lock token to ensure thread safety during enumeration
      public
        // Constructor
        // Initializes the enumerator with a reference to the thread-safe list
        // Parameters:
        //   AList: The thread-safe list to enumerate
        constructor Create(AList: specialize TThreadSafeList<T>);

        // Destructor
        // Releases the lock token when the enumerator is destroyed
        destructor Destroy; override;

        // Moves to the next element in the list
        // Returns:
        //   True if there is a next element; otherwise, False
        function MoveNext: Boolean;

        // Property to access the current element in the enumeration
        property Current: T read FCurrent;
      end;

    // GetEnumerator method for for-in loops
    // Returns an enumerator to iterate over the list's elements
    function GetEnumerator: TEnumerator;

    // Acquires a lock token for thread-safe operations outside the class
    // Returns:
    //   An ILockToken that manages the critical section lock
    function Lock: ILockToken;

    // Capacity management
    property Capacity: Integer read GetCapacity write SetCapacity;

    // Array operations
    function ToArray: specialize TArray<T>;
    procedure FromArray(const Values: array of T);

    // Range operations
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Collection: specialize IThreadSafeList<T>); overload;
    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    procedure InsertRange(Index: Integer; const Collection: specialize IThreadSafeList<T>); overload;
    procedure DeleteRange(AIndex, ACount: Integer);

    // Search operations
    function Contains(const Value: T): Boolean;
    function IndexOfItem(const Item: T; StartIndex: Integer): Integer; overload;
    function IndexOfItem(const Item: T; StartIndex, ACount: Integer): Integer; overload;
    function LastIndexOf(const Item: T): Integer; overload;
    function LastIndexOf(const Item: T; StartIndex: Integer): Integer; overload;
    function LastIndexOf(const Item: T; StartIndex, ACount: Integer): Integer; overload;

    // Additional utility methods
    procedure Insert(Index: Integer; const Item: T);
    procedure Exchange(Index1, Index2: Integer);
    procedure MoveItem(CurIndex, NewIndex: Integer);
    procedure Reverse;
    function Extract(const Item: T): T;
    function ExtractAt(Index: Integer): T;
    procedure TrimExcess;
  end;

// Basic comparers declarations
function IntegerComparer(const A, B: Integer): Integer;
function StringComparer(const A, B: string): Integer;
function BooleanComparer(const A, B: Boolean): Integer;
function RealComparer(const A, B: Real): Integer;

implementation

// Basic comparers implementation
function IntegerComparer(const A, B: Integer): Integer;
begin
  Result := A - B;
end;

function StringComparer(const A, B: string): Integer;
begin
  Result := CompareStr(A, B);
end;

function BooleanComparer(const A, B: Boolean): Integer;
begin
  Result := Ord(A) - Ord(B);
end;

function RealComparer(const A, B: Real): Integer;
begin
  if A < B then
    Result := -1
  else if A > B then
    Result := 1
  else
    Result := 0;
end;

{ TThreadSafeList }

constructor TThreadSafeList.Create(AComparer: specialize TComparer<T>);
begin
  inherited Create;
  if not Assigned(AComparer) then
    raise Exception.Create('Comparer must be provided');
    
  FLock := TCriticalSection.Create;
  FComparer := AComparer;
  FCount := 0;
  FCapacity := 0;
  FSorted := True;
end;

destructor TThreadSafeList.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TThreadSafeList.Grow;
begin
  if FCapacity = 0 then
    FCapacity := 4
  else
    FCapacity := FCapacity * 2;
  SetLength(FList, FCapacity);
end;

procedure TThreadSafeList.QuickSort(Left, Right: Integer; Ascending: Boolean);
var
  I, J: Integer;
  P, Temp: T;
begin
  if Right <= Left then Exit;
  
  I := Left;
  J := Right;
  P := FList[(Left + Right) div 2];
  
  repeat
    if Ascending then
    begin
      while FComparer(FList[I], P) < 0 do Inc(I);
      while FComparer(FList[J], P) > 0 do Dec(J);
    end else
    begin
      while FComparer(FList[I], P) > 0 do Inc(I);
      while FComparer(FList[J], P) < 0 do Dec(J);
    end;
    
    if I <= J then
    begin
      Temp := FList[I];
      FList[I] := FList[J];
      FList[J] := Temp;
      Inc(I);
      Dec(J);
    end;
  until I > J;
  
  if Left < J then QuickSort(Left, J, Ascending);
  if I < Right then QuickSort(I, Right, Ascending);
end;

function TThreadSafeList.Add(const Item: T): Integer;
begin
  FLock.Acquire;
  try
    if FCount = FCapacity then
      Grow;
      
    FList[FCount] := Item;
    Result := FCount;
    Inc(FCount);
    
    if FCount > 1 then
      FSorted := FSorted and (FComparer(FList[FCount-2], Item) <= 0);
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeList.Delete(Index: Integer);
var
  I: Integer;
begin
  FLock.Acquire;
  try
    if (Index < 0) or (Index >= FCount) then
      raise Exception.Create('Index out of bounds');
      
    for I := Index to FCount - 2 do
      FList[I] := FList[I + 1];
    Dec(FCount);
  finally
    FLock.Release;
  end;
end;

function TThreadSafeList.IndexOf(const Item: T): Integer;
var
  I: Integer;
begin
  Result := -1;
  FLock.Acquire;
  try
    for I := 0 to FCount - 1 do
      if FComparer(FList[I], Item) = 0 then
      begin
        Result := I;
        Break;
      end;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeList.First: T;
begin
  FLock.Acquire;
  try
    if FCount = 0 then
      raise Exception.Create('List is empty');
    Result := FList[0];
  finally
    FLock.Release;
  end;
end;

function TThreadSafeList.Last: T;
begin
  FLock.Acquire;
  try
    if FCount = 0 then
      raise Exception.Create('List is empty');
    Result := FList[FCount - 1];
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeList.Sort(Ascending: Boolean);
begin
  FLock.Acquire;
  try
    if FCount > 1 then
      QuickSort(0, FCount - 1, Ascending);
    FSorted := True;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeList.IsSorted: Boolean;
begin
  FLock.Acquire;
  try
    Result := FSorted;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeList.Replace(Index: Integer; const Item: T);
begin
  FLock.Acquire;
  try
    if (Index < 0) or (Index >= FCount) then
      raise Exception.Create('Index out of bounds');
    FList[Index] := Item;
    
    if FSorted then
    begin
      if (Index > 0) and (FComparer(FList[Index-1], Item) > 0) then
        FSorted := False
      else if (Index < FCount-1) and (FComparer(Item, FList[Index+1]) > 0) then
        FSorted := False;
    end;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeList.GetItem(Index: Integer): T;
begin
  FLock.Acquire;
  try
    if (Index < 0) or (Index >= FCount) then
      raise Exception.Create('Index out of bounds');
    Result := FList[Index];
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeList.SetItem(Index: Integer; const Value: T);
begin
  Replace(Index, Value);
end;

{ TThreadSafeList.TEnumerator }

constructor TThreadSafeList.TEnumerator.Create(AList: specialize TThreadSafeList<T>);
begin
  inherited Create;
  FList := AList;
  FLockToken := FList.Lock;
  FIndex := -1;
end;

destructor TThreadSafeList.TEnumerator.Destroy;
begin
  FLockToken := nil; // Release lock
  inherited;
end;

function TThreadSafeList.TEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  if FIndex < FList.FCount then
  begin
    FCurrent := FList.FList[FIndex];
    Result := True;
  end
  else
    Result := False;
end;

function TThreadSafeList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TThreadSafeList.Lock: ILockToken;
begin
  Result := TLockToken.Create(FLock);
end;

function TThreadSafeList.GetCount: Integer;
begin
  FLock.Acquire;
  try
    Result := FCount;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeList.Clear;
begin
  FLock.Acquire;
  try
    SetLength(FList, 0);
    FCount := 0;
    FCapacity := 0;
    FSorted := True;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeList.IsEmpty: Boolean;
begin
  FLock.Acquire;
  try
    Result := FCount = 0;
  finally
    FLock.Release;
  end;
end;

{ TThreadSafeList implementation - Part 1: Capacity and Array Operations }

function TThreadSafeList.GetCapacity: Integer;
begin
  FLock.Acquire;
  try
    Result := FCapacity;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeList.SetCapacity(const Value: Integer);
begin
  FLock.Acquire;
  try
    if Value < FCount then
      raise EArgumentOutOfRangeException.Create('Capacity cannot be less than Count');
      
    if Value <> FCapacity then
    begin
      SetLength(FList, Value);
      FCapacity := Value;
    end;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeList.ToArray: specialize TArray<T>;
begin
  FLock.Acquire;
  try
    SetLength(Result, FCount);
    if FCount > 0 then
      Move(FList[0], Result[0], FCount * SizeOf(T));
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeList.FromArray(const Values: array of T);
var
  NewCount: Integer;
begin
  FLock.Acquire;
  try
    NewCount := Length(Values);
    if NewCount > FCapacity then
      SetCapacity(NewCount);
      
    if NewCount > 0 then
      Move(Values[0], FList[0], NewCount * SizeOf(T));
    FCount := NewCount;
    FSorted := False; // Reset sorted flag as we don't know the order
  finally
    FLock.Release;
  end;
end;

{ Range Operations }

procedure TThreadSafeList.AddRange(const Values: array of T);
var
  I: Integer;
begin
  FLock.Acquire;
  try
    for I := 0 to High(Values) do
      Add(Values[I]);
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeList.AddRange(const Collection: specialize IThreadSafeList<T>);
var
  SourceArray: specialize TItemArray<T>;
begin
  if Collection = nil then
    Exit;
    
  // Get array from source collection
  SourceArray := Collection.ToArray;
  AddRange(SourceArray);
end;

procedure TThreadSafeList.InsertRange(Index: Integer; const Values: array of T);
var
  InsertCount: Integer;
begin
  FLock.Acquire;
  try
    if (Index < 0) or (Index > FCount) then
      raise EArgumentOutOfRangeException.Create('Index out of bounds');
      
    InsertCount := Length(Values);
    if InsertCount = 0 then
      Exit;
      
    // Ensure capacity
    if FCount + InsertCount > FCapacity then
      SetCapacity(FCount + InsertCount);
      
    // Move existing items
    if Index < FCount then
      Move(FList[Index], FList[Index + InsertCount], (FCount - Index) * SizeOf(T));
      
    // Copy new items
    Move(Values[0], FList[Index], InsertCount * SizeOf(T));
    Inc(FCount, InsertCount);
    FSorted := False;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeList.InsertRange(Index: Integer; const Collection: specialize IThreadSafeList<T>);
var
  SourceArray: specialize TItemArray<T>;
begin
  if Collection = nil then
    Exit;
    
  SourceArray := Collection.ToArray;
  InsertRange(Index, SourceArray);
end;

procedure TThreadSafeList.DeleteRange(AIndex, ACount: Integer);
var
  OldCount: Integer;
begin
  FLock.Acquire;
  try
    if (AIndex < 0) or (ACount < 0) or (AIndex + ACount > FCount) then
      raise EArgumentOutOfRangeException.Create('Invalid index or count');
      
    if ACount = 0 then
      Exit;
      
    OldCount := FCount;
    if AIndex + ACount < OldCount then
      Move(FList[AIndex + ACount], FList[AIndex], (OldCount - (AIndex + ACount)) * SizeOf(T));
      
    Dec(FCount, ACount);
  finally
    FLock.Release;
  end;
end;

{ TThreadSafeList implementation - Part 2: Search Operations }

function TThreadSafeList.Contains(const Value: T): Boolean;
begin
  Result := IndexOf(Value) >= 0;
end;

function TThreadSafeList.IndexOfItem(const Item: T; StartIndex: Integer): Integer;
begin
  Result := IndexOfItem(Item, StartIndex, FCount - StartIndex);
end;

function TThreadSafeList.IndexOfItem(const Item: T; StartIndex, ACount: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  FLock.Acquire;
  try
    if (StartIndex < 0) or (StartIndex >= FCount) then
      Exit;
      
    ACount := Min(ACount, FCount - StartIndex);
    for I := StartIndex to StartIndex + ACount - 1 do
      if FComparer(FList[I], Item) = 0 then
      begin
        Result := I;
        Break;
      end;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeList.LastIndexOf(const Item: T): Integer;
begin
  Result := LastIndexOf(Item, FCount - 1);
end;

function TThreadSafeList.LastIndexOf(const Item: T; StartIndex: Integer): Integer;
begin
  Result := LastIndexOf(Item, StartIndex, StartIndex + 1);
end;

function TThreadSafeList.LastIndexOf(const Item: T; StartIndex, ACount: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  FLock.Acquire;
  try
    if (FCount = 0) or (StartIndex < 0) then
      Exit;
      
    StartIndex := Min(StartIndex, FCount - 1);
    ACount := Min(ACount, StartIndex + 1);
    
    for I := StartIndex downto StartIndex - ACount + 1 do
      if FComparer(FList[I], Item) = 0 then
      begin
        Result := I;
        Break;
      end;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeList.MoveItem(CurIndex, NewIndex: Integer);
var
  Item: T;
begin
  FLock.Acquire;
  try
    if (CurIndex < 0) or (CurIndex >= FCount) or
       (NewIndex < 0) or (NewIndex >= FCount) then
      raise EArgumentOutOfRangeException.Create('Index out of bounds');
      
    if CurIndex <> NewIndex then
    begin
      Item := FList[CurIndex];
      if NewIndex < CurIndex then
        Move(FList[NewIndex], FList[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(T))
      else
        Move(FList[CurIndex + 1], FList[CurIndex], (NewIndex - CurIndex) * SizeOf(T));
      FList[NewIndex] := Item;
      FSorted := False;
    end;
  finally
    FLock.Release;
  end;
end;

{ TThreadSafeList implementation - Additional methods }

procedure TThreadSafeList.RaiseIfOutOfBounds(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EArgumentOutOfRangeException.Create('Index out of bounds');
end;

procedure TThreadSafeList.Insert(Index: Integer; const Item: T);
begin
  FLock.Acquire;
  try
    if (Index < 0) or (Index > FCount) then
      raise EArgumentOutOfRangeException.Create('Index out of bounds');
      
    if FCount = FCapacity then
      Grow;
      
    if Index < FCount then
      System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(T));
      
    FList[Index] := Item;
    Inc(FCount);
    FSorted := False;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeList.Exchange(Index1, Index2: Integer);
var
  Temp: T;
begin
  FLock.Acquire;
  try
    RaiseIfOutOfBounds(Index1);
    RaiseIfOutOfBounds(Index2);
    
    if Index1 <> Index2 then
    begin
      Temp := FList[Index1];
      FList[Index1] := FList[Index2];
      FList[Index2] := Temp;
      FSorted := False;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeList.Reverse;
var
  I: Integer;
  Temp: T;
begin
  FLock.Acquire;
  try
    for I := 0 to (FCount div 2) - 1 do
    begin
      Temp := FList[I];
      FList[I] := FList[FCount - 1 - I];
      FList[FCount - 1 - I] := Temp;
    end;
    FSorted := False;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeList.Extract(const Item: T): T;
var
  Index: Integer;
begin
  FLock.Acquire;
  try
    Index := IndexOf(Item);
    if Index = -1 then
      raise EArgumentOutOfRangeException.Create('Item not found');
    Result := FList[Index];
    Delete(Index);
  finally
    FLock.Release;
  end;
end;

function TThreadSafeList.ExtractAt(Index: Integer): T;
begin
  FLock.Acquire;
  try
    RaiseIfOutOfBounds(Index);
    Result := FList[Index];
    Delete(Index);
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeList.TrimExcess;
begin
  FLock.Acquire;
  try
    if FCount < FCapacity then
      SetCapacity(FCount);
  finally
    FLock.Release;
  end;
end;

end.
 
