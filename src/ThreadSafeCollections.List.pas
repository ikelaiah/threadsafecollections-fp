unit ThreadSafeCollections.List;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, SyncObjs, ThreadSafeCollections.Interfaces;

type
  // Generic comparer type
  generic TComparer<T> = function(const A, B: T): Integer;

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
    function Find(const Item: T): Integer;

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

function TThreadSafeList.Find(const Item: T): Integer;
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

end.
 
