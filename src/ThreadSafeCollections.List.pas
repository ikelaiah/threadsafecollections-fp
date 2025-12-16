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

    // Accessors for Capacity property
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);

    // Checks if the provided index is within the valid range
    // Parameters:
    //   Index: The index to check
    procedure RaiseIfOutOfBounds(Index: Integer);

  public
    // Constructor
    // Initializes the thread-safe list with a provided comparer for sorting
    // Parameters:
    //   AComparer: A comparer function to determine the order of elements
    constructor Create(AComparer: specialize TComparer<T>);

    // v0.8: Constructor with initial capacity hint for performance optimization
    // Parameters:
    //   AComparer: A comparer function to determine the order of elements
    //   AInitialCapacity: Suggested initial capacity (will be pre-allocated to avoid early resizes)
    constructor Create(AComparer: specialize TComparer<T>; AInitialCapacity: Integer);

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
    // Converts the list to a dynamic array
    function ToArray: specialize TArray<T>;

    // Populates the list from a given array of values
    // Parameters:
    //   Values: An array of items to populate the list with
    procedure FromArray(const Values: array of T);

    // Range operations
    // Adds a range of values to the list
    procedure AddRange(const Values: array of T); overload;

    // Adds a range of values from another thread-safe list
    // Parameters:
    //   Collection: The source thread-safe list to copy items from
    // Notes:
    //   - If Collection is nil, the method exits without doing anything
    //   - The operation is thread-safe
    //   - The sorted status will be set to False after adding
    procedure AddRange(const Collection: specialize IThreadSafeList<T>); overload;

    // Inserts a range of values at a specific index
    procedure InsertRange(Index: Integer; const Values: array of T); overload;

    // Inserts a range of values from another thread-safe list at a specific index
    procedure InsertRange(Index: Integer; const Collection: specialize IThreadSafeList<T>); overload;

    // Deletes a range of items starting from a specific index
    procedure DeleteRange(AIndex, ACount: Integer);

    // Search operations
    // Checks if the list contains a specific value
    function Contains(const Value: T): Boolean;

    // Finds the index of an item starting from a specific index
    // Parameters:
    //   Item: The item to search for
    //   StartIndex: The index to start searching from
    // Returns:
    //   The index of the first occurrence of the item if found; otherwise, -1
    // Notes:
    //   - StartIndex must be within bounds (0 to Count-1)
    //   - The search uses the comparer function provided at creation
    function IndexOfItem(const Item: T; StartIndex: Integer): Integer; overload;

    // Finds the index of an item starting from a specific index and within a count
    function IndexOfItem(const Item: T; StartIndex, ACount: Integer): Integer; overload;

    // Finds the last index of an item in the list
    function LastIndexOf(const Item: T): Integer; overload;

    // Finds the last index of an item starting from a specific index
    function LastIndexOf(const Item: T; StartIndex: Integer): Integer; overload;

    // Finds the last index of an item starting from a specific index and within a count
    // Parameters:
    //   Item: The item to search for
    //   StartIndex: The index to start searching from (backwards)
    //   ACount: The maximum number of elements to search through
    // Returns:
    //   The index of the last occurrence of the item if found; otherwise, -1
    // Notes:
    //   - StartIndex and ACount are adjusted if they exceed bounds
    //   - The search uses the comparer function provided at creation
    function LastIndexOf(const Item: T; StartIndex, ACount: Integer): Integer; overload;

    // Additional utility methods
    // Inserts an item at a specific index
    procedure Insert(Index: Integer; const Item: T);

    // Exchanges two items at specified indices
    procedure Exchange(Index1, Index2: Integer);

    // Moves an item from one index to another in the list
    // Parameters:
    //   CurIndex: The current index of the item to move
    //   NewIndex: The target index where the item should be moved to
    // Notes:
    //   - Both indices must be within bounds (0 to Count-1)
    //   - The operation is thread-safe
    //   - The sorted status will be set to False after moving
    procedure MoveItem(CurIndex, NewIndex: Integer);

    // Reverses the order of items in the list
    procedure Reverse;

    // Extracts an item from the list and returns it
    // Parameters:
    //   Item: The item to find and extract from the list
    // Returns:
    //   The extracted item
    // Raises:
    //   EArgumentOutOfRangeException if the item is not found
    function Extract(const Item: T): T;

    // Extracts an item at a specific index
    // Parameters:
    //   Index: The index of the item to extract
    // Returns:
    //   The extracted item
    function ExtractAt(Index: Integer): T;

    // Trims excess capacity from the list
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
  Create(AComparer, 16);  // v0.8: Delegate to overloaded constructor with default capacity
end;

constructor TThreadSafeList.Create(AComparer: specialize TComparer<T>; AInitialCapacity: Integer);
begin
  inherited Create;
  if not Assigned(AComparer) then
    raise Exception.Create('Comparer must be provided');

  FLock := TCriticalSection.Create;      // Initialize the critical section for thread safety
  FComparer := AComparer;                // Assign the comparer function
  FCount := 0;                           // Initialize item count

  // v0.8: Use provided capacity hint or default to 16
  if AInitialCapacity < 4 then
    AInitialCapacity := 4;                // Minimum reasonable capacity
  FCapacity := AInitialCapacity;         // Set initial capacity
  SetLength(FList, FCapacity);           // Pre-allocate buffer to reduce early resizes
  FSorted := True;                       // Initially, the list is considered sorted
end;

destructor TThreadSafeList.Destroy;
begin
  FLock.Free;                            // Free the critical section
  inherited;
end;

procedure TThreadSafeList.Grow;
begin
  // v0.8: Optimized growth strategy
  if FCapacity = 0 then
    FCapacity := 16                         // Start with reasonable initial capacity
  else if FCapacity < 64 then
    FCapacity := FCapacity * 2              // Double for small sizes
  else
    FCapacity := FCapacity + (FCapacity div 2);  // Grow by 50% for larger sizes to reduce memory waste
  SetLength(FList, FCapacity);              // Resize the internal array
end;

procedure TThreadSafeList.QuickSort(Left, Right: Integer; Ascending: Boolean);
var
  I, J: Integer;
  P, Temp: T;
begin
  if Right <= Left then Exit;              // Base case: segment size <= 1

  I := Left;
  J := Right;
  P := FList[(Left + Right) div 2];        // Choose the pivot element

  repeat
    if Ascending then
    begin
      while FComparer(FList[I], P) < 0 do Inc(I);    // Find element >= pivot
      while FComparer(FList[J], P) > 0 do Dec(J);    // Find element <= pivot
    end else
    begin
      while FComparer(FList[I], P) > 0 do Inc(I);    // For descending order
      while FComparer(FList[J], P) < 0 do Dec(J);
    end;

    if I <= J then
    begin
      Temp := FList[I];                              // Swap elements
      FList[I] := FList[J];
      FList[J] := Temp;
      Inc(I);
      Dec(J);
    end;
  until I > J;

  if Left < J then QuickSort(Left, J, Ascending);     // Recursively sort left partition
  if I < Right then QuickSort(I, Right, Ascending);   // Recursively sort right partition
end;

function TThreadSafeList.Add(const Item: T): Integer;
begin
  FLock.Acquire;                                     // Enter critical section
  try
    if FCount = FCapacity then
      Grow;                                          // Grow the list if capacity is reached

    FList[FCount] := Item;                           // Add the new item
    Result := FCount;
    Inc(FCount);

    // Update the sorted flag if necessary
    if FCount > 1 then
      FSorted := FSorted and (FComparer(FList[FCount-2], Item) <= 0);
  finally
    FLock.Release;                                     // Exit critical section
  end;
end;

procedure TThreadSafeList.Delete(Index: Integer);
var
  I: Integer;
begin
  FLock.Acquire;                                       // Enter critical section
  try
    if (Index < 0) or (Index >= FCount) then
      raise Exception.Create('Index out of bounds');

    // Shift elements to remove the item
    for I := Index to FCount - 2 do
      FList[I] := FList[I + 1];
    Dec(FCount);
  finally
    FLock.Release;                                     // Exit critical section
  end;
end;

function TThreadSafeList.IndexOf(const Item: T): Integer;
var
  I: Integer;
begin
  Result := -1;
  FLock.Acquire;                                       // Enter critical section
  try
    for I := 0 to FCount - 1 do
      if FComparer(FList[I], Item) = 0 then
      begin
        Result := I;
        Break;                                          // Item found, exit loop
      end;
  finally
    FLock.Release;                                     // Exit critical section
  end;
end;

function TThreadSafeList.First: T;
begin
  FLock.Acquire;                                       // Enter critical section
  try
    if FCount = 0 then
      raise Exception.Create('List is empty');
    Result := FList[0];                                // Return the first item
  finally
    FLock.Release;                                     // Exit critical section
  end;
end;

function TThreadSafeList.Last: T;
begin
  FLock.Acquire;                                       // Enter critical section
  try
    if FCount = 0 then
      raise Exception.Create('List is empty');
    Result := FList[FCount - 1];                       // Return the last item
  finally
    FLock.Release;                                     // Exit critical section
  end;
end;

procedure TThreadSafeList.Sort(Ascending: Boolean);
begin
  FLock.Acquire;                                       // Enter critical section
  try
    if FCount > 1 then
      QuickSort(0, FCount - 1, Ascending);             // Perform quicksort
    FSorted := True;                                   // Mark the list as sorted
  finally
    FLock.Release;                                     // Exit critical section
  end;
end;

function TThreadSafeList.IsSorted: Boolean;
begin
  FLock.Acquire;                                       // Enter critical section
  try
    Result := FSorted;                                 // Return sorted status
  finally
    FLock.Release;                                     // Exit critical section
  end;
end;

procedure TThreadSafeList.Replace(Index: Integer; const Item: T);
begin
  FLock.Acquire;                                       // Enter critical section
  try
    if (Index < 0) or (Index >= FCount) then
      raise Exception.Create('Index out of bounds');
    FList[Index] := Item;                              // Replace the item

    // Update the sorted flag based on neighboring elements
    if FSorted then
    begin
      if (Index > 0) and (FComparer(FList[Index-1], Item) > 0) then
        FSorted := False
      else if (Index < FCount-1) and (FComparer(Item, FList[Index+1]) > 0) then
        FSorted := False;
    end;
  finally
    FLock.Release;                                     // Exit critical section
  end;
end;

function TThreadSafeList.GetItem(Index: Integer): T;
begin
  FLock.Acquire;                                       // Enter critical section
  try
    if (Index < 0) or (Index >= FCount) then
      raise Exception.Create('Index out of bounds');
    Result := FList[Index];                            // Retrieve the item
  finally
    FLock.Release;                                     // Exit critical section
  end;
end;

procedure TThreadSafeList.SetItem(Index: Integer; const Value: T);
begin
  Replace(Index, Value);                               // Delegate to Replace method
end;

{ TThreadSafeList.TEnumerator }

constructor TThreadSafeList.TEnumerator.Create(AList: specialize TThreadSafeList<T>);
begin
  inherited Create;
  FList := AList;
  FLockToken := FList.Lock;                             // Acquire a lock token for safe enumeration
  FIndex := -1;                                         // Initialize enumerator index
end;

destructor TThreadSafeList.TEnumerator.Destroy;
begin
  FLockToken := nil;                                    // Release the lock token
  inherited;
end;

function TThreadSafeList.TEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);                                          // Move to the next index
  if FIndex < FList.FCount then
  begin
    FCurrent := FList.FList[FIndex];                    // Retrieve the current item
    Result := True;                                     // Indicate that there is a next item
  end
  else
    Result := False;                                    // No more items
end;

function TThreadSafeList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);                     // Create and return an enumerator instance
end;

function TThreadSafeList.Lock: ILockToken;
begin
  Result := TLockToken.Create(FLock);                     // Create and return a lock token
end;

function TThreadSafeList.GetCount: Integer;
begin
  FLock.Acquire;                                           // Enter critical section
  try
    Result := FCount;                                      // Return the count
  finally
    FLock.Release;                                         // Exit critical section
  end;
end;

procedure TThreadSafeList.Clear;
begin
  FLock.Acquire;                                          // Enter critical section
  try
    SetLength(FList, 0);                                  // Clear the internal array
    FCount := 0;                                          // Reset count
    FCapacity := 0;                                       // Reset capacity
    FSorted := True;                                      // Reset sorted status
  finally
    FLock.Release;                                        // Exit critical section
  end;
end;

function TThreadSafeList.IsEmpty: Boolean;
begin
  FLock.Acquire;                                           // Enter critical section
  try
    Result := FCount = 0;                                  // Check if the list is empty
  finally
    FLock.Release;                                         // Exit critical section
  end;
end;

{ TThreadSafeList implementation - Part 1: Capacity and Array Operations }

function TThreadSafeList.GetCapacity: Integer;
begin
  FLock.Acquire;                                           // Enter critical section
  try
    Result := FCapacity;                                   // Return current capacity
  finally
    FLock.Release;                                         // Exit critical section
  end;
end;

procedure TThreadSafeList.SetCapacity(const Value: Integer);
begin
  FLock.Acquire;                                           // Enter critical section
  try
    if Value < FCount then
      raise EArgumentOutOfRangeException.Create('Capacity cannot be less than Count');

    if Value <> FCapacity then
    begin
      SetLength(FList, Value);                             // Resize the internal array
      FCapacity := Value;                                  // Update capacity
    end;
  finally
    FLock.Release;                                         // Exit critical section
  end;
end;

function TThreadSafeList.ToArray: specialize TArray<T>;
begin
  FLock.Acquire;                                           // Enter critical section
  try
    SetLength(Result, FCount);                             // Initialize the result array
    if FCount > 0 then
      Move(FList[0], Result[0], FCount * SizeOf(T));       // Copy items to the result array
  finally
    FLock.Release;                                         // Exit critical section
  end;
end;

procedure TThreadSafeList.FromArray(const Values: array of T);
var
  NewCount: Integer;
begin
  FLock.Acquire;                                            // Enter critical section
  try
    NewCount := Length(Values);
    if NewCount > FCapacity then
      SetCapacity(NewCount);                                // Ensure enough capacity

    if NewCount > 0 then
      Move(Values[0], FList[0], NewCount * SizeOf(T));      // Copy items from the input array
    FCount := NewCount;                                     // Update the count
    FSorted := False;                                       // Reset sorted flag as order is unknown
  finally
    FLock.Release;                                          // Exit critical section
  end;
end;

{ Range Operations }

procedure TThreadSafeList.AddRange(const Values: array of T);
var
  I, NewCount, RequiredCapacity: Integer;
begin
  if Length(Values) = 0 then
    Exit;                                          // Nothing to add

  FLock.Acquire;                                   // Enter critical section
  try
    NewCount := FCount + Length(Values);

    // v0.8: Pre-allocate capacity if needed to avoid multiple resizes
    if NewCount > FCapacity then
    begin
      RequiredCapacity := NewCount;
      // Round up to avoid immediate regrowth
      if RequiredCapacity < 64 then
        RequiredCapacity := ((RequiredCapacity + 15) div 16) * 16  // Round to nearest 16
      else
        RequiredCapacity := ((RequiredCapacity * 3) div 2);         // 50% extra buffer
      SetCapacity(RequiredCapacity);
    end;

    // Copy items efficiently
    for I := 0 to High(Values) do
    begin
      FList[FCount] := Values[I];
      Inc(FCount);
      // Update the sorted flag if necessary
      if (FCount > 1) and FSorted then
        FSorted := FComparer(FList[FCount-2], FList[FCount-1]) <= 0;
    end;
  finally
    FLock.Release;                                 // Exit critical section
  end;
end;

procedure TThreadSafeList.AddRange(const Collection: specialize IThreadSafeList<T>);
var
  SourceArray: specialize TItemArray<T>;
begin
  if Collection = nil then
    Exit;                                           // Exit if the collection is nil

  // Get array from source collection
  SourceArray := Collection.ToArray;
  AddRange(SourceArray);                            // Add the range from the array
end;

procedure TThreadSafeList.InsertRange(Index: Integer; const Values: array of T);
var
  InsertCount: Integer;
begin
  FLock.Acquire;                                           // Enter critical section
  try
    if (Index < 0) or (Index > FCount) then
      raise EArgumentOutOfRangeException.Create('Index out of bounds');

    InsertCount := Length(Values);
    if InsertCount = 0 then
      Exit;                                                 // Nothing to insert

    // Ensure capacity
    if FCount + InsertCount > FCapacity then
      SetCapacity(FCount + InsertCount);

    // Move existing items to make space for new items
    if Index < FCount then
      Move(FList[Index], FList[Index + InsertCount], (FCount - Index) * SizeOf(T));

    // Copy new items into the list
    Move(Values[0], FList[Index], InsertCount * SizeOf(T));
    Inc(FCount, InsertCount);                               // Update the count
    FSorted := False;                                       // List is no longer sorted
  finally
    FLock.Release;                                          // Exit critical section
  end;
end;

procedure TThreadSafeList.InsertRange(Index: Integer; const Collection: specialize IThreadSafeList<T>);
var
  SourceArray: specialize TItemArray<T>;
begin
  if Collection = nil then
    Exit;                                                  // Exit if the collection is nil

  SourceArray := Collection.ToArray;
  InsertRange(Index, SourceArray);                         // Insert the range from the array
end;

procedure TThreadSafeList.DeleteRange(AIndex, ACount: Integer);
var
  OldCount: Integer;
begin
  FLock.Acquire;                                           // Enter critical section
  try
    if (AIndex < 0) or (ACount < 0) or (AIndex + ACount > FCount) then
      raise EArgumentOutOfRangeException.Create('Invalid index or count');

    if ACount = 0 then
      Exit;                                                 // Nothing to delete

    OldCount := FCount;
    if AIndex + ACount < OldCount then
      Move(FList[AIndex + ACount], 
           FList[AIndex], 
           (OldCount - (AIndex + ACount)) * SizeOf(T));    // Shift remaining items to fill the gap

    Dec(FCount, ACount);                                   // Update the count
  finally
    FLock.Release;                                         // Exit critical section
  end;
end;

{ TThreadSafeList implementation - Part 2: Search Operations }

function TThreadSafeList.Contains(const Value: T): Boolean;
begin
  Result := IndexOf(Value) >= 0;                           // Check if the item exists in the list
end;

function TThreadSafeList.IndexOfItem(const Item: T; StartIndex: Integer): Integer;
begin
  Result := IndexOfItem(Item, StartIndex, FCount - StartIndex);    // Delegate to overloaded method
end;

function TThreadSafeList.IndexOfItem(const Item: T; StartIndex, ACount: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  FLock.Acquire;                                                // Enter critical section
  try
    if (StartIndex < 0) or (StartIndex >= FCount) then
      Exit;                                                     // Invalid start index

    ACount := Min(ACount, FCount - StartIndex);                 // Adjust count if it exceeds bounds
    for I := StartIndex to StartIndex + ACount - 1 do
      if FComparer(FList[I], Item) = 0 then
      begin
        Result := I;
        Break;                                                  // Item found, exit loop
      end;
  finally
    FLock.Release;                                              // Exit critical section
  end;
end;

function TThreadSafeList.LastIndexOf(const Item: T): Integer;
begin
  Result := LastIndexOf(Item, FCount - 1);                     // Delegate to overloaded method
end;

function TThreadSafeList.LastIndexOf(const Item: T; StartIndex: Integer): Integer;
begin
  Result := LastIndexOf(Item, StartIndex, StartIndex + 1);    // Delegate to overloaded method
end;

function TThreadSafeList.LastIndexOf(const Item: T; StartIndex, ACount: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  FLock.Acquire;                                               // Enter critical section
  try
    if (FCount = 0) or (StartIndex < 0) then
      Exit;                                                    // Invalid conditions

    StartIndex := Min(StartIndex, FCount - 1);                 // Adjust start index if out of bounds
    ACount := Min(ACount, StartIndex + 1);                     // Adjust count if it exceeds bounds

    // Iterate backwards to find the last occurrence
    for I := StartIndex downto StartIndex - ACount + 1 do
      if FComparer(FList[I], Item) = 0 then
      begin
        Result := I;
        Break;                                                  // Item found, exit loop
      end;
  finally
    FLock.Release;                                              // Exit critical section
  end;
end;

procedure TThreadSafeList.MoveItem(CurIndex, NewIndex: Integer);
var
  Item: T;
begin
  FLock.Acquire;                                               // Enter critical section
  try
    if (CurIndex < 0) or (CurIndex >= FCount) or
       (NewIndex < 0) or (NewIndex >= FCount) then
      raise EArgumentOutOfRangeException.Create('Index out of bounds');

    if CurIndex <> NewIndex then
    begin
      Item := FList[CurIndex];                                 // Store the item to move
      if NewIndex < CurIndex then
        Move(FList[NewIndex], 
             FList[NewIndex + 1], 
             (CurIndex - NewIndex) * SizeOf(T))                // Shift items to make space
      else
        Move(FList[CurIndex + 1], 
             FList[CurIndex], 
             (NewIndex - CurIndex) * SizeOf(T));                // Shift items to fill the gap

      FList[NewIndex] := Item;                                  // Place the item at the new index
      FSorted := False;                                         // List is no longer sorted
    end;
  finally
    FLock.Release;                                              // Exit critical section
  end;
end;

{ TThreadSafeList implementation - Additional methods }

procedure TThreadSafeList.RaiseIfOutOfBounds(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EArgumentOutOfRangeException.Create('Index out of bounds');   // Raise exception for invalid index
end;

procedure TThreadSafeList.Insert(Index: Integer; const Item: T);
begin
  FLock.Acquire;                                               // Enter critical section
  try
    if (Index < 0) or (Index > FCount) then
      raise EArgumentOutOfRangeException.Create('Index out of bounds');

    if FCount = FCapacity then
      Grow;                                                   // Grow the list if capacity is reached

    if Index < FCount then
      System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(T));
                                                              // Shift items to make space for the new item

    FList[Index] := Item;                                     // Insert the new item
    Inc(FCount);                                              // Update the count
    FSorted := False;                                         // List is no longer sorted
  finally
    FLock.Release;                                            // Exit critical section
  end;
end;

procedure TThreadSafeList.Exchange(Index1, Index2: Integer);
var
  Temp: T;
begin
  FLock.Acquire;                                               // Enter critical section
  try
    RaiseIfOutOfBounds(Index1);                                // Validate first index
    RaiseIfOutOfBounds(Index2);                                // Validate second index

    if Index1 <> Index2 then
    begin
      Temp := FList[Index1];                                   // Swap the items
      FList[Index1] := FList[Index2];
      FList[Index2] := Temp;
      FSorted := False;                                       // List is no longer sorted
    end;
  finally
    FLock.Release;                                               // Exit critical section
  end;
end;

procedure TThreadSafeList.Reverse;
var
  I: Integer;
  Temp: T;
begin
  FLock.Acquire;                                               // Enter critical section
  try
    // Swap elements from both ends towards the center
    for I := 0 to (FCount div 2) - 1 do
    begin
      Temp := FList[I];
      FList[I] := FList[FCount - 1 - I];
      FList[FCount - 1 - I] := Temp;
    end;
    FSorted := False;                                         // List is no longer sorted
  finally
    FLock.Release;                                            // Exit critical section
  end;
end;

function TThreadSafeList.Extract(const Item: T): T;
var
  Index: Integer;
begin
  FLock.Acquire;                                               // Enter critical section
  try
    Index := IndexOf(Item);                                    // Find the item's index
    if Index = -1 then
      raise EArgumentOutOfRangeException.Create('Item not found');
    Result := FList[Index];                                    // Retrieve the item
    Delete(Index);                                             // Remove the item from the list
  finally
    FLock.Release;                                             // Exit critical section
  end;
end;

function TThreadSafeList.ExtractAt(Index: Integer): T;
begin
  FLock.Acquire;                                               // Enter critical section
  try
    RaiseIfOutOfBounds(Index);                                 // Validate index
    Result := FList[Index];                                    // Retrieve the item
    Delete(Index);                                             // Remove the item from the list
  finally
    FLock.Release;                                             // Exit critical section
  end;
end;

procedure TThreadSafeList.TrimExcess;
begin
  FLock.Acquire;                                               // Enter critical section
  try
    if FCount < FCapacity then
      SetCapacity(FCount);                                     // Reduce capacity to match the count
  finally
    FLock.Release;                                             // Exit critical section
  end;
end;

end.
 
