unit ThreadSafeCollections.HashSet;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, SyncObjs, HashFunctions, TypInfo, ThreadSafeCollections.Interfaces;

type
  // Function type for comparing two values of type T for equality
  generic TEqualityComparer<T> = function(const A, B: T): Boolean;
  
  // Function type for generating hash codes for values of type T
  generic THashFunction<T> = function(const Value: T): Cardinal;

  { TThreadSafeHashSet<T>: A generic thread-safe hash set implementation
    - Uses separate chaining for collision resolution
    - Automatically resizes when load factor exceeds threshold
    - Thread-safe for all operations }
  { 
    TThreadSafeHashSet<T>: 
      A generic thread-safe hash set implementation.
      
      This class manages a collection of unique items of type T, ensuring thread safety
      for concurrent operations. It utilizes separate chaining for collision resolution
      and automatically resizes the underlying bucket array when the load factor exceeds
      a predefined threshold. This implementation is suitable for scenarios requiring
      high-performance set operations in multi-threaded environments.
      
      Features:
        - Separate chaining for resolving hash collisions.
        - Automatic resizing based on load factor to maintain performance.
        - Thread-safe operations for adding, removing, and searching items.
  }
  generic TThreadSafeHashSet<T> = class(TInterfacedObject, specialize IThreadSafeHashSet<T>)
  private
    type
      // PEntry is a pointer to a TEntry record, representing an individual entry in the hash set.
      PEntry = ^TEntry;
      
      { 
        TEntry: 
          Represents a single entry in the hash set.
          
          Each entry stores the value of type T, its precomputed hash code for performance,
          and a pointer to the next entry in the chain to handle hash collisions.
      }
      TEntry = record
        Value: T;        // The actual stored value in the hash set.
        Hash: Cardinal;  // Cached hash code of the value to optimize lookups.
        Next: PEntry;    // Pointer to the next entry in the chain (for handling collisions).
      end;

      { 
        TEnumerator: 
          Provides enumeration capabilities for iterating over the hash set's elements.
          
          This internal class allows users to traverse the hash set in a thread-safe manner,
          ensuring that the set's state remains consistent during iteration.
      }
      TEnumerator = class
      private
        FSet: TThreadSafeHashSet;    // Reference to the parent hash set being enumerated.
        FCurrentBucket: Integer;     // Index of the current bucket in the buckets array.
        FCurrentEntry: PEntry;       // Pointer to the current entry within the current bucket.
        FLockToken: ILockToken;      // Token managing the lock to ensure thread safety during enumeration.
      public
        constructor Create(ASet: TThreadSafeHashSet); // Initializes the enumerator with the target hash set.
        destructor Destroy; override;                  // Cleans up resources when the enumerator is destroyed.
        function GetCurrent: T;                        // Retrieves the current element in the enumeration.
        function MoveNext: Boolean;                    // Advances the enumerator to the next element.
        property Current: T read GetCurrent;            // Read-only property to access the current element.
      end;

      { 
        Local array type for internal use only
        Using specialize TArray<T> for better compatibility with FPC's generic collections
      }
      _TArray = specialize TArray<T>;

    const
      INITIAL_BUCKET_COUNT = 16;   // Default number of buckets in the hash table upon initialization.
      LOAD_FACTOR = 0.75;          // Threshold ratio to determine when to resize the hash table (75% full).
      MIN_BUCKET_COUNT = 4;        // Minimum number of buckets allowed to prevent excessive shrinking.

    private
      FBuckets: array of PEntry;   // Dynamic array holding pointers to the head of each bucket's entry chain.
      FCount: Integer;             // Current number of unique items stored in the hash set.
      FLock: TCriticalSection;     // Critical section to synchronize access and ensure thread safety.
      FEqualityComparer: specialize TEqualityComparer<T>;  // Delegate for comparing two items for equality.
      FHashFunction: specialize THashFunction<T>;          // Delegate for computing the hash code of an item.

      { 
        GetBucketIndex: 
          Maps a given hash code to an index within the buckets array.
          
          This function uses bitwise AND to ensure the hash code maps within the valid range
          of bucket indices, facilitating efficient access to the corresponding bucket.
          
          Parameters:
            - Hash: The hash code of an item.
          
          Returns:
            - The index of the bucket corresponding to the provided hash code.
      }
      function GetBucketIndex(Hash: Cardinal): Integer; inline;
      
      { 
        Resize: 
          Expands or contracts the bucket array to accommodate more or fewer items.
          
          When the number of items exceeds the load factor threshold, this procedure
          increases the number of buckets to maintain efficient operation and minimize
          collision chains. Resizing involves rehashing existing items to distribute them
          across the new bucket array.
          
          Parameters:
            - NewSize: The desired number of buckets after resizing.
      }
      procedure Resize(NewSize: Integer);
      
      { 
        CheckLoadFactor: 
          Evaluates whether the current load factor exceeds the predefined threshold.
          
          If the load factor (FCount / number of buckets) surpasses LOAD_FACTOR, the hash set
          triggers a resize to maintain optimal performance and prevent long collision chains.
      }
      procedure CheckLoadFactor;
      
      { 
        FindEntry: 
          Searches for a specific item within a designated bucket's entry chain.
          
          This function traverses the linked list of entries in the specified bucket to locate
          an entry matching the provided item based on its hash code and equality comparer.
          
          Parameters:
            - Item: The item to search for in the hash set.
            - Hash: The precomputed hash code of the item.
            - BucketIdx: The index of the bucket where the item should reside based on its hash code.
          
          Returns:
            - A pointer to the entry if found; otherwise, nil.
      }
      function FindEntry(const Item: T; Hash: Cardinal; BucketIdx: Integer): PEntry;

      { 
        GetNextPowerOfTwo: 
          Calculates the next highest power of two for a given integer value.
          
          This utility function ensures that the bucket array size remains a power of two,
          which optimizes the distribution of hash codes and simplifies index calculations.
          
          Parameters:
            - Value: The integer for which to find the next power of two.
          
          Returns:
            - The smallest power of two that is greater than or equal to Value.
      }
      function GetNextPowerOfTwo(Value: Integer): Integer;

      { 
        GetCount: 
          Retrieves the current number of items stored in the hash set.
          
          This function provides a thread-safe way to access the FCount field.
          
          Returns:
            - The total number of unique items in the set.
      }
      function GetCount: Integer;

  public
    { 
      Create: 
        Constructor to initialize a new instance of TThreadSafeHashSet.
        
        This constructor sets up the hash set with the provided equality and hash functions,
        and initializes the bucket array based on the initial capacity.
        
        Parameters:
          - AEqualityComparer: A delegate function that compares two items of type T for equality.
          - AHashFunction: A delegate function that computes the hash code for an item of type T.
          - AInitialCapacity: Optional parameter to specify the initial number of buckets. Defaults to INITIAL_BUCKET_COUNT.
    }
    constructor Create(AEqualityComparer: specialize TEqualityComparer<T>;
                      AHashFunction: specialize THashFunction<T>;
                      AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); 

    { 
      Destroy: 
        Destructor to clean up resources used by the hash set.
        
        This procedure releases all allocated memory for entries and ensures that
        the critical section is properly disposed of to prevent resource leaks.
    }
    destructor Destroy; override;

    // Core operations

    { 
      Add: 
        Adds a new item to the hash set.
        
        This function inserts the item if it does not already exist in the set. It computes
        the item's hash code, determines the appropriate bucket, and appends the item to the
        bucket's entry chain. If the item is successfully added, the function returns True.
        
        Parameters:
          - Item: The item to be added to the hash set.
        
        Returns:
          - True if the item was newly added; False if the item already existed in the set.
    }
    function Add(const Item: T): Boolean;      // Returns true if item was newly added

    { 
      Remove: 
        Removes an existing item from the hash set.
        
        This function searches for the specified item and, if found, removes it from the set.
        It updates the entry chain accordingly and frees any associated memory. Returns True
        if the item was successfully removed.
        
        Parameters:
          - Item: The item to be removed from the hash set.
        
        Returns:
          - True if the item was found and removed; False otherwise.
    }
    function Remove(const Item: T): Boolean;   // Returns true if item was found and removed

    { 
      Contains: 
        Checks whether a specific item exists within the hash set.
        
        This function determines if the provided item is present by computing its hash code
        and searching the corresponding bucket's entry chain using the equality comparer.
        
        Parameters:
          - Item: The item to search for in the hash set.
        
        Returns:
          - True if the item exists in the set; False otherwise.
    }
    function Contains(const Item: T): Boolean; // Returns true if item exists

    { 
      Clear: 
        Removes all items from the hash set, resetting it to an empty state.
        
        This procedure iterates through all buckets, freeing each entry and clearing the
        bucket array. It resets the item count to zero, effectively emptying the set.
    }
    procedure Clear;                           // Removes all items

    { 
      Count: 
        Read-only property to access the number of items currently stored in the hash set.
        
        This property provides a thread-safe way to retrieve the count of items without
        modifying the set.
    }
    property Count: Integer read GetCount;       // Number of items in set

    { 
      GetEnumerator: 
        Provides an enumerator for iterating over the hash set's elements.
        
        This function returns an instance of TEnumerator, allowing users to traverse the
        set in a controlled and thread-safe manner.
        
        Returns:
          - An enumerator object for the hash set.
    }
    function GetEnumerator: TEnumerator;

    { 
      Lock: 
        Acquires a lock on the hash set to ensure exclusive access.
        
        This function returns an ILockToken that manages the lifecycle of the lock. When
        the token is destroyed or released, the lock is automatically released, ensuring
        that critical sections are properly managed.
        
        Returns:
          - An ILockToken instance representing the acquired lock.
    }
    function Lock: ILockToken;

    // IThreadSafeCollection methods

    { 
      IsEmpty: 
        Determines whether the hash set contains any items.
        
        This function provides a quick way to check if the set is empty without iterating
        through its elements.
        
        Returns:
          - True if the hash set has no items; False otherwise.
    }
    function IsEmpty: Boolean;

    // IThreadSafeHashSet methods

    { 
      ToArray: 
        Converts the hash set's elements into a dynamic array.
        
        This function creates an array containing all the items in the set, facilitating
        operations that require array-based data structures or interoperability with
        functions that accept arrays.
        
        Returns:
          - A dynamic array of type T containing all items from the hash set.
    }
    function ToArray: specialize TArray<T>;

    { 
      AddRange (array overload):
        Adds multiple items from an array to the hash set.
        
        This procedure efficiently adds multiple items at once while maintaining thread safety.
        Duplicate items are handled according to hash set rules.
        
        Parameters:
          - Items: Array of type T containing elements to add
    }
    procedure AddRange(const Items: array of T);

    { 
      AddRange (collection overload):
        Adds all items from another thread-safe hash set to this set.
        
        This procedure safely transfers items between sets while maintaining thread safety
        for both collections. Duplicate items are handled according to hash set rules.
        
        Parameters:
          - Collection: Source IThreadSafeHashSet to copy items from
    }
    procedure AddRange(const Collection: specialize IThreadSafeHashSet<T>);

    { 
      RemoveRange (array overload):
        Removes multiple items specified in an array from the hash set.
        
        This function attempts to remove each item in the array and counts successful removals.
        
        Parameters:
          - Items: Array of type T containing elements to remove
        Returns:
          - Number of items successfully removed
    }
    function RemoveRange(const Items: array of T): Integer;

    { 
      RemoveRange (collection overload):
        Removes all items present in another thread-safe hash set from this set.
        
        This function safely removes items that exist in both sets while maintaining
        thread safety.
        
        Parameters:
          - Collection: Source IThreadSafeHashSet containing items to remove
        Returns:
          - Number of items successfully removed
    }
    function RemoveRange(const Collection: specialize IThreadSafeHashSet<T>): Integer;

    { 
      TryGetValue:
        Attempts to retrieve a value from the set that matches the input item.
        
        This function is useful when the type T has additional properties beyond
        those used for equality comparison.
        
        Parameters:
          - Item: The item to search for
          - Value: Output parameter that receives the found value
        Returns:
          - True if the item was found; False otherwise
    }
    function TryGetValue(const Item: T; out Value: T): Boolean;

    { 
      IntersectWith:
        Modifies the current set to contain only elements present in both sets.
        
        This procedure performs a set intersection operation, keeping only items
        that exist in both this set and the provided collection.
        
        Parameters:
          - Collection: The IThreadSafeHashSet to intersect with
    }
    procedure IntersectWith(const Collection: specialize IThreadSafeHashSet<T>);

    { 
      UnionWith:
        Modifies the current set to contain all unique elements from both sets.
        
        This procedure performs a set union operation, adding all items from the
        provided collection while maintaining uniqueness.
        
        Parameters:
          - Collection: The IThreadSafeHashSet to union with
    }
    procedure UnionWith(const Collection: specialize IThreadSafeHashSet<T>);

    { 
      ExceptWith:
        Removes all elements in the specified collection from the current set.
        
        This procedure performs a set difference operation, removing any items
        that exist in the provided collection.
        
        Parameters:
          - Collection: The IThreadSafeHashSet containing items to remove
    }
    procedure ExceptWith(const Collection: specialize IThreadSafeHashSet<T>);

    { 
      Overlaps:
        Determines whether the current set and another collection share any elements.
        
        This function checks if there is at least one common element between the sets.
        
        Parameters:
          - Collection: The IThreadSafeHashSet to check for common elements
        Returns:
          - True if the sets have at least one element in common; False otherwise
    }
    function Overlaps(const Collection: specialize IThreadSafeHashSet<T>): Boolean;

    { 
      SetEquals:
        Determines whether the current set contains exactly the same elements as another collection.
        
        This function verifies that both sets have the same size and contain the same elements.
        
        Parameters:
          - Collection: The IThreadSafeHashSet to compare with
        Returns:
          - True if both sets contain exactly the same elements; False otherwise
    }
    function SetEquals(const Collection: specialize IThreadSafeHashSet<T>): Boolean;
  end;

  { Specialized hash set types with predefined hash and equality functions }
  
  { TThreadSafeHashSetInteger:
      Thread-safe hash set specialized for Integer values.
      Uses IntegerEquals for equality comparison and IntegerHash for hashing.
      Provides a simple way to store unique integer values in a thread-safe manner. }
  TThreadSafeHashSetInteger = class(specialize TThreadSafeHashSet<Integer>)
  public
    { Default constructor using standard integer hash function and equality comparer.
      Parameters:
        - AInitialCapacity: Initial size of the internal bucket array. Will be
                           rounded up to the next power of 2. Default is INITIAL_BUCKET_COUNT. }
    constructor Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); overload;
  end;

  { TThreadSafeHashSetString:
      Thread-safe hash set specialized for string values.
      Uses StringEquals for equality comparison and StringHash for hashing.
      
      The default constructor uses XXHash32 for string hashing, which provides:
      - Fast hashing performance
      - Good distribution characteristics
      - Low collision rates for typical string data
      
      A custom hash function can be provided via the overloaded constructor,
      which is particularly useful for testing collision handling and
      other edge cases in the hash set implementation. }
  TThreadSafeHashSetString = class(specialize TThreadSafeHashSet<string>)
  public
    { Default constructor using standard string hash function (XXHash32) }
    constructor Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); overload;
    
    { Special constructor that allows injection of custom hash function.
      Primary use: Testing hash collision scenarios
      Example: ForceCollisionHash function that returns constant value ($DEADBEEF)
               to force all items into same bucket }
    constructor Create(AHashFunction: specialize THashFunction<string>; 
                      AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); overload;
  end;

  { TThreadSafeHashSetBoolean:
      Thread-safe hash set specialized for Boolean values.
      Uses BooleanEquals for equality comparison and BooleanHash for hashing.
      Note: Since Boolean only has two possible values, this is mainly useful
            for completeness and consistency with other primitive types. }
  TThreadSafeHashSetBoolean = class(specialize TThreadSafeHashSet<Boolean>)
  public
    constructor Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); overload;
  end;

  { TThreadSafeHashSetReal:
      Thread-safe hash set specialized for floating point Real values.
      Uses RealEquals for equality comparison and RealHash for hashing.
      Note: Due to floating point precision, be careful when using Real values
            as hash keys. Small rounding differences may cause unexpected behavior. }
  TThreadSafeHashSetReal = class(specialize TThreadSafeHashSet<Real>)
  public
    constructor Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); overload;
  end;

// Basic equality comparers for primitive types
function IntegerEquals(const A, B: Integer): Boolean;
function StringEquals(const A, B: string): Boolean;
function BooleanEquals(const A, B: Boolean): Boolean;
function RealEquals(const A, B: Real): Boolean;

// Hash functions for primitive types
function IntegerHash(const Value: Integer): Cardinal;
function StringHash(const Value: string): Cardinal;
function BooleanHash(const Value: Boolean): Cardinal;
function RealHash(const Value: Real): Cardinal;

implementation

{ TThreadSafeHashSet }

// Basic equality comparers - straightforward comparisons
function IntegerEquals(const A, B: Integer): Boolean;
begin
  Result := A = B;
end;

function StringEquals(const A, B: string): Boolean;
begin
  Result := A = B;
end;

function BooleanEquals(const A, B: Boolean): Boolean;
begin
  Result := A = B;
end;

function RealEquals(const A, B: Real): Boolean;
begin
  Result := A = B;
end;

// Hash functions for different types
function IntegerHash(const Value: Integer): Cardinal;
begin
  // Use multiplicative hash for integers (good distribution for sequential values)
  Result := MultiplicativeHash(Cardinal(Value));
end;

function StringHash(const Value: string): Cardinal;
begin
  // XXHash32 provides good distribution and performance for strings
  Result := XXHash32(Value);
end;

function BooleanHash(const Value: Boolean): Cardinal;
begin
  // Simple hash for boolean - just convert to number
  Result := MultiplicativeHash(Cardinal(Value));
end;

function RealHash(const Value: Real): Cardinal;
var
  IntValue: Int64;
begin
  // Convert to fixed-point (6 decimal places) then hash
  IntValue := Int64(Trunc(Value * 1000000));
  Result := DefaultHash(IntValue);
end;

constructor TThreadSafeHashSet.Create(AEqualityComparer: specialize TEqualityComparer<T>;
                                    AHashFunction: specialize THashFunction<T>;
                                    AInitialCapacity: Integer = INITIAL_BUCKET_COUNT);
begin
  // Initialize with thread safety and hash functions
  FLock := TCriticalSection.Create;
  FEqualityComparer := AEqualityComparer;
  FHashFunction := AHashFunction;
  FCount := 0;
  SetLength(FBuckets, GetNextPowerOfTwo(AInitialCapacity));
end;

destructor TThreadSafeHashSet.Destroy;
begin
  Clear;
  FLock.Free;
  inherited;
end;

function TThreadSafeHashSet.GetBucketIndex(Hash: Cardinal): Integer;
begin
  Result := Hash and (Length(FBuckets) - 1);
end;

function TThreadSafeHashSet.Add(const Item: T): Boolean;
var
  Hash: Cardinal;
  BucketIdx: Integer;
  Entry: PEntry;
begin
  Result := False;
  FLock.Acquire;
  try
    Hash := FHashFunction(Item);
    BucketIdx := GetBucketIndex(Hash);
    
    // Check if item already exists
    if FindEntry(Item, Hash, BucketIdx) <> nil then
      Exit;

    // Add new entry
    New(Entry);
    Entry^.Value := Item;
    Entry^.Hash := Hash;
    Entry^.Next := FBuckets[BucketIdx];
    FBuckets[BucketIdx] := Entry;
    Inc(FCount);
    
    CheckLoadFactor;
    Result := True;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeHashSet.Contains(const Item: T): Boolean;
var
  Hash: Cardinal;
  BucketIdx: Integer;
begin
  FLock.Acquire;
  try
    Hash := FHashFunction(Item);
    BucketIdx := GetBucketIndex(Hash);
    Result := FindEntry(Item, Hash, BucketIdx) <> nil;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeHashSet.Remove(const Item: T): Boolean;
var
  Hash: Cardinal;
  BucketIdx: Integer;
  Current, Previous: PEntry;
begin
  Result := False;
  FLock.Acquire;
  try
    Hash := FHashFunction(Item);
    BucketIdx := GetBucketIndex(Hash);
    
    Current := FBuckets[BucketIdx];
    Previous := nil;
    
    while Current <> nil do
    begin
      if (Current^.Hash = Hash) and FEqualityComparer(Current^.Value, Item) then
      begin
        if Previous = nil then
          FBuckets[BucketIdx] := Current^.Next
        else
          Previous^.Next := Current^.Next;
          
        Dispose(Current);
        Dec(FCount);
        Result := True;
        Exit;
      end;
      Previous := Current;
      Current := Current^.Next;
    end;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeHashSet.FindEntry(const Item: T; Hash: Cardinal; BucketIdx: Integer): PEntry;
var
  Current: PEntry;
begin
  Result := nil;
  Current := FBuckets[BucketIdx];
  while Current <> nil do
  begin
    if (Current^.Hash = Hash) and FEqualityComparer(Current^.Value, Item) then
    begin
      Result := Current;
      Exit;
    end;
    Current := Current^.Next;
  end;
end;

procedure TThreadSafeHashSet.CheckLoadFactor;
begin
  if FCount > Length(FBuckets) * LOAD_FACTOR then
    Resize(Length(FBuckets) * 2);
end;

function TThreadSafeHashSet.GetNextPowerOfTwo(Value: Integer): Integer;
begin
  Result := MIN_BUCKET_COUNT;
  while Result < Value do
    Result := Result * 2;
end;

procedure TThreadSafeHashSet.Resize(NewSize: Integer);
var
  OldBuckets: array of PEntry;
  I: Integer;
  Current, Next: PEntry;
  NewBucketIdx: Integer;
begin
  NewSize := GetNextPowerOfTwo(NewSize);
  OldBuckets := FBuckets;
  SetLength(FBuckets, NewSize);
  
  for I := 0 to Length(FBuckets) - 1 do
    FBuckets[I] := nil;
    
  for I := 0 to Length(OldBuckets) - 1 do
  begin
    Current := OldBuckets[I];
    while Current <> nil do
    begin
      Next := Current^.Next;
      NewBucketIdx := GetBucketIndex(Current^.Hash);
      Current^.Next := FBuckets[NewBucketIdx];
      FBuckets[NewBucketIdx] := Current;
      Current := Next;
    end;
  end;
end;

{ TThreadSafeHashSet.TEnumerator }

constructor TThreadSafeHashSet.TEnumerator.Create(ASet: TThreadSafeHashSet);
begin
  inherited Create;
  FSet := ASet;
  FLockToken := FSet.Lock;
  FCurrentBucket := -1;
  FCurrentEntry := nil;
end;

destructor TThreadSafeHashSet.TEnumerator.Destroy;
begin
  FLockToken := nil; // Release lock
  inherited;
end;

function TThreadSafeHashSet.TEnumerator.GetCurrent: T;
begin
  if FCurrentEntry = nil then
    raise EInvalidOperation.Create('Invalid enumerator position');
  Result := FCurrentEntry^.Value;
end;

function TThreadSafeHashSet.TEnumerator.MoveNext: Boolean;
begin
  Result := False;
  
  // If we have a current entry, try its next entry first
  if FCurrentEntry <> nil then
    FCurrentEntry := FCurrentEntry^.Next;
    
  // If we need a new entry, search through buckets
  while (FCurrentEntry = nil) and (FCurrentBucket < Length(FSet.FBuckets) - 1) do
  begin
    Inc(FCurrentBucket);
    FCurrentEntry := FSet.FBuckets[FCurrentBucket];
  end;
  
  Result := FCurrentEntry <> nil;
end;

function TThreadSafeHashSet.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TThreadSafeHashSet.Lock: ILockToken;
begin
  Result := TLockToken.Create(FLock);
end;

// Specialized types with their own constructors
constructor TThreadSafeHashSetInteger.Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT);
var
  EqualityComparer: specialize TEqualityComparer<Integer>;
  HashFunc: specialize THashFunction<Integer>;
begin
  EqualityComparer := @IntegerEquals;
  HashFunc := @IntegerHash;
   inherited Create(EqualityComparer, HashFunc, AInitialCapacity);
end;

constructor TThreadSafeHashSetString.Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT);
var
  EqualityComparer: specialize TEqualityComparer<string>;
  HashFunc: specialize THashFunction<string>;
begin
  EqualityComparer := @StringEquals;
  HashFunc := @StringHash;
  inherited Create(EqualityComparer, HashFunc, AInitialCapacity);
end;

constructor TThreadSafeHashSetString.Create(AHashFunction: specialize THashFunction<string>; 
                                          AInitialCapacity: Integer = INITIAL_BUCKET_COUNT);
var
  EqualityComparer: specialize TEqualityComparer<string>;
begin
  EqualityComparer := @StringEquals;
  inherited Create(EqualityComparer, AHashFunction, AInitialCapacity);
end;

constructor TThreadSafeHashSetBoolean.Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT);
var
  EqualityComparer: specialize TEqualityComparer<Boolean>;
  HashFunc: specialize THashFunction<Boolean>;
begin
  EqualityComparer := @BooleanEquals;
  HashFunc := @BooleanHash;
   inherited Create(EqualityComparer, HashFunc, AInitialCapacity);
end;

constructor TThreadSafeHashSetReal.Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT);
var
  EqualityComparer: specialize TEqualityComparer<Real>;
  HashFunc: specialize THashFunction<Real>;
begin
  EqualityComparer := @RealEquals;
  HashFunc := @RealHash;
   inherited Create(EqualityComparer, HashFunc, AInitialCapacity);
end;

function TThreadSafeHashSet.GetCount: Integer;
begin
  FLock.Acquire;
  try
    Result := FCount;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeHashSet.IsEmpty: Boolean;
begin
  FLock.Acquire;
  try
    Result := FCount = 0;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeHashSet.Clear;
var
  I: Integer;
  Current, Next: PEntry;
begin
  FLock.Acquire;
  try
    for I := 0 to Length(FBuckets) - 1 do
    begin
      Current := FBuckets[I];
      while Current <> nil do
      begin
        Next := Current^.Next;
        Dispose(Current);
        Current := Next;
      end;
      FBuckets[I] := nil;
    end;
    FCount := 0;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeHashSet.ToArray: specialize TArray<T>;
var
  I: Integer;
  Current: PEntry;
  Index: Integer;
  LocalArray: _TArray;
begin
  FLock.Acquire;
  try
    SetLength(LocalArray, FCount);
    Index := 0;
    for I := 0 to Length(FBuckets) - 1 do
    begin
      Current := FBuckets[I];
      while Current <> nil do
      begin
        LocalArray[Index] := Current^.Value;
        Inc(Index);
        Current := Current^.Next;
      end;
    end;
    Result := LocalArray;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeHashSet.AddRange(const Items: array of T);
var
  I: Integer;
begin
  if Length(Items) = 0 then
    Exit;
    
  FLock.Acquire;
  try
    for I := Low(Items) to High(Items) do
    begin
      Add(Items[I]); // Add will handle duplicates
    end;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeHashSet.AddRange(const Collection: specialize IThreadSafeHashSet<T>);
var
  LockToken: ILockToken;
  LocalArray: _TArray;
begin
  if Collection = nil then
    Exit;
    
  LockToken := Collection.Lock;
  LocalArray := Collection.ToArray;
  LockToken := nil;
  
  AddRange(LocalArray);
end;

function TThreadSafeHashSet.RemoveRange(const Items: array of T): Integer;
var
  I: Integer;
begin
  Result := 0;
  if Length(Items) = 0 then
    Exit;
    
  FLock.Acquire;
  try
    for I := Low(Items) to High(Items) do
      if Remove(Items[I]) then
        Inc(Result);
  finally
    FLock.Release;
  end;
end;

function TThreadSafeHashSet.RemoveRange(const Collection: specialize IThreadSafeHashSet<T>): Integer;
var
  LockToken: ILockToken;
  LocalArray: _TArray;
begin
  if Collection = nil then
    Exit(0);
    
  LockToken := Collection.Lock;
  LocalArray := Collection.ToArray;
  LockToken := nil;
  
  Result := RemoveRange(LocalArray);
end;

function TThreadSafeHashSet.TryGetValue(const Item: T; out Value: T): Boolean;
var
  Hash: Cardinal;
  BucketIdx: Integer;
  Entry: PEntry;
begin
  FLock.Acquire;
  try
    Hash := FHashFunction(Item);
    BucketIdx := GetBucketIndex(Hash);
    Entry := FindEntry(Item, Hash, BucketIdx);
    Result := Entry <> nil;
    if Result then
      Value := Entry^.Value;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeHashSet.IntersectWith(const Collection: specialize IThreadSafeHashSet<T>);
var
  ToRemove: _TArray;
  CurrentArray: _TArray;
  I: Integer;
begin
  if Collection = nil then
  begin
    Clear;
    Exit;
  end;

  FLock.Acquire;
  try
    CurrentArray := ToArray;
    SetLength(ToRemove, Length(CurrentArray));
    
    // Find items to remove (those not in other collection)
    for I := 0 to Length(CurrentArray) - 1 do
      if not Collection.Contains(CurrentArray[I]) then
        ToRemove[I] := CurrentArray[I];
        
    // Remove items not in intersection
    RemoveRange(ToRemove);
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeHashSet.UnionWith(const Collection: specialize IThreadSafeHashSet<T>);
begin
  if Collection = nil then
    Exit;
    
  AddRange(Collection);
end;

procedure TThreadSafeHashSet.ExceptWith(const Collection: specialize IThreadSafeHashSet<T>);
var
  LocalArray: _TArray;
begin
  if Collection = nil then
    Exit;
    
  LocalArray := Collection.ToArray;
  RemoveRange(LocalArray);
end;

function TThreadSafeHashSet.Overlaps(const Collection: specialize IThreadSafeHashSet<T>): Boolean;
var
  LocalArray: _TArray;
  I: Integer;
begin
  Result := False;
  if (Collection = nil) or IsEmpty or Collection.IsEmpty then
    Exit;
    
  LocalArray := Collection.ToArray;
  for I := 0 to Length(LocalArray) - 1 do
    if Contains(LocalArray[I]) then
      Exit(True);
end;

function TThreadSafeHashSet.SetEquals(const Collection: specialize IThreadSafeHashSet<T>): Boolean;
var
  OtherArray: _TArray;
  I: Integer;
begin
  Result := False;
  
  if Collection = nil then
    Exit;
    
  if Count <> Collection.Count then
    Exit;
    
  OtherArray := Collection.ToArray;
  for I := 0 to Length(OtherArray) - 1 do
    if not Contains(OtherArray[I]) then
      Exit;
      
  Result := True;
end;

end.
