{*******************************************************}
{                                                       }
{       Thread-safe Dictionary Implementation           }
{                                                       }
{       Copyright (C) 2024                              }
{       Version: 0.5                                    }
{                                                       }
{*******************************************************}

{
  This unit provides a thread-safe implementation of a generic dictionary
  that can be used in multi-threaded applications. It uses a hash table
  with separate chaining for collision resolution and provides automatic
  resizing when the load factor threshold is reached.

  Features:
  - Thread-safe operations using critical sections
  - Separate chaining for collision handling
  - Automatic resizing when load factor exceeds 0.75
  - Support for custom hash functions and equality comparers
  - Compatible with Delphi's TDictionary interface
  - RAII-style locking mechanism

  Usage example:
    var
      Dict: specialize TThreadSafeDictionary<string, integer>;
      Pair: specialize TPair<string, integer>;
    begin
      Dict := TThreadSafeDictionary<string, integer>.Create;
      try
        Dict.Add('one', 1);
        Dict.AddOrSetValue('two', 2);
        
        // Using TPair from Generics.Collections
        for Pair in Dict do
          WriteLn(Format('%s: %d', [Pair.Key, Pair.Value]));
      finally
        Dict.Free;
      end;
    end;
}

unit ThreadSafeCollections.Dictionary;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes, SyncObjs, HashFunctions, TypInfo, 
  ThreadSafeCollections.Interfaces, Generics.Collections;

{ 
  DEBUG_LOGGING: Global flag to control debug output
  - When True: Outputs detailed operation logs
  - When False: No debug output (use for production)
}
const
  DEBUG_LOGGING = False;

type
  { EKeyNotFoundException
    Custom exception raised when attempting to access a non-existent key }
  EKeyNotFoundException = class(Exception);

  { TDictionaryEntry
    Internal record representing a key-value pair in the hash table

    Parameters:
      TKey - The type of the dictionary keys
      TValue - The type of the dictionary values

    Fields:
      Key: The lookup key
      Value: The stored value
      Hash: Cached hash value to avoid recalculation during resize
      Next: Pointer to next entry (for handling collisions via chaining) }
  generic TDictionaryEntry<TKey, TValue> = record
    Key: TKey;
    Value: TValue;
    Hash: cardinal;
    Next: ^TDictionaryEntry;
  end;

  { THashFunction
    Function type for custom key hashing

    Parameters:
      T - The type of the key to hash
    Returns:
      Cardinal - The hash value for the key }
  generic THashFunction<T> = function(const Key: T): Cardinal;

  { TEqualityComparison
    Function type for custom key comparison

    Parameters:
      T - The type of the keys to compare
      Left, Right - The keys to compare
    Returns:
      Boolean - True if keys are equal, False otherwise }
  generic TEqualityComparison<T> = function(const Left, Right: T): Boolean;

  { TThreadSafeDictionary
    Thread-safe implementation of a generic dictionary

    Parameters:
      TKey - The type of the dictionary keys
      TValue - The type of the dictionary values

    Thread Safety:
      All public methods are thread-safe using a critical section
      The Lock method provides RAII-style locking for bulk operations }
  generic TThreadSafeDictionary<TKey, TValue> = class(TInterfacedObject, 
    specialize IThreadSafeDictionary<TKey, TValue>)
  private
    type
      // PEntry is a pointer to a TEntry record, representing a single entry in the hash table
      PEntry = ^TEntry;

      // TEntry represents a key-value pair stored in the dictionary along with its hash and a pointer to the next entry (for collision resolution)
      TEntry = record
        Key: TKey;                         // The key associated with the value
        Value: TValue;                     // The value associated with the key
        Hash: Cardinal;                    // Cached hash value of the key to optimize lookups
        Next: PEntry;                      // Pointer to the next entry in the same bucket (in case of hash collisions)
      end;

      // TEnumerator is a helper class to enable iteration over the dictionary's key-value pairs
      TEnumerator = class
      private
        FDictionary: TThreadSafeDictionary;          // Reference to the dictionary being enumerated
        FCurrentBucket: Integer;                     // Index of the current bucket being iterated
        FCurrentEntry: PEntry;                       // Pointer to the current entry in the bucket
        FLockToken: ILockToken;                      // Token for managing thread-safe access during enumeration

        // Retrieves the current key-value pair
        function GetCurrent: specialize TPair<TKey, TValue>;


      public
        // Constructor initializes the enumerator with a reference to the dictionary
        constructor Create(ADictionary: TThreadSafeDictionary);

        // Destructor cleans up any resources
        destructor Destroy; override;

        // Advances the enumerator to the next key-value pair
        function MoveNext: Boolean;

        // Property to access the current key-value pair
        property Current: specialize TPair<TKey, TValue> read GetCurrent;
      end;

  private
    const
      INITIAL_BUCKET_COUNT = 16;   // Initial number of buckets in the hash table
      LOAD_FACTOR = 0.75;          // Load factor threshold to trigger resizing (75% full)
      MIN_BUCKET_COUNT = 4;        // Minimum number of buckets to maintain

  private
    FLock: TCriticalSection;     // Critical section object to ensure thread safety during operations
    FBuckets: array of PEntry;   // Dynamic array of bucket heads; each bucket is a linked list of entries
    FCount: integer;             // Current number of key-value pairs stored in the dictionary
    FHashFunc: specialize THashFunction<TKey>;             // Custom hash function for hashing keys
    FEqualityComparer: specialize TEqualityComparison<TKey>; // Custom equality comparison function for keys

    { 
      Internal methods for hash table operations 
    }

    { GetHashValue
      Computes the hash value for a given key
      
      Parameters:
        Key: The key to hash
      
      Returns:
        A positive cardinal value representing the hash
      
      Notes:
        - Uses custom hash function if provided
        - Falls back to type-specific hash for basic types
        - Ensures result is always positive }
    function GetHashValue(const Key: TKey): cardinal;

    { GetBucketIndex
      Maps a hash value to a bucket index
      
      Parameters:
        Hash: The hash value to map
      
      Returns:
        Index of the bucket for this hash
      
      Notes:
        - Uses bitwise AND for efficient modulo operation
        - Requires bucket count to be power of 2 }
    function GetBucketIndex(Hash: cardinal): integer; inline;

    { Resize
      Resizes the hash table and rehashes all entries
      
      Parameters:
        NewSize: The new size for the bucket array
      
      Notes:
        - Must be called within a lock
        - Preserves all existing entries
        - Maintains collision chains }
    procedure Resize(NewSize: integer);

    { CheckLoadFactor
      Monitors and maintains hash table efficiency
      
      Notes:
        - Called after each Add operation
        - Triggers resize when load > LOAD_FACTOR
        - Must be called within a lock }
    procedure CheckLoadFactor;

    { FindEntry
      Locates an entry in a specific bucket
      
      Parameters:
        Key: The key to find
        Hash: Pre-calculated hash value
        BucketIdx: Pre-calculated bucket index
      
      Returns:
        Pointer to found entry or nil if not found
      
      Notes:
        - Must be called within a lock
        - Uses cached hash for optimization }
    function FindEntry(const Key: TKey; Hash: cardinal; BucketIdx: integer): PEntry;

    { FindValue
      Searches for a specific value in the dictionary
      
      Parameters:
        Value: The value to find
      
      Returns:
        True if value exists, False otherwise
      
      Notes:
        - Must be called within a lock
        - Performs byte-by-byte comparison }
    function FindValue(const Value: TValue): Boolean;

    // Calculates the next power of two greater than or equal to the provided value
    function GetNextPowerOfTwo(Value: integer): integer;

    // Compares two keys for equality using the equality comparer
    function CompareKeys(const Left, Right: TKey): Boolean;

    // Private helper methods
    function GetCount: Integer;  // Add this for interface

    procedure SetItem(const Key: TKey; const Value: TValue);  // Add this for interface

  public
    { Create
      Creates a new dictionary with default settings
      
      The default constructor initializes the dictionary with:
      - Initial capacity of 16 buckets
      - Default hash function based on key type
      - Default equality comparison based on key type }
    constructor Create;

    { Create
      Creates a new dictionary with specified initial capacity
      
      Parameters:
        InitialCapacity: Desired initial bucket count (will be rounded up to next power of 2)
      
      Notes:
        Actual capacity will be at least MIN_BUCKET_COUNT (4) }
    constructor Create(InitialCapacity: integer);

    { Create
      Creates a new dictionary with custom hash and equality functions
      
      Parameters:
        AHashFunc: Custom function to compute hash values for keys
        AEqualityComparer: Custom function to compare keys for equality
      
      Use this constructor when working with compound keys or when
      custom hash/equality behavior is needed }
    constructor Create(const AHashFunc: specialize THashFunction<TKey>;
                      const AEqualityComparer: specialize TEqualityComparison<TKey>);

    { Create
      Creates a new dictionary with specified capacity and custom functions
      
      Parameters:
        InitialCapacity: Desired initial bucket count (will be rounded up to next power of 2)
        AHashFunc: Custom function to compute hash values for keys
        AEqualityComparer: Custom function to compare keys for equality
      
      This is the most flexible constructor, allowing full customization of both
      initial capacity and key handling behavior }
    constructor Create(InitialCapacity: integer;
                      const AHashFunc: specialize THashFunction<TKey>;
                      const AEqualityComparer: specialize TEqualityComparison<TKey>);

    { Destroy
      Cleans up all resources used by the dictionary
      
      Notes:
        - Automatically called when the dictionary is freed
        - Releases all memory used by entries
        - Releases the critical section }
    destructor Destroy; override;

    { Add
      Adds a new key-value pair to the dictionary
      
      Parameters:
        Key: The key to add
        Value: The value to associate with the key
      
      Raises:
        Exception if the key already exists
      
      Thread Safety:
        Method is thread-safe }
    procedure Add(const Key: TKey; const Value: TValue);

    { GetItem
      Retrieves the value associated with a key
      
      Parameters:
        Key: The key to look up
      
      Returns:
        The value associated with the key
      
      Raises:
        EKeyNotFoundException if the key doesn't exist
      
      Thread Safety:
        Method is thread-safe }
    function GetItem(const Key: TKey): TValue;

    { TryGetValue
      Attempts to retrieve a value for the specified key
      
      Parameters:
        Key: The key to look up
        Value: Output parameter that receives the found value
      
      Returns:
        True if the key was found, False otherwise
      
      Thread Safety:
        Method is thread-safe }
    function TryGetValue(const Key: TKey; out Value: TValue): boolean;

    { Remove
      Removes a key-value pair from the dictionary
      
      Parameters:
        Key: The key to remove
      
      Returns:
        True if the key was found and removed
        False if the key wasn't found
      
      Thread Safety:
        Method is thread-safe }
    function Remove(const Key: TKey): boolean;

    { AddOrSetValue
      Adds a new key-value pair or updates an existing one
      
      Parameters:
        Key: The key to add or update
        Value: The value to store
      
      Notes:
        - If the key exists, its value is updated
        - If the key doesn't exist, a new pair is added
      
      Thread Safety:
        Method is thread-safe }
    procedure AddOrSetValue(const Key: TKey; const Value: TValue);

    { First
      Retrieves the first key-value pair in the dictionary
      
      Parameters:
        Key: Output parameter that receives the first key
        Value: Output parameter that receives the first value
      
      Returns:
        True if the dictionary is not empty and a pair was retrieved
        False if the dictionary is empty
      
      Notes:
        - The "first" pair is implementation-dependent
        - Don't rely on any specific ordering
      
      Thread Safety:
        Method is thread-safe }
    function First(out Key: TKey; out Value: TValue): boolean;

    { Last
      Retrieves the last key-value pair in the dictionary
      
      Parameters:
        Key: Output parameter that receives the last key
        Value: Output parameter that receives the last value
      
      Returns:
        True if the dictionary is not empty and a pair was retrieved
        False if the dictionary is empty
      
      Notes:
        - The "last" pair is implementation-dependent
        - Don't rely on any specific ordering
      
      Thread Safety:
        Method is thread-safe }
    function Last(out Key: TKey; out Value: TValue): boolean;

    { Clear
      Removes all key-value pairs from the dictionary
      
      Notes:
        - Maintains the current bucket count
        - Properly disposes of all entries
        - Resets count to 0
      
      Thread Safety:
        Method is thread-safe }
    procedure Clear;

    // Returns the number of key-value pairs currently stored in the dictionary
    function Count: integer; 

    { ResizeBuckets
      Manually resizes the internal bucket array
      
      Parameters:
        NewSize: Desired new bucket count
      
      Notes:
        - Size will be adjusted to next power of 2
        - Won't resize smaller than minimum bucket count
        - Won't resize smaller than needed for current items
      
      Raises:
        Exception if new size is too small for current items
      
      Thread Safety:
        Method is thread-safe }
    procedure ResizeBuckets(NewSize: integer);

    { GetBucketCount
      Returns the current number of buckets in the hash table
      
      Returns:
        The current number of buckets (always a power of 2)
      
      Thread Safety:
        Method is thread-safe }
    function GetBucketCount: integer; 

    // Property to access the number of buckets
    property BucketCount: integer read GetBucketCount; 

    // Default property to access items by key, supports read and write operations
    property Items[const Key: TKey]: TValue read GetItem write AddOrSetValue; default;

    { GetEnumerator
      Creates an enumerator for iterating over the dictionary
      
      Returns:
        A new TEnumerator instance
      
      Notes:
        - Enumerator maintains a lock during iteration
        - Remember to free the enumerator when done
      
      Thread Safety:
        Iteration is thread-safe }
    function GetEnumerator: TEnumerator;

    { Lock
      Acquires a lock on the dictionary using RAII pattern
      
      Returns:
        An ILockToken that automatically releases the lock when freed
      
      Usage:
        var
          LockToken: ILockToken;
        begin
          LockToken := Dict.Lock;  // Lock acquired here
          try
            // Multiple thread-safe operations
            Dict.Add('one', 1);
            Dict.Remove('two');
            Dict.AddOrSetValue('three', 3);
          finally
            LockToken := nil;  // Lock released here
          end;
        end;
      
      Thread Safety:
        Core mechanism for atomic multi-operation sequences }
    function Lock: ILockToken;

    { ContainsKey
      Checks if the dictionary contains the specified key
      
      Parameters:
        Key: The key to check for
        
      Returns:
        True if the key exists, False otherwise
        
      Thread Safety:
        Method is thread-safe }
    function ContainsKey(const Key: TKey): Boolean;

    { GetKeys
      Returns an array containing all keys in the dictionary
      
      Returns:
        Array of TKey containing all dictionary keys
        
      Thread Safety:
        Method is thread-safe }
    function GetKeys: specialize TKeyArray<TKey>;

    { GetValues 
      Returns an array containing all values in the dictionary
      
      Returns:
        Array of TValue containing all dictionary values
        
      Thread Safety:
        Method is thread-safe }
    function GetValues: specialize TValueArray<TValue>;

    { TrimExcess
      Reduces the internal capacity to match the actual item count
      
      Notes:
        - Only trims if significant memory can be saved
        - May take longer with large dictionaries
        
      Thread Safety:
        Method is thread-safe }
    procedure TrimExcess;

    { TryAdd
      Attempts to add a key-value pair if the key doesn't exist
      
      Parameters:
        Key: The key to add
        Value: The value to associate with the key
        
      Returns:
        True if added successfully, False if key already exists
        
      Thread Safety:
        Method is thread-safe }
    function TryAdd(const Key: TKey; const Value: TValue): Boolean;

    { AddRange
      Adds all key-value pairs from another dictionary
      
      Parameters:
        ADictionary: Source dictionary to copy from
        
      Notes:
        - Skips if source dictionary is nil
        - Updates existing keys using AddOrSetValue
        - Thread-safe on both dictionaries
        
      Thread Safety:
        - Method is thread-safe
        - Locks both source and destination dictionaries }
    procedure AddRange(const ADictionary: specialize IThreadSafeDictionary<TKey, TValue>); overload;

    { AddRange
      Adds all key-value pairs from an array
      
      Parameters:
        AArray: Array of key-value pairs to add
        
      Thread Safety:
        Method is thread-safe }
    procedure AddRange(const AArray: specialize TPairArray<TKey, TValue>); overload;

    { ToArray
      Converts the dictionary contents to an array of key-value pairs
      
      Returns:
        Array of TPair containing all dictionary entries
        
      Thread Safety:
        Method is thread-safe }
    function ToArray: specialize TPairArray<TKey, TValue>;

    { ContainsValue
      Checks if the dictionary contains the specified value
      
      Parameters:
        Value: The value to search for
        
      Returns:
        True if value exists, False otherwise
        
      Notes:
        - This operation requires scanning all entries
        - May be slow for large dictionaries
        
      Thread Safety:
        Method is thread-safe }
    function ContainsValue(const Value: TValue): Boolean;
  end;

implementation


{ TThreadSafeDictionary implementation }

function TThreadSafeDictionary.GetNextPowerOfTwo(Value: integer): integer;
begin
  Result := MIN_BUCKET_COUNT;
  while Result < Value do
    Result := Result * 2;
end;

constructor TThreadSafeDictionary.Create;
begin
  Create(INITIAL_BUCKET_COUNT, nil, nil);  // Pass nil for hash and equality functions
end;

constructor TThreadSafeDictionary.Create(InitialCapacity: integer);
begin
  Create(InitialCapacity, nil, nil);  // Pass nil for hash and equality functions
end;


constructor TThreadSafeDictionary.Create(const AHashFunc: specialize THashFunction<TKey>;
                                       const AEqualityComparer: specialize TEqualityComparison<TKey>);
begin
  Create(INITIAL_BUCKET_COUNT, AHashFunc, AEqualityComparer);
end;

constructor TThreadSafeDictionary.Create(InitialCapacity: integer;
                                       const AHashFunc: specialize THashFunction<TKey>;
                                       const AEqualityComparer: specialize TEqualityComparison<TKey>);
var
  AdjustedSize:Integer;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  
  // Store the custom functions or use defaults
  FHashFunc := AHashFunc;
  FEqualityComparer := AEqualityComparer;
  
  // Ensure power of 2 and minimum size
  AdjustedSize := GetNextPowerOfTwo(InitialCapacity);
  SetLength(FBuckets, AdjustedSize);
  FCount := 0;
end;


destructor TThreadSafeDictionary.Destroy;
begin
  Clear;  // Clean up all entries before destroying
  FLock.Free;
  inherited Destroy;
end;

function TThreadSafeDictionary.GetBucketCount: integer;
begin
  FLock.Enter;
  try
    Result := Length(FBuckets);
  finally
    FLock.Leave;
  end;
end;


procedure TThreadSafeDictionary.ResizeBuckets(NewSize: integer);
var
  MinRequired: integer;
  AdjustedSize: integer;
begin
  FLock.Enter;
  try
    // Calculate minimum size needed for current items
    MinRequired := Trunc(FCount / LOAD_FACTOR) + 1;
    
    // Ensure new size is adequate
    if NewSize < MinRequired then
      raise Exception.CreateFmt(
        'New size (%d) too small for current item count. Minimum required: %d',
        [NewSize, MinRequired]);
    
    // Adjust to next power of 2 and ensure minimum
    AdjustedSize := GetNextPowerOfTwo(NewSize);
    
    if DEBUG_LOGGING then
      WriteLn(Format('ResizeBuckets: Adjusting requested size %d to %d',
          [NewSize, AdjustedSize]));
    
    // Perform the resize
    Resize(AdjustedSize);
  finally
    FLock.Leave;
  end;
end;


function TThreadSafeDictionary.GetHashValue(const Key: TKey): cardinal;
begin
  // Use custom hash function if provided
  if Assigned(FHashFunc) then
    Result := FHashFunc(Key)
  else
  begin
    // Original type-specific hash logic
    if TypeInfo(TKey) = TypeInfo(string) then
      Result := XXHash32(string((@Key)^))
    else if TypeInfo(TKey) = TypeInfo(integer) then
      Result := MultiplicativeHash(cardinal(integer((@Key)^)))
    else
      Result := DefaultHash(Key);
  end;
  
  Result := Result and $7FFFFFFF; // Ensure positive
end;


function TThreadSafeDictionary.GetBucketIndex(Hash: cardinal): integer;
begin
  if DEBUG_LOGGING then WriteLn(
      Format('DEBUG: GetBucketIndex - Hash: %d, Buckets Length: %d',
      [Hash, Length(FBuckets)]));
  Result := Hash and (Length(FBuckets) - 1);
  if DEBUG_LOGGING then WriteLn(Format('DEBUG: GetBucketIndex - Result: %d', [Result]));
end;


procedure TThreadSafeDictionary.CheckLoadFactor;
begin
  if DEBUG_LOGGING then WriteLn(Format('CheckLoadFactor: Current ratio: %f',
      [FCount / Length(FBuckets)]));
  if (FCount / Length(FBuckets)) > LOAD_FACTOR then
  begin
    if DEBUG_LOGGING then WriteLn('CheckLoadFactor: Resizing needed');
    Resize(Length(FBuckets) * 2);
    if DEBUG_LOGGING then WriteLn('CheckLoadFactor: Resize complete');
  end;
end;


procedure TThreadSafeDictionary.Resize(NewSize: integer);
var
  OldBuckets: array of PEntry;
  Entry, Next: PEntry;
  I, NewBucketIdx: integer;
begin
  if DEBUG_LOGGING then WriteLn(Format('Resize: Starting resize from %d to %d',
      [Length(FBuckets), NewSize]));

  // Store old buckets and create new array
  OldBuckets := FBuckets;
  SetLength(FBuckets, NewSize);

  // Initialize new buckets to nil
  for I := 0 to High(FBuckets) do
    FBuckets[I] := nil;

  // Rehash all existing entries
  for I := 0 to High(OldBuckets) do
  begin
    Entry := OldBuckets[I];
    while Entry <> nil do
    begin
      Next := Entry^.Next;  // Save next pointer before modifying entry

      // Calculate new bucket index using cached hash
      NewBucketIdx := Entry^.Hash and (NewSize - 1);

      // Insert at beginning of new bucket (prepend)
      Entry^.Next := FBuckets[NewBucketIdx];
      FBuckets[NewBucketIdx] := Entry;

      Entry := Next;  // Move to next entry in original chain
    end;
  end;

  if DEBUG_LOGGING then WriteLn('Resize: Complete');
end;


function TThreadSafeDictionary.FindEntry(const Key: TKey; Hash: cardinal;
  BucketIdx: integer): PEntry;
var
  Entry: PEntry;
begin
  Entry := FBuckets[BucketIdx];
  while Entry <> nil do
  begin
    if (Entry^.Hash = Hash) and CompareKeys(Entry^.Key, Key) then
      Exit(Entry);
    Entry := Entry^.Next;
  end;
  Result := nil;
end;


procedure TThreadSafeDictionary.Add(const Key: TKey; const Value: TValue);
var
  Hash: cardinal;
  BucketIdx: integer;
  NewEntry: PEntry;
begin
  if DEBUG_LOGGING then WriteLn('Add: Before lock');
  FLock.Enter;
  if DEBUG_LOGGING then WriteLn('Add: Lock acquired');
  try
    if DEBUG_LOGGING then WriteLn(Format('DEBUG: Add - Current bucket array size: %d',
        [Length(FBuckets)]));
    Hash := GetHashValue(Key);
    if DEBUG_LOGGING then WriteLn(Format('Add: Raw hash value: %d', [integer(Hash)]));
    Hash := Hash and $7FFFFFFF;  // Double-check positive value
    if DEBUG_LOGGING then WriteLn(Format('Add: Masked hash value: %d', [integer(Hash)]));

    BucketIdx := GetBucketIndex(Hash);
    if DEBUG_LOGGING then WriteLn(Format('Add: Got bucket index: %d', [BucketIdx]));

    if FindEntry(Key, Hash, BucketIdx) <> nil then
    begin
      if DEBUG_LOGGING then WriteLn('Add: Found duplicate key');
      raise Exception.Create('Duplicate key');
    end;

    if DEBUG_LOGGING then WriteLn('Add: Creating new entry');
    New(NewEntry);
    NewEntry^.Key := Key;
    NewEntry^.Value := Value;
    NewEntry^.Hash := Hash;  // Store the masked hash

    NewEntry^.Next := FBuckets[BucketIdx];
    FBuckets[BucketIdx] := NewEntry;

    Inc(FCount);
    CheckLoadFactor;
  finally
    if DEBUG_LOGGING then WriteLn('Add: Releasing lock');
    FLock.Leave;
  end;
end;

function TThreadSafeDictionary.Remove(const Key: TKey): boolean;
var
  Hash: cardinal;
  BucketIdx: integer;
  Entry, Prev: PEntry;
begin
  Result := False;
  FLock.Enter;
  try
    Hash := GetHashValue(Key);
    BucketIdx := GetBucketIndex(Hash);

    Entry := FBuckets[BucketIdx];
    Prev := nil;

    while Entry <> nil do
    begin
      if (Entry^.Hash = Hash) and CompareKeys(Entry^.Key, Key) then
      begin
        if Prev = nil then
          FBuckets[BucketIdx] := Entry^.Next
        else
          Prev^.Next := Entry^.Next;

        Dispose(Entry);
        Dec(FCount);
        Result := True;
        Exit;
      end;
      Prev := Entry;
      Entry := Entry^.Next;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeDictionary.AddOrSetValue(const Key: TKey; const Value: TValue);
var
  Hash: Cardinal;
  BucketIdx: Integer;
  Entry: PEntry;
begin
  FLock.Enter;
  try
    Hash := GetHashValue(Key);
    BucketIdx := GetBucketIndex(Hash);
    Entry := FindEntry(Key, Hash, BucketIdx);
    if Entry <> nil then
      Entry^.Value := Value
    else
      Add(Key, Value);
  finally
    FLock.Leave;
  end;
end;


function TThreadSafeDictionary.First(out Key: TKey; out Value: TValue): boolean;
var
  I: integer;
begin
  Result := False;
  FLock.Enter;
  try
    for I := 0 to Length(FBuckets) - 1 do
      if FBuckets[I] <> nil then
      begin
        Key := FBuckets[I]^.Key;
        Value := FBuckets[I]^.Value;
        Result := True;
        Exit;
      end;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeDictionary.Last(out Key: TKey; out Value: TValue): boolean;
var
  I: integer;
begin
  Result := False;
  FLock.Enter;
  try
    for I := Length(FBuckets) - 1 downto 0 do
      if FBuckets[I] <> nil then
      begin
        Key := FBuckets[I]^.Key;
        Value := FBuckets[I]^.Value;
        Result := True;
        Exit;
      end;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeDictionary.Clear;
var
  I: integer;
  Entry, Next: PEntry;
begin
  FLock.Enter;
  try
    for I := 0 to Length(FBuckets) - 1 do
    begin
      Entry := FBuckets[I];
      while Entry <> nil do
      begin
        Next := Entry^.Next;
        Dispose(Entry);
        Entry := Next;
      end;
      FBuckets[I] := nil;
    end;
    FCount := 0;
  finally
    FLock.Leave;
  end;
end;


function TThreadSafeDictionary.Count: integer;
begin
  FLock.Enter;
  try
    Result := FCount;
  finally
    FLock.Leave;
  end;
end;


function TThreadSafeDictionary.TryGetValue(const Key: TKey; out Value: TValue): boolean;
var
  Hash: cardinal;
  BucketIdx: integer;
  Entry: PEntry;
begin
  FLock.Enter;
  try
    Hash := GetHashValue(Key);
    BucketIdx := GetBucketIndex(Hash);
    Entry := FindEntry(Key, Hash, BucketIdx);

    if Entry <> nil then
    begin
      Value := Entry^.Value;
      Result := True;
    end
    else
      Result := False;
  finally
    FLock.Leave;
  end;
end;

{ TThreadSafeDictionary.TEnumerator }

constructor TThreadSafeDictionary.TEnumerator.Create(ADictionary: TThreadSafeDictionary);
begin
  inherited Create;
  FDictionary := ADictionary;
  FCurrentBucket := -1;  // Start before first bucket
  FCurrentEntry := nil;
  FLockToken := FDictionary.Lock;
end;

destructor TThreadSafeDictionary.TEnumerator.Destroy;
begin
  FLockToken := nil; // Release lock
  inherited;
end;

function TThreadSafeDictionary.TEnumerator.GetCurrent: specialize TPair<TKey, TValue>;
begin
  if FCurrentEntry = nil then
    raise Exception.Create('Invalid enumerator position');
  Result.Key := FCurrentEntry^.Key;
  Result.Value := FCurrentEntry^.Value;
end;

function TThreadSafeDictionary.TEnumerator.MoveNext: Boolean;
begin
  Result := False;
  FDictionary.FLock.Enter;
  try
    // If we have more entries in current bucket
    if (FCurrentEntry <> nil) and (FCurrentEntry^.Next <> nil) then
    begin
      FCurrentEntry := FCurrentEntry^.Next;
      Result := True;
      Exit;
    end;

    // Find next non-empty bucket
    while FCurrentBucket < Length(FDictionary.FBuckets) - 1 do
    begin
      Inc(FCurrentBucket);
      if FDictionary.FBuckets[FCurrentBucket] <> nil then
      begin
        FCurrentEntry := FDictionary.FBuckets[FCurrentBucket];
        Result := True;
        Exit;
      end;
    end;
  finally
    FDictionary.FLock.Leave;
  end;
end;

function TThreadSafeDictionary.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TThreadSafeDictionary.Lock: ILockToken;
begin
  Result := TLockToken.Create(FLock);
end;

{ Helper function to compare keys safely }
function TThreadSafeDictionary.CompareKeys(const Left, Right: TKey): Boolean;
begin
  if Assigned(FEqualityComparer) then
    Result := FEqualityComparer(Left, Right)
  else
    Result := CompareByte(Left, Right, SizeOf(TKey)) = 0;
end;

function TThreadSafeDictionary.GetCount: Integer;
begin
  FLock.Enter;
  try
    Result := FCount;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeDictionary.GetItem(const Key: TKey): TValue;
var
  Hash: Cardinal;
  BucketIdx: Integer;
  Entry: PEntry;
begin
  FLock.Enter;
  try
    Hash := GetHashValue(Key);
    BucketIdx := GetBucketIndex(Hash);
    Entry := FindEntry(Key, Hash, BucketIdx);
    if Entry = nil then
      raise EKeyNotFoundException.Create('Key not found');
    Result := Entry^.Value;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeDictionary.SetItem(const Key: TKey; const Value: TValue);
begin
  AddOrSetValue(Key, Value);
end;

function TThreadSafeDictionary.ContainsKey(const Key: TKey): Boolean;
var
  Hash: Cardinal;
  BucketIdx: Integer;
begin
  FLock.Enter;
  try
    Hash := GetHashValue(Key);
    BucketIdx := GetBucketIndex(Hash);
    Result := FindEntry(Key, Hash, BucketIdx) <> nil;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeDictionary.GetKeys: specialize TKeyArray<TKey>;
var
  I: Integer;
  Entry: PEntry;
  Index: Integer;
begin
  FLock.Enter;
  try
    SetLength(Result, FCount);
    Index := 0;
    for I := 0 to Length(FBuckets) - 1 do
    begin
      Entry := FBuckets[I];
      while Entry <> nil do
      begin
        Result[Index] := Entry^.Key;
        Inc(Index);
        Entry := Entry^.Next;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeDictionary.GetValues: specialize TValueArray<TValue>;
var
  I: Integer;
  Entry: PEntry;
  Index: Integer;
begin
  FLock.Enter;
  try
    SetLength(Result, FCount);
    Index := 0;
    for I := 0 to Length(FBuckets) - 1 do
    begin
      Entry := FBuckets[I];
      while Entry <> nil do
      begin
        Result[Index] := Entry^.Value;
        Inc(Index);
        Entry := Entry^.Next;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeDictionary.TrimExcess;
var
  NewSize: Integer;
begin
  FLock.Enter;
  try
    NewSize := GetNextPowerOfTwo(Trunc(FCount / LOAD_FACTOR) + 1);
    if NewSize < Length(FBuckets) then
      Resize(NewSize);
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeDictionary.TryAdd(const Key: TKey; const Value: TValue): Boolean;
var
  Hash: Cardinal;
  BucketIdx: Integer;
  NewEntry: PEntry;
begin
  Result := False;
  FLock.Enter;
  try
    Hash := GetHashValue(Key);
    BucketIdx := GetBucketIndex(Hash);
    
    if FindEntry(Key, Hash, BucketIdx) <> nil then
      Exit;
      
    New(NewEntry);
    NewEntry^.Key := Key;
    NewEntry^.Value := Value;
    NewEntry^.Hash := Hash;
    NewEntry^.Next := FBuckets[BucketIdx];
    FBuckets[BucketIdx] := NewEntry;
    
    Inc(FCount);
    CheckLoadFactor;
    Result := True;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeDictionary.AddRange(const ADictionary: specialize IThreadSafeDictionary<TKey, TValue>);
var
  LockToken: ILockToken;
  Keys: specialize TArray<TKey>;
  I: Integer;
  Value: TValue;
begin
  if ADictionary = nil then
    Exit;
    
  LockToken := ADictionary.Lock;
  try
    Keys := ADictionary.GetKeys;
    for I := 0 to Length(Keys) - 1 do
    begin
      if ADictionary.TryGetValue(Keys[I], Value) then
        AddOrSetValue(Keys[I], Value);
    end;
  finally
    LockToken := nil;
  end;
end;

procedure TThreadSafeDictionary.AddRange(const AArray: specialize TPairArray<TKey, TValue>);
var
  I: Integer;
begin
  FLock.Enter;
  try
    for I := Low(AArray) to High(AArray) do
      AddOrSetValue(AArray[I].Key, AArray[I].Value);
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeDictionary.ToArray: specialize TPairArray<TKey, TValue>;
var
  I: Integer;
  Entry: PEntry;
  Index: Integer;
begin
  FLock.Enter;
  try
    SetLength(Result, FCount);
    Index := 0;
    for I := 0 to Length(FBuckets) - 1 do
    begin
      Entry := FBuckets[I];
      while Entry <> nil do
      begin
        Result[Index].Key := Entry^.Key;
        Result[Index].Value := Entry^.Value;
        Inc(Index);
        Entry := Entry^.Next;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeDictionary.FindValue(const Value: TValue): Boolean;
var
  I: Integer;
  Entry: PEntry;
begin
  Result := False;
  for I := 0 to Length(FBuckets) - 1 do
  begin
    Entry := FBuckets[I];
    while Entry <> nil do
    begin
      if CompareByte(Entry^.Value, Value, SizeOf(TValue)) = 0 then
        Exit(True);
      Entry := Entry^.Next;
    end;
  end;
end;

function TThreadSafeDictionary.ContainsValue(const Value: TValue): Boolean;
begin
  FLock.Enter;
  try
    Result := FindValue(Value);
  finally
    FLock.Leave;
  end;
end;

end.