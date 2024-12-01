unit ThreadSafeCollections.Dictionary;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes, SyncObjs, HashFunctions, TypInfo, ThreadSafeCollections.Interfaces;

{ 
  DEBUG_LOGGING: Global flag to control debug output
  - When True: Outputs detailed operation logs
  - When False: No debug output (use for production)
}
const
  DEBUG_LOGGING = False;

type
  { 
    TDictionaryEntry: Generic record representing a key-value pair in the hash table
    - Key: The lookup key
    - Value: The stored value
    - Hash: Cached hash value to avoid recalculation during resize
    - Next: Pointer to next entry (for handling collisions via chaining)
  }
  generic TDictionaryEntry<TKey, TValue> = record
    Key: TKey;
    Value: TValue;
    Hash: cardinal;
    Next: ^TDictionaryEntry;
  end;

  // First declare TDictionaryPair
  generic TDictionaryPair<TKey, TValue> = record
    Key: TKey;
    Value: TValue;
  end;

  // Then declare hash function types
  generic THashFunction<T> = function(const Key: T): Cardinal;
  generic TEqualityComparison<T> = function(const Left, Right: T): Boolean;

  {
    TThreadSafeDictionary: Thread-safe generic dictionary implementation
    - Uses critical section for thread safety
    - Implements separate chaining for collision resolution
    - Automatic resizing when load factor threshold is reached
  }
  generic TThreadSafeDictionary<TKey, TValue> = class
  private
  type
    PEntry = ^TEntry;
    TEntry = record
      Key: TKey;
      Value: TValue;
      Hash: Cardinal;
      Next: PEntry;
    end;

    // Single enumerator class
    TEnumerator = class
    private
      FDictionary: TThreadSafeDictionary;
      FCurrentBucket: Integer;
      FCurrentEntry: PEntry;
      FLockToken: ILockToken;
      function GetCurrent: specialize TDictionaryPair<TKey, TValue>;
    public
      constructor Create(ADictionary: TThreadSafeDictionary);
      destructor Destroy; override;
      function MoveNext: Boolean;
      property Current: specialize TDictionaryPair<TKey, TValue> read GetCurrent;
    end;

  private
  const
    INITIAL_BUCKET_COUNT = 16;   // Initial size of hash table
    LOAD_FACTOR = 0.75;          // Threshold for resizing (75% full)
    MIN_BUCKET_COUNT = 4;        // Minimum number of buckets

  private
    FLock: TCriticalSection;     // Thread synchronization
    FBuckets: array of PEntry;   // Array of bucket heads
    FCount: integer;             // Number of items in dictionary
    FHashFunc: specialize THashFunction<TKey>;
    FEqualityComparer: specialize TEqualityComparison<TKey>;

    { Internal methods for hash table operations }
    function GetHashValue(const Key: TKey): cardinal;
    function GetBucketIndex(Hash: cardinal): integer; inline;
    procedure Resize(NewSize: integer);
    procedure CheckLoadFactor;
    function FindEntry(const Key: TKey; Hash: cardinal; BucketIdx: integer): PEntry;
    function GetNextPowerOfTwo(Value: integer): integer;
    function CompareKeys(const Left, Right: TKey): Boolean;
  public
    constructor Create;
    constructor Create(InitialCapacity: integer);
    constructor Create(const AHashFunc: specialize THashFunction<TKey>;
                      const AEqualityComparer: specialize TEqualityComparison<TKey>);
    constructor Create(InitialCapacity: integer;
                      const AHashFunc: specialize THashFunction<TKey>;
                      const AEqualityComparer: specialize TEqualityComparison<TKey>);
    destructor Destroy; override;

    procedure Add(const Key: TKey; const Value: TValue);
    function TryGetValue(const Key: TKey; out Value: TValue): boolean;
    function Remove(const Key: TKey): boolean;
    procedure Replace(const Key: TKey; const Value: TValue);

    function First(out Key: TKey; out Value: TValue): boolean;
    function Last(out Key: TKey; out Value: TValue): boolean;
    function Find(const Key: TKey): TValue;

    procedure Clear;
    function Count: integer; 
    procedure ResizeBuckets(NewSize: integer);
    function GetBucketCount: integer; 
    property BucketCount: integer read GetBucketCount; 
    property Items[const Key: TKey]: TValue read Find write Replace; default;
    function GetEnumerator: TEnumerator;
    function Lock: ILockToken;
  end;

implementation

{ TThreadSafeDictionary implementation }

{
  GetNextPowerOfTwo: Returns the next power of 2 >= Value
  - Ensures bucket count is always a power of 2
  - Required for efficient hash distribution
}
function TThreadSafeDictionary.GetNextPowerOfTwo(Value: integer): integer;
begin
  Result := MIN_BUCKET_COUNT;
  while Result < Value do
    Result := Result * 2;
end;


{
  Create: Default constructor
  - Initializes with DEFAULT_BUCKET_COUNT buckets
  - Uses built-in hash functions for basic types
}
constructor TThreadSafeDictionary.Create;
begin
  Create(INITIAL_BUCKET_COUNT, nil, nil);  // Pass nil for hash and equality functions
end;

{
  Create: Constructor with initial capacity
  - Adjusts capacity to next power of 2
  - Ensures minimum bucket count
  - Uses built-in hash functions for basic types
  
  Parameters:
  - InitialCapacity: Desired initial bucket count
}
constructor TThreadSafeDictionary.Create(InitialCapacity: integer);
begin
  Create(InitialCapacity, nil, nil);  // Pass nil for hash and equality functions
end;

{
  Create: Constructor with custom hash and equality functions
  - Uses default bucket count
  - Required for compound/custom types
  
  Parameters:
  - AHashFunc: Custom hash function for the key type
  - AEqualityComparer: Custom equality comparison for the key type
}
constructor TThreadSafeDictionary.Create(const AHashFunc: specialize THashFunction<TKey>;
                                       const AEqualityComparer: specialize TEqualityComparison<TKey>);
begin
  Create(INITIAL_BUCKET_COUNT, AHashFunc, AEqualityComparer);
end;

{
  Create: Full constructor with all options
  - Adjusts capacity to next power of 2
  - Ensures minimum bucket count
  - Can use custom hash and equality functions
  
  Parameters:
  - InitialCapacity: Desired initial bucket count (will be rounded up to power of 2)
  - AHashFunc: Custom hash function (nil uses built-in hash)
  - AEqualityComparer: Custom equality comparison (nil uses built-in comparison)
}
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

{
  GetBucketCount: Returns the current number of buckets (not key-value pairs)
  - Thread-safe operation
  - Accesses FBuckets directly
}
function TThreadSafeDictionary.GetBucketCount: integer;
begin
  FLock.Enter;
  try
    Result := Length(FBuckets);
  finally
    FLock.Leave;
  end;
end;

{
  ResizeBuckets: Public method to manually resize bucket array
  - Thread-safe operation
  - Adjusts size to next power of 2
  - Won't resize smaller than minimum bucket count
  - Won't resize smaller than needed for current items
  
  Parameters:
  - NewSize: Desired new bucket count
  
  Raises:
  - Exception if new size too small for current items
}
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

{
  GetHashValue: Calculates hash for different key types
  - Uses XXHash32 for strings (good distribution)
  - Uses MultiplicativeHash for integers
  - Falls back to DefaultHash for other types
  - Always returns a positive value (masked with $7FFFFFFF)
}
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

{
  GetBucketIndex: Maps hash value to bucket index
  - Uses bitwise AND with (bucket_count - 1)
  - Requires bucket count to be power of 2
  - Provides fast modulo operation
}
function TThreadSafeDictionary.GetBucketIndex(Hash: cardinal): integer;
begin
  if DEBUG_LOGGING then WriteLn(
      Format('DEBUG: GetBucketIndex - Hash: %d, Buckets Length: %d',
      [Hash, Length(FBuckets)]));
  Result := Hash and (Length(FBuckets) - 1);
  if DEBUG_LOGGING then WriteLn(Format('DEBUG: GetBucketIndex - Result: %d', [Result]));
end;

{
  CheckLoadFactor: Monitors and maintains hash table efficiency
  - Calculates current load (items/buckets)
  - Triggers resize when load > 0.75
  - Doubles bucket count on resize
  
  Note: Called after each Add operation
}
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

{
  Resize: Doubles the hash table size and rehashes all entries
  - Creates new bucket array
  - Rehashes existing entries to new positions
  - Maintains linked lists for collision chains
  - Uses cached hash values to avoid recalculation
  
  Important: Must be called within a lock
}
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

{
  FindEntry: Internal method to locate an entry in a specific bucket
  - Uses cached hash value for initial comparison (optimization)
  - Traverses collision chain if needed
  - Returns nil if key not found
  
  Parameters:
  - Key: The key to find
  - Hash: Pre-calculated hash value
  - BucketIdx: Pre-calculated bucket index
  
  Note: Caller must hold lock
}
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

{
  Add: Inserts a new key-value pair
  - Thread-safe operation (uses lock)
  - Checks for duplicate keys
  - Handles collision chaining
  - Triggers resize if needed
  
  Raises:
  - Exception if key already exists
}
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


{
  Find: Retrieves value for given key
  - Thread-safe operation
  - Raises exception if key not found
  
  Returns: Value associated with key
  
  Raises:
  - Exception if key not found
}
function TThreadSafeDictionary.Find(const Key: TKey): TValue;
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

    if Entry = nil then
      raise Exception.Create('Key not found');
    Result := Entry^.Value;
  finally
    FLock.Leave;
  end;
end;

{
  Remove: Deletes entry with given key
  - Thread-safe operation
  - Handles linked list maintenance
  - Updates count
  
  Returns:
  - True if key found and removed
  - False if key not found
}
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

{
  Replace: Updates value for existing key
  - Thread-safe operation
  - Does not change bucket structure
  - Raises exception if key doesn't exist
  
  Parameters:
  - Key: The key to update
  - Value: New value to store
  
  Raises:
  - Exception if key not found
}
procedure TThreadSafeDictionary.Replace(const Key: TKey; const Value: TValue);
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

    if Entry = nil then
      raise Exception.Create('Key not found')
    else
      Entry^.Value := Value;
  finally
    FLock.Leave;
  end;
end;


{
  First: Retrieves first key-value pair in dictionary
  - Thread-safe operation
  - Traverses buckets from start
  - Returns first non-empty bucket's first entry
  
  Parameters:
  - Key: Output parameter for found key
  - Value: Output parameter for found value
  
  Returns:
  - True if dictionary not empty and pair found
  - False if dictionary empty
}
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

{
  Last: Retrieves last key-value pair in dictionary
  - Thread-safe operation
  - Traverses buckets from end
  - Returns last non-empty bucket's first entry
  
  Parameters:
  - Key: Output parameter for found key
  - Value: Output parameter for found value
  
  Returns:
  - True if dictionary not empty and pair found
  - False if dictionary empty
}
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

{
  Clear: Removes all entries from dictionary
  - Thread-safe operation
  - Properly disposes of all entries
  - Maintains bucket array but empties it
  - Resets count to 0
  
  Note: Bucket array size remains unchanged
}
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

{
  Count: Returns current number of items
  - Thread-safe operation
  - Simple accessor for FCount
  
  Returns:
  - Current number of key-value pairs in dictionary
}
function TThreadSafeDictionary.Count: integer;
begin
  FLock.Enter;
  try
    Result := FCount;
  finally
    FLock.Leave;
  end;
end;

{
  TryGetValue: Attempts to retrieve value for key
  - Thread-safe operation
  - Non-throwing alternative to Find
  
  Parameters:
  - Key: The key to look up
  - Value: Output parameter for found value
  
  Returns:
  - True if key found and value retrieved
  - False if key not found
}
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

function TThreadSafeDictionary.TEnumerator.GetCurrent: specialize TDictionaryPair<TKey, TValue>;
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

end.
