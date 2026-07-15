{*******************************************************}
{                                                       }
{       Thread-safe Dictionary Implementation           }
{                                                       }
{       Copyright (C) 2024                              }
{       Version: 0.8.4                                  }
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

  NOTE: This implementation shares common hash table patterns with ThreadSafeCollections.HashSet.
        Both use: bucket arrays, GetBucketIndex, Resize, CheckLoadFactor, and entry chaining.
        Future refactoring could extract a common base class to reduce duplication.

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
  SysUtils, Classes, SyncObjs, HashFunctions, TypInfo, Generics.Defaults,
  ThreadSafeCollections.Interfaces, ThreadSafeCollections.ErrorMessages,
  Generics.Collections;


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

      // TEnumerator is a helper class to enable iteration over the dictionary's key-value pairs.
      // It takes a snapshot of all entries at construction time so that modifications to the
      // dictionary during iteration do not cause access violations or corrupt the traversal.
      TEnumerator = class
      private
        type
          TSnapshot = array of specialize TPair<TKey, TValue>;
      private
        FSnapshot: TSnapshot;                        // Snapshot of key-value pairs taken at construction
        FSnapshotIndex: Integer;                     // Current position in the snapshot (-1 = before first)

        // Retrieves the current key-value pair
        function GetCurrent: specialize TPair<TKey, TValue>;

      public
        // Constructor initializes the enumerator and snapshots the dictionary contents
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
      ENTRY_BLOCK_SIZE = 256;      // Entries per allocator block (~8 KB for <string,int>)

    type
      // Key kind cached at construction to avoid repeated TypeInfo pointer comparisons
      // on every hash operation.
      TKeyKind = (kkString, kkInteger, kkOther);

      // Block allocator for TEntry records.
      // Allocates entries in flat blocks of ENTRY_BLOCK_SIZE to avoid per-entry heap
      // calls and improve cache locality.  Freed entries are recycled via a freelist
      // (the entry's own Next pointer is reused as the freelist link).
      // The allocator has no internal lock — callers must hold FLock.
      PEntryBlock = ^TEntryBlock;
      TEntryBlock = record
        Entries: array[0..ENTRY_BLOCK_SIZE - 1] of TEntry;
        Next: PEntryBlock;   // singly-linked list of blocks
      end;

      TEntryAllocator = record
        FBlocks:      PEntryBlock;  // head of the block chain
        FFreeList:    PEntry;       // recycled entries waiting for reuse
        FUsedInBlock: Integer;      // entries consumed from the current (head) block
        procedure Init;
        function  Alloc: PEntry;
        procedure RecycleEntry(Entry: PEntry);
        procedure ReleaseAll;
      end;

  private
    FLock: TCriticalSection;     // Critical section object to ensure thread safety during operations
    FBuckets: array of PEntry;   // Dynamic array of bucket heads; each bucket is a linked list of entries
    FCount: integer;             // Current number of key-value pairs stored in the dictionary
    FHashFunc: specialize THashFunction<TKey>;             // Custom hash function for hashing keys
    FEqualityComparer: specialize TEqualityComparison<TKey>; // Custom equality comparison function for keys
    FDefaultKeyComparer: specialize IEqualityComparer<TKey>; // Type-aware fallback equality for keys
    FValueComparer: specialize IEqualityComparer<TValue>;  // Type-aware default equality for values
    FKeyKind: TKeyKind;          // Cached key type to avoid TypeInfo comparisons per call
    FAllocator: TEntryAllocator; // Slab allocator for TEntry records

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
        - Uses the RTL's type-aware default equality comparer }
    function FindValue(const Value: TValue): Boolean;

    // Calculates the next power of two greater than or equal to the provided value
    function GetNextPowerOfTwo(Value: integer): integer;

    // Compares two keys for equality using the equality comparer
    function CompareKeys(const Left, Right: TKey): Boolean;

    // Private helper methods
    function GetCount: Integer;
    procedure SetItem(const Key: TKey; const Value: TValue);

    // Internal unlocked helper — must only be called while FLock is already held.
    procedure InternalAdd(const Key: TKey; const Value: TValue);

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


{ TEntryAllocator implementation }

procedure TThreadSafeDictionary.TEntryAllocator.Init;
begin
  FBlocks      := nil;
  FFreeList    := nil;
  FUsedInBlock := ENTRY_BLOCK_SIZE;  // force new block on first Alloc
end;

function TThreadSafeDictionary.TEntryAllocator.Alloc: PEntry;
var
  NewBlock: PEntryBlock;
begin
  // Prefer a recycled entry from the freelist
  if FFreeList <> nil then
  begin
    Result    := FFreeList;
    FFreeList := FFreeList^.Next;
    // Entry was finalized in RecycleEntry; re-initialize managed fields
    Initialize(Result^);
    Exit;
  end;

  // Need a fresh slot — allocate a new block if the current one is full
  if FUsedInBlock >= ENTRY_BLOCK_SIZE then
  begin
    New(NewBlock);  // New zeroes the block; FPC runtime initialises managed fields in Entries[]
    NewBlock^.Next := FBlocks;
    FBlocks        := NewBlock;
    FUsedInBlock   := 0;
  end;

  Result := @FBlocks^.Entries[FUsedInBlock];
  Inc(FUsedInBlock);
  // Slot is already zero-initialised by New(NewBlock), no explicit Initialize needed
end;

procedure TThreadSafeDictionary.TEntryAllocator.RecycleEntry(Entry: PEntry);
begin
  Finalize(Entry^);          // release managed-type reference counts (Key, Value)
  Entry^.Next := FFreeList;  // reuse Next as freelist link
  FFreeList   := Entry;
end;

procedure TThreadSafeDictionary.TEntryAllocator.ReleaseAll;
var
  Block, NextBlock: PEntryBlock;
  I: Integer;
begin
  Block := FBlocks;
  while Block <> nil do
  begin
    NextBlock := Block^.Next;
    // Finalize every slot.  Slots never handed out are zero (nil managed fields) — no-op.
    // Slots already recycled were finalized in RecycleEntry; their fields are nil — no-op.
    // Using FreeMem (not Dispose) to avoid the compiler generating a second FinalizeArray
    // over the entire Entries array, which would double-finalize managed fields.
    for I := 0 to ENTRY_BLOCK_SIZE - 1 do
      Finalize(Block^.Entries[I]);
    FreeMem(Block, SizeOf(TEntryBlock));
    Block := NextBlock;
  end;
  FBlocks      := nil;
  FFreeList    := nil;
  FUsedInBlock := ENTRY_BLOCK_SIZE;
end;


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
  FAllocator.Init;

  // Store the custom functions or use defaults
  FHashFunc := AHashFunc;
  FEqualityComparer := AEqualityComparer;
  FDefaultKeyComparer := specialize TEqualityComparer<TKey>.Default;
  FValueComparer := specialize TEqualityComparer<TValue>.Default;

  // Cache key type once so GetHashValue uses a fast case branch instead of
  // TypeInfo pointer comparisons on every single hash call.
  if TypeInfo(TKey) = TypeInfo(string) then
    FKeyKind := kkString
  else if TypeInfo(TKey) = TypeInfo(integer) then
    FKeyKind := kkInteger
  else
    FKeyKind := kkOther;

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
  FLock.Acquire;
  try
    Result := Length(FBuckets);
  finally
    FLock.Release;
  end;
end;


procedure TThreadSafeDictionary.ResizeBuckets(NewSize: integer);
var
  MinRequired: integer;
  AdjustedSize: integer;
begin
  FLock.Acquire;
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
    // Perform the resize
    Resize(AdjustedSize);
  finally
    FLock.Release;
  end;
end;


function TThreadSafeDictionary.GetHashValue(const Key: TKey): cardinal;
begin
  // Use custom hash function if provided
  if Assigned(FHashFunc) then
    Result := FHashFunc(Key)
  else
  begin
    // FKeyKind is set once in the constructor, avoiding repeated TypeInfo pointer
    // comparisons on every hash call.
    case FKeyKind of
      kkString:  Result := XXHash32(string((@Key)^));
      kkInteger: Result := MultiplicativeHash(cardinal(integer((@Key)^)));
    else
      Result := DefaultHash(Key);
    end;
  end;

  Result := Result and $7FFFFFFF; // Ensure positive
end;


function TThreadSafeDictionary.GetBucketIndex(Hash: cardinal): integer;
begin
  Result := Hash and (Length(FBuckets) - 1);
end;


procedure TThreadSafeDictionary.CheckLoadFactor;
begin
  if (FCount / Length(FBuckets)) > LOAD_FACTOR then
    Resize(Length(FBuckets) * 2);
end;


procedure TThreadSafeDictionary.Resize(NewSize: integer);
var
  OldBuckets: array of PEntry;
  Entry, Next: PEntry;
  I, NewBucketIdx: integer;
begin
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


// Internal: add key-value pair without acquiring the lock. Caller must hold FLock.
procedure TThreadSafeDictionary.InternalAdd(const Key: TKey; const Value: TValue);
var
  Hash: cardinal;
  BucketIdx: integer;
  NewEntry: PEntry;
begin
  Hash := GetHashValue(Key);
  BucketIdx := GetBucketIndex(Hash);
  if FindEntry(Key, Hash, BucketIdx) <> nil then
    raise Exception.Create(ERR_DUPLICATE_KEY);
  NewEntry := FAllocator.Alloc;
  NewEntry^.Key := Key;
  NewEntry^.Value := Value;
  NewEntry^.Hash := Hash;
  NewEntry^.Next := FBuckets[BucketIdx];
  FBuckets[BucketIdx] := NewEntry;
  Inc(FCount);
  CheckLoadFactor;
end;

procedure TThreadSafeDictionary.Add(const Key: TKey; const Value: TValue);
begin
  FLock.Acquire;
  try
    InternalAdd(Key, Value);
  finally
    FLock.Release;
  end;
end;

function TThreadSafeDictionary.Remove(const Key: TKey): boolean;
var
  Hash: cardinal;
  BucketIdx: integer;
  Entry, Prev: PEntry;
begin
  Result := False;
  FLock.Acquire;
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

        FAllocator.RecycleEntry(Entry);
        Dec(FCount);
        Result := True;
        Exit;
      end;
      Prev := Entry;
      Entry := Entry^.Next;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeDictionary.AddOrSetValue(const Key: TKey; const Value: TValue);
var
  Hash: Cardinal;
  BucketIdx: Integer;
  Entry: PEntry;
begin
  FLock.Acquire;
  try
    Hash := GetHashValue(Key);
    BucketIdx := GetBucketIndex(Hash);
    Entry := FindEntry(Key, Hash, BucketIdx);
    if Entry <> nil then
      Entry^.Value := Value
    else
      InternalAdd(Key, Value);  // Use internal helper — lock already held
  finally
    FLock.Release;
  end;
end;


function TThreadSafeDictionary.First(out Key: TKey; out Value: TValue): boolean;
var
  I: integer;
begin
  Result := False;
  FLock.Acquire;
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
    FLock.Release;
  end;
end;

function TThreadSafeDictionary.Last(out Key: TKey; out Value: TValue): boolean;
var
  I: integer;
begin
  Result := False;
  FLock.Acquire;
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
    FLock.Release;
  end;
end;

procedure TThreadSafeDictionary.Clear;
var
  I: integer;
  Entry, Next: PEntry;
begin
  FLock.Acquire;
  try
    for I := 0 to Length(FBuckets) - 1 do
    begin
      Entry := FBuckets[I];
      while Entry <> nil do
      begin
        Next := Entry^.Next;
        FAllocator.RecycleEntry(Entry);
        Entry := Next;
      end;
      FBuckets[I] := nil;
    end;
    FCount := 0;
    FAllocator.ReleaseAll;  // bulk-free all backing blocks
  finally
    FLock.Release;
  end;
end;


function TThreadSafeDictionary.Count: integer;
begin
  FLock.Acquire;
  try
    Result := FCount;
  finally
    FLock.Release;
  end;
end;


function TThreadSafeDictionary.TryGetValue(const Key: TKey; out Value: TValue): boolean;
var
  Hash: cardinal;
  BucketIdx: integer;
  Entry: PEntry;
begin
  FLock.Acquire;
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
    FLock.Release;
  end;
end;

{ TThreadSafeDictionary.TEnumerator }

constructor TThreadSafeDictionary.TEnumerator.Create(ADictionary: TThreadSafeDictionary);
var
  LockToken: ILockToken;
  BucketIdx, SnapCount: Integer;
  Entry: PEntry;
begin
  inherited Create;
  FSnapshotIndex := -1;

  // Acquire lock only long enough to copy all entries into a snapshot.
  // Releasing it immediately means concurrent modifications during iteration
  // are allowed but will not cause dangling-pointer access violations.
  LockToken := ADictionary.Lock;
  try
    SetLength(FSnapshot, ADictionary.FCount);
    SnapCount := 0;
    for BucketIdx := 0 to Length(ADictionary.FBuckets) - 1 do
    begin
      Entry := ADictionary.FBuckets[BucketIdx];
      while Entry <> nil do
      begin
        FSnapshot[SnapCount].Key   := Entry^.Key;
        FSnapshot[SnapCount].Value := Entry^.Value;
        Inc(SnapCount);
        Entry := Entry^.Next;
      end;
    end;
    SetLength(FSnapshot, SnapCount);
  finally
    LockToken := nil; // Release lock — snapshot is self-contained
  end;
end;

destructor TThreadSafeDictionary.TEnumerator.Destroy;
begin
  inherited;
end;

function TThreadSafeDictionary.TEnumerator.GetCurrent: specialize TPair<TKey, TValue>;
begin
  if (FSnapshotIndex < 0) or (FSnapshotIndex >= Length(FSnapshot)) then
    raise Exception.Create(ERR_INVALID_ENUMERATOR_POSITION);
  Result := FSnapshot[FSnapshotIndex];
end;

function TThreadSafeDictionary.TEnumerator.MoveNext: Boolean;
begin
  Inc(FSnapshotIndex);
  Result := FSnapshotIndex < Length(FSnapshot);
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
    Result := FDefaultKeyComparer.Equals(Left, Right);
end;

function TThreadSafeDictionary.GetCount: Integer;
begin
  FLock.Acquire;
  try
    Result := FCount;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeDictionary.GetItem(const Key: TKey): TValue;
var
  Hash: Cardinal;
  BucketIdx: Integer;
  Entry: PEntry;
begin
  FLock.Acquire;
  try
    Hash := GetHashValue(Key);
    BucketIdx := GetBucketIndex(Hash);
    Entry := FindEntry(Key, Hash, BucketIdx);
    if Entry = nil then
      raise EKeyNotFoundException.Create(ERR_KEY_NOT_FOUND);
    Result := Entry^.Value;
  finally
    FLock.Release;
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
  FLock.Acquire;
  try
    Hash := GetHashValue(Key);
    BucketIdx := GetBucketIndex(Hash);
    Result := FindEntry(Key, Hash, BucketIdx) <> nil;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeDictionary.GetKeys: specialize TKeyArray<TKey>;
var
  I: Integer;
  Entry: PEntry;
  Index: Integer;
begin
  FLock.Acquire;
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
    FLock.Release;
  end;
end;

function TThreadSafeDictionary.GetValues: specialize TValueArray<TValue>;
var
  I: Integer;
  Entry: PEntry;
  Index: Integer;
begin
  FLock.Acquire;
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
    FLock.Release;
  end;
end;

procedure TThreadSafeDictionary.TrimExcess;
var
  NewSize: Integer;
begin
  FLock.Acquire;
  try
    NewSize := GetNextPowerOfTwo(Trunc(FCount / LOAD_FACTOR) + 1);
    if NewSize < Length(FBuckets) then
      Resize(NewSize);
  finally
    FLock.Release;
  end;
end;

function TThreadSafeDictionary.TryAdd(const Key: TKey; const Value: TValue): Boolean;
var
  Hash: Cardinal;
  BucketIdx: Integer;
  NewEntry: PEntry;
begin
  Result := False;
  FLock.Acquire;
  try
    Hash := GetHashValue(Key);
    BucketIdx := GetBucketIndex(Hash);
    
    if FindEntry(Key, Hash, BucketIdx) <> nil then
      Exit;

    NewEntry := FAllocator.Alloc;
    NewEntry^.Key := Key;
    NewEntry^.Value := Value;
    NewEntry^.Hash := Hash;
    NewEntry^.Next := FBuckets[BucketIdx];
    FBuckets[BucketIdx] := NewEntry;
    
    Inc(FCount);
    CheckLoadFactor;
    Result := True;
  finally
    FLock.Release;
  end;
end;

procedure TThreadSafeDictionary.AddRange(const ADictionary: specialize IThreadSafeDictionary<TKey, TValue>);
begin
  if ADictionary = nil then
    Exit;

  // ToArray acquires the source lock only while creating the snapshot.
  // Do not manually lock the source and then call its public methods: FPC's
  // TCriticalSection is non-reentrant on POSIX and that pattern deadlocks.
  AddRange(ADictionary.ToArray);
end;

procedure TThreadSafeDictionary.AddRange(const AArray: specialize TPairArray<TKey, TValue>);
var
  I, NewCount, RequiredBuckets: Integer;
  Hash: Cardinal;
  BucketIdx: Integer;
  Entry: PEntry;
begin
  if Length(AArray) = 0 then
    Exit;

  FLock.Acquire;
  try
    NewCount := FCount + Length(AArray);

    // v0.8: Pre-calculate required bucket count to avoid multiple resizes
    RequiredBuckets := Trunc(NewCount / LOAD_FACTOR) + 1;
    if RequiredBuckets > Length(FBuckets) then
    begin
      RequiredBuckets := GetNextPowerOfTwo(RequiredBuckets);
      Resize(RequiredBuckets);
    end;

    // Add all items — inline AddOrSetValue logic to avoid re-acquiring the lock
    for I := Low(AArray) to High(AArray) do
    begin
      Hash := GetHashValue(AArray[I].Key);
      BucketIdx := GetBucketIndex(Hash);
      Entry := FindEntry(AArray[I].Key, Hash, BucketIdx);
      if Entry <> nil then
        Entry^.Value := AArray[I].Value
      else
        InternalAdd(AArray[I].Key, AArray[I].Value);
    end;
  finally
    FLock.Release;
  end;
end;

function TThreadSafeDictionary.ToArray: specialize TPairArray<TKey, TValue>;
var
  I: Integer;
  Entry: PEntry;
  Index: Integer;
begin
  FLock.Acquire;
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
    FLock.Release;
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
      if FValueComparer.Equals(Entry^.Value, Value) then
        Exit(True);
      Entry := Entry^.Next;
    end;
  end;
end;

function TThreadSafeDictionary.ContainsValue(const Value: TValue): Boolean;
begin
  FLock.Acquire;
  try
    Result := FindValue(Value);
  finally
    FLock.Release;
  end;
end;

end.
