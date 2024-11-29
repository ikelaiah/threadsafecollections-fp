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
  generic TThreadSafeHashSet<T> = class
  private
  type
    // Entry structure for hash set elements
    PEntry = ^TEntry;
    TEntry = record
      Value: T;        // The actual stored value
      Hash: Cardinal;  // Cached hash code for better performance
      Next: PEntry;    // Pointer to next entry in chain (for collision handling)
    end;

    { TEnumerator - Iterator for the hash set }
    TEnumerator = class
    private
      FSet: TThreadSafeHashSet;
      FCurrentBucket: Integer;
      FCurrentEntry: PEntry;
      FLockToken: ILockToken;
    public
      constructor Create(ASet: TThreadSafeHashSet);
      destructor Destroy; override;
      function GetCurrent: T;
      function MoveNext: Boolean;
      property Current: T read GetCurrent;
    end;

  const
    INITIAL_BUCKET_COUNT = 16;   // Default number of buckets
    LOAD_FACTOR = 0.75;          // Threshold for resizing (75% full)
    MIN_BUCKET_COUNT = 4;        // Minimum number of buckets

  private
    FBuckets: array of PEntry;   // Array of bucket heads
    FCount: Integer;             // Number of items in the set
    FLock: TCriticalSection;     // Thread synchronization
    FEqualityComparer: specialize TEqualityComparer<T>;  // Equality comparison function
    FHashFunction: specialize THashFunction<T>;          // Hash function

    // Maps a hash code to a bucket index using bitwise AND
    function GetBucketIndex(Hash: Cardinal): Integer; inline;
    
    // Grows the hash table when load factor is exceeded
    procedure Resize(NewSize: Integer);
    
    // Checks if resizing is needed after an addition
    procedure CheckLoadFactor;
    
    // Finds an entry in a specific bucket chain
    function FindEntry(const Item: T; Hash: Cardinal; BucketIdx: Integer): PEntry;
    
    // Calculates next power of 2 for bucket count
    function GetNextPowerOfTwo(Value: Integer): Integer;

  public
    // Creates a new hash set with specified equality and hash functions
    constructor Create(AEqualityComparer: specialize TEqualityComparer<T>;
                      AHashFunction: specialize THashFunction<T>;
                      AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); 

    destructor Destroy; override;

    // Core operations
    function Add(const Item: T): Boolean;      // Returns true if item was newly added
    function Remove(const Item: T): Boolean;   // Returns true if item was found and removed
    function Contains(const Item: T): Boolean; // Returns true if item exists
    procedure Clear;                           // Removes all items
    
    property Count: Integer read FCount;       // Number of items in set
    
    { Iterator support }
    function GetEnumerator: TEnumerator;

    function Lock: ILockToken;
  end;

  { Specialized hash set types with predefined hash and equality functions }
  
  // Integer hash set
  TThreadSafeHashSetInteger = class(specialize TThreadSafeHashSet<Integer>)
  public
    constructor Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); overload;
  end;

  // String hash set with optional custom hash function
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

  // Boolean hash set
  TThreadSafeHashSetBoolean = class(specialize TThreadSafeHashSet<Boolean>)
  public
    constructor Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); overload;
  end;

  // Real number hash set
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

{ Basic equality comparers - straightforward comparisons }
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

{ Hash functions for different types }
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

{ TThreadSafeHashSet implementation }

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

end.
