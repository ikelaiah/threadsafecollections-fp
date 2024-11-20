unit ThreadSafeCollections.HashSet;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, SyncObjs, HashFunctions, TypInfo;

type
  // Generic equality comparer type
  generic TEqualityComparer<T> = function(const A, B: T): Boolean;
  
  // Generic hash function type
  generic THashFunction<T> = function(const Value: T): Cardinal;

  // Generic ThreadSafe HashSet
  generic TThreadSafeHashSet<T> = class
  private
  type
    PEntry = ^TEntry;
    TEntry = record
      Value: T;
      Hash: Cardinal;
      Next: PEntry;
    end;

  const
    INITIAL_BUCKET_COUNT = 16;
    LOAD_FACTOR = 0.75;
    MIN_BUCKET_COUNT = 4;

  private
    FBuckets: array of PEntry;
    FCount: Integer;
    FLock: TCriticalSection;
    FEqualityComparer: specialize TEqualityComparer<T>;
    FHashFunction: specialize THashFunction<T>;

    function GetBucketIndex(Hash: Cardinal): Integer; inline;
    procedure Resize(NewSize: Integer);
    procedure CheckLoadFactor;
    function FindEntry(const Item: T; Hash: Cardinal; BucketIdx: Integer): PEntry;
    function GetNextPowerOfTwo(Value: Integer): Integer;

  public
   // Single, safe constructor with optional initial capacity
    constructor Create(AEqualityComparer: specialize TEqualityComparer<T>;
                      AHashFunction: specialize THashFunction<T>;
                      AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); 

    destructor Destroy; override;

    function Add(const Item: T): Boolean;  // Returns true if item was added (not already present)
    function Remove(const Item: T): Boolean;
    function Contains(const Item: T): Boolean;
    procedure Clear;
    
    property Count: Integer read FCount;
  end;

  // Specialized types with their own constructors
  TThreadSafeHashSetInteger = class(specialize TThreadSafeHashSet<Integer>)
  public
    constructor Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); overload;
  end;

  TThreadSafeHashSetString = class(specialize TThreadSafeHashSet<string>)
  public
    constructor Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); overload;
  end;

  TThreadSafeHashSetBoolean = class(specialize TThreadSafeHashSet<Boolean>)
  public
    constructor Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); overload;
  end;

  TThreadSafeHashSetReal = class(specialize TThreadSafeHashSet<Real>)
  public
    constructor Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); overload;
  end;

// Basic equality comparers
function IntegerEquals(const A, B: Integer): Boolean;
function StringEquals(const A, B: string): Boolean;
function BooleanEquals(const A, B: Boolean): Boolean;
function RealEquals(const A, B: Real): Boolean;

// Basic hash functions
function IntegerHash(const Value: Integer): Cardinal;
function StringHash(const Value: string): Cardinal;
function BooleanHash(const Value: Boolean): Cardinal;
function RealHash(const Value: Real): Cardinal;


implementation

// Basic equality comparers implementation
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

function IntegerHash(const Value: Integer): Cardinal;
begin
  Result := MultiplicativeHash(Cardinal(Value));
end;

function StringHash(const Value: string): Cardinal;
begin
  // Use XXHash32 for better performance on longer strings
  Result := XXHash32(Value);
end;

function BooleanHash(const Value: Boolean): Cardinal;
begin
  // Use MultiplicativeHash since it's just a small integer
  Result := MultiplicativeHash(Cardinal(Value));
end;

function RealHash(const Value: Real): Cardinal;
var
  IntValue: Int64;
begin
  // Convert to fixed-point and use DefaultHash
  IntValue := Int64(Trunc(Value * 1000000));
  Result := DefaultHash(IntValue);
end;

{ TThreadSafeHashSet }

constructor TThreadSafeHashSet.Create(AEqualityComparer: specialize TEqualityComparer<T>;
                                      AHashFunction: specialize THashFunction<T>;
                                      AInitialCapacity: Integer = INITIAL_BUCKET_COUNT);
begin
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
