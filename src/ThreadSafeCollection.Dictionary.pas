unit ThreadSafeCollection.Dictionary;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes, SyncObjs, HashFunctions, TypInfo;

const
  DEBUG_LOGGING = False;

type
  generic TDictionaryEntry<TKey, TValue> = record
    Key: TKey;
    Value: TValue;
    Hash: cardinal;
    Next: ^TDictionaryEntry;
  end;

  generic TThreadSafeDictionary<TKey, TValue> = class
  private
  type
    TEntry = specialize TDictionaryEntry<TKey, TValue>;
    PEntry = ^TEntry;
  private
  const
    INITIAL_BUCKET_COUNT = 16;
    LOAD_FACTOR = 0.75;

  private
    FLock: TCriticalSection;
    FBuckets: array of PEntry;
    FCount: integer;

    function GetHashValue(const Key: TKey): cardinal;
    function GetBucketIndex(Hash: cardinal): integer; inline;
    procedure Resize(NewSize: integer);
    procedure CheckLoadFactor;
    function FindEntry(const Key: TKey; Hash: cardinal; BucketIdx: integer): PEntry;
  public
    constructor Create;
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

    property Items[const Key: TKey]: TValue read Find write Replace; default;
  end;

implementation

{ Hash calculation functions }
function HashString(const Key: string): cardinal;
var
  I: integer;
begin
  Result := 0;
  for I := 1 to Length(Key) do
    Result := ((Result shl 5) or (Result shr 27)) xor Ord(Key[I]);
end;

function HashInteger(const Key: integer): cardinal;
begin
  Result := cardinal(Key);
end;

{ TThreadSafeDictionary implementation }

constructor TThreadSafeDictionary.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  SetLength(FBuckets, INITIAL_BUCKET_COUNT);
  FCount := 0;
end;

destructor TThreadSafeDictionary.Destroy;
begin
  Clear;
  FLock.Free;
  inherited Destroy;
end;

function TThreadSafeDictionary.GetHashValue(const Key: TKey): cardinal;
var
  S: string;
  I: integer;
  RawHash: cardinal;
begin
  if TypeInfo(TKey) = TypeInfo(string) then
  begin
    S := string((@Key)^);
    RawHash := XXHash32(S);
    if DEBUG_LOGGING then WriteLn(Format('GetHashValue: Raw hash for string "%s": %d',
        [S, integer(RawHash)]));
    Result := RawHash and $7FFFFFFF;
    if DEBUG_LOGGING then WriteLn(Format('GetHashValue: Masked hash: %d',
        [integer(Result)]));
  end
  else if TypeInfo(TKey) = TypeInfo(integer) then
  begin
    I := integer((@Key)^);
    RawHash := MultiplicativeHash(cardinal(I));
    if DEBUG_LOGGING then WriteLn(Format('GetHashValue: Raw hash for int %d: %d',
        [I, integer(RawHash)]));
    Result := RawHash and $7FFFFFFF;
    if DEBUG_LOGGING then WriteLn(Format('GetHashValue: Masked hash: %d',
        [integer(Result)]));
  end
  else
  begin
    RawHash := DefaultHash(Key);
    if DEBUG_LOGGING then WriteLn(Format('GetHashValue: Raw hash for other type: %d',
        [integer(RawHash)]));
    Result := RawHash and $7FFFFFFF;
    if DEBUG_LOGGING then WriteLn(Format('GetHashValue: Masked hash: %d',
        [integer(Result)]));
  end;
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
    if DEBUG_LOGGING then WriteLn('DEBUG: About to call DoResize');
    Resize(Length(FBuckets) * 2);
    if DEBUG_LOGGING then WriteLn('DEBUG: After DoResize call');
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

      // Use stored hash to calculate new bucket index
      NewBucketIdx := Entry^.Hash and (NewSize - 1);

      // Insert at beginning of new bucket
      Entry^.Next := FBuckets[NewBucketIdx];
      FBuckets[NewBucketIdx] := Entry;

      Entry := Next;  // Move to next entry
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
    if (Entry^.Hash = Hash) and (Entry^.Key = Key) then
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
      if (Entry^.Hash = Hash) and (Entry^.Key = Key) then
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

end.
