unit ThreadSafeCollections.List;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  // Generic comparer type
  generic TComparer<T> = function(const A, B: T): Integer;

  // Generic ThreadSafe List
  generic TThreadSafeList<T> = class
  private
    FList: array of T;
    FCount: Integer;
    FCapacity: Integer;
    FLock: TCriticalSection;
    FComparer: specialize TComparer<T>;
    FSorted: Boolean;
    
    procedure Grow;
    procedure QuickSort(Left, Right: Integer);
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
  public
    constructor Create(AComparer: specialize TComparer<T>);
    destructor Destroy; override;
    
    function Add(const Item: T): Integer;
    procedure Delete(Index: Integer);
    function Find(const Item: T): Integer;
    function First: T;
    function Last: T;
    procedure Sort;
    function IsSorted: Boolean;
    procedure Replace(Index: Integer; const Item: T);
    
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property Count: Integer read FCount;
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

procedure TThreadSafeList.QuickSort(Left, Right: Integer);
var
  I, J: Integer;
  P, Temp: T;
begin
  if Right <= Left then Exit;
  
  I := Left;
  J := Right;
  P := FList[(Left + Right) div 2];
  
  repeat
    while FComparer(FList[I], P) < 0 do Inc(I);
    while FComparer(FList[J], P) > 0 do Dec(J);
    
    if I <= J then
    begin
      Temp := FList[I];
      FList[I] := FList[J];
      FList[J] := Temp;
      Inc(I);
      Dec(J);
    end;
  until I > J;
  
  if Left < J then QuickSort(Left, J);
  if I < Right then QuickSort(I, Right);
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

procedure TThreadSafeList.Sort;
begin
  FLock.Acquire;
  try
    if FCount > 1 then
      QuickSort(0, FCount - 1);
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

end.
 
