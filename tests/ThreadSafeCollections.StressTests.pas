unit ThreadSafeCollections.StressTests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Generics.Collections,
  ThreadSafeCollections.List,
  ThreadSafeCollections.Deque,
  ThreadSafeCollections.Dictionary,
  ThreadSafeCollections.HashSet,
  ThreadSafeCollections.Interfaces;

type
  TIntegerList = specialize TThreadSafeList<Integer>;
  TIntegerDeque = specialize TThreadSafeDeque<Integer>;
  TStringIntegerDictionary = specialize TThreadSafeDictionary<string, Integer>;
  TIntegerHashSet = TThreadSafeHashSetInteger;

  TListStressThread = class(TThread)
  private
    FList: TIntegerList;
    FStartValue: Integer;
    FOperations: Integer;
    FRandomState: Integer;
    FFailureCode: Integer;
    FAdded: Integer;
    function NextRandom(Range: Integer): Integer;
  public
    constructor Create(AList: TIntegerList; AStartValue, AOperations, ASeed: Integer);
    procedure Execute; override;
    property FailureCode: Integer read FFailureCode;
    property Added: Integer read FAdded;
  end;

  TListRemoveStressThread = class(TThread)
  private
    FList: TIntegerList;
    FStartValue: Integer;
    FCount: Integer;
    FFailed: Boolean;
  public
    constructor Create(AList: TIntegerList; AStartValue, ACount: Integer);
    procedure Execute; override;
    property Failed: Boolean read FFailed;
  end;

  TDictionaryStressThread = class(TThread)
  private
    FDictionary: TStringIntegerDictionary;
    FStartValue: Integer;
    FOperations: Integer;
    FRandomState: Integer;
    FFailureCode: Integer;
    FAdded: Integer;
    function NextRandom(Range: Integer): Integer;
  public
    constructor Create(ADictionary: TStringIntegerDictionary; AStartValue, AOperations, ASeed: Integer);
    procedure Execute; override;
    property FailureCode: Integer read FFailureCode;
    property Added: Integer read FAdded;
  end;

  TDictionaryRemoveStressThread = class(TThread)
  private
    FDictionary: TStringIntegerDictionary;
    FStartValue: Integer;
    FCount: Integer;
    FFailed: Boolean;
  public
    constructor Create(ADictionary: TStringIntegerDictionary; AStartValue, ACount: Integer);
    procedure Execute; override;
    property Failed: Boolean read FFailed;
  end;

  TDictionaryAddStressThread = class(TThread)
  private
    FDictionary: TStringIntegerDictionary;
    FStartValue: Integer;
    FCount: Integer;
    FFailed: Boolean;
    FAdded: Integer;
  public
    constructor Create(ADictionary: TStringIntegerDictionary; AStartValue, ACount: Integer);
    procedure Execute; override;
    property Failed: Boolean read FFailed;
    property Added: Integer read FAdded;
  end;

  THashSetStressThread = class(TThread)
  private
    FSet: TIntegerHashSet;
    FStartValue: Integer;
    FOperations: Integer;
    FRandomState: Integer;
    FFailed: Boolean;
    FAdded: Integer;
    FRemoved: array of Boolean;
    function NextRandom(Range: Integer): Integer;
  public
    constructor Create(ASet: TIntegerHashSet; AStartValue, AOperations, ASeed: Integer);
    procedure Execute; override;
    property Failed: Boolean read FFailed;
    property Added: Integer read FAdded;
  end;

  THashSetRemoveStressThread = class(TThread)
  private
    FSet: TIntegerHashSet;
    FStartValue: Integer;
    FCount: Integer;
    FFailed: Boolean;
  public
    constructor Create(ASet: TIntegerHashSet; AStartValue, ACount: Integer);
    procedure Execute; override;
    property Failed: Boolean read FFailed;
  end;

  TDequeStressProducer = class(TThread)
  private
    FDeque: TIntegerDeque;
    FStartValue: Integer;
    FCount: Integer;
    FRandomState: Integer;
    function NextRandom(Range: Integer): Integer;
  public
    constructor Create(ADeque: TIntegerDeque; AStartValue, ACount, ASeed: Integer);
    procedure Execute; override;
  end;

  TDequeStressConsumer = class(TThread)
  private
    FDeque: TIntegerDeque;
    FTarget: Integer;
    FRandomState: Integer;
    FPopped: specialize TArray<Integer>;
    FPopCount: Integer;
    function NextRandom(Range: Integer): Integer;
  public
    constructor Create(ADeque: TIntegerDeque; ATarget, ASeed: Integer);
    procedure Execute; override;
    property Popped: specialize TArray<Integer> read FPopped;
    property PopCount: Integer read FPopCount;
  end;

  TMixedCollectionsStressThread = class(TThread)
  private
    FList: TIntegerList;
    FDeque: TIntegerDeque;
    FDictionary: TStringIntegerDictionary;
    FSet: TIntegerHashSet;
    FStartValue: Integer;
    FOperations: Integer;
    FRandomState: Integer;
    FFailed: Boolean;
    FListAdded: Integer;
    FDictAdded: Integer;
    FSetAdded: Integer;
    function NextRandom(Range: Integer): Integer;
  public
    constructor Create(AList: TIntegerList; ADeque: TIntegerDeque;
      ADictionary: TStringIntegerDictionary; ASet: TIntegerHashSet;
      AStartValue, AOperations, ASeed: Integer);
    procedure Execute; override;
    property Failed: Boolean read FFailed;
    property ListAdded: Integer read FListAdded;
    property DictAdded: Integer read FDictAdded;
    property SetAdded: Integer read FSetAdded;
  end;

  TPoorHashDictionary = class(TStringIntegerDictionary)
  public
    constructor Create(AInitialCapacity: Integer = 16); reintroduce;
  end;

  TThreadSafeRandomizedStressTests = class(TTestCase)
  private
    function GetStressSeed(ADefaultSeed: Integer): Integer;
  published
    procedure TestListRandomizedStress;
    procedure TestDictionaryRandomizedStress;
    procedure TestHashSetRandomizedStress;
    procedure TestDequeRandomizedStress;
    procedure TestPoorHashDictionaryRandomizedStress;
    procedure TestMixedCollectionsRandomizedStress;
  end;

procedure SortIntArray(var A: array of Integer);
procedure SortStringArray(var A: array of string);

implementation

const
  BASE_SEED = 20260816;
  KEY_SPACE = 24000;

procedure SortIntArray(var A: array of Integer);
  procedure QuickSort(L, R: Integer);
  var
    I, J, Pivot, Temp: Integer;
  begin
    if L >= R then
      Exit;
    Pivot := A[(L + R) div 2];
    I := L;
    J := R;
    repeat
      while A[I] < Pivot do
        Inc(I);
      while A[J] > Pivot do
        Dec(J);
      if I <= J then
      begin
        Temp := A[I];
        A[I] := A[J];
        A[J] := Temp;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    QuickSort(L, J);
    QuickSort(I, R);
  end;
begin
  if Length(A) > 1 then
    QuickSort(0, High(A));
end;

procedure SortStringArray(var A: array of string);
  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    Pivot, Temp: string;
  begin
    if L >= R then
      Exit;
    Pivot := A[(L + R) div 2];
    I := L;
    J := R;
    repeat
      while A[I] < Pivot do
        Inc(I);
      while A[J] > Pivot do
        Dec(J);
      if I <= J then
      begin
        Temp := A[I];
        A[I] := A[J];
        A[J] := Temp;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    QuickSort(L, J);
    QuickSort(I, R);
  end;
begin
  if Length(A) > 1 then
    QuickSort(0, High(A));
end;

function IntegerCompare(const A, B: Integer): Integer;
begin
  if A < B then
    Result := -1
  else if A > B then
    Result := 1
  else
    Result := 0;
end;

function SameKey(const A, B: string): Boolean;
begin
  Result := A = B;
end;

function ForceCollisionHash(const Value: string): Cardinal;
begin
  Result := $BADF00D;
end;

{ TListStressThread }

constructor TListStressThread.Create(AList: TIntegerList; AStartValue, AOperations, ASeed: Integer);
begin
  inherited Create(True);
  FList := AList;
  FStartValue := AStartValue;
  FOperations := AOperations;
  FRandomState := ASeed;
  FFailureCode := 0;
  FAdded := 0;
  FreeOnTerminate := False;
end;

function TListStressThread.NextRandom(Range: Integer): Integer;
begin
  FRandomState := (FRandomState * 1103515245 + 12345) and $7fffffff;
  Result := (FRandomState shr 16) mod Range;
end;

procedure TListStressThread.Execute;
var
  I, Op, Value, Enumerated, CountAtStart: Integer;
begin
  for I := 0 to FOperations - 1 do
  begin
    Op := NextRandom(100);
    if Op < 40 then
    begin
      FList.Add(FStartValue + FAdded);
      Inc(FAdded);
    end
    else if Op < 65 then
    begin
      Value := NextRandom(FStartValue + FAdded + 1);
      if (Value >= FStartValue) and (Value < FStartValue + FAdded) then
        if not FList.Contains(Value) then
          FFailureCode := 1;
    end
    else if Op < 80 then
      FList.IndexOf(NextRandom(FStartValue + FAdded + 1))
    else if Op < 90 then
    begin
      if FList.Count > 0 then
        Value := FList.Items[NextRandom(FList.Count)];
    end
    else
    begin
      CountAtStart := FList.Count;
      Enumerated := 0;
      for Value in FList do
      begin
        Inc(Enumerated);
        if Enumerated >= 64 then
          Break;
      end;
      if CountAtStart >= 64 then
      begin
        if Enumerated <> 64 then
          FFailureCode := 2;
      end
      else if Enumerated < CountAtStart then
        FFailureCode := 3;
    end;
  end;
end;

{ TListRemoveStressThread }

constructor TListRemoveStressThread.Create(AList: TIntegerList; AStartValue, ACount: Integer);
begin
  inherited Create(True);
  FList := AList;
  FStartValue := AStartValue;
  FCount := ACount;
  FFailed := False;
  FreeOnTerminate := False;
end;

procedure TListRemoveStressThread.Execute;
var
  I: Integer;
  Found: Boolean;
begin
  for I := 0 to FCount - 1 do
  begin
    repeat
      Found := False;
      try
        FList.Extract(FStartValue + I);
        Found := True;
      except
        on EArgumentOutOfRangeException do
          Found := False;
      end;
    until not Found;
  end;
end;

{ TDictionaryStressThread }

constructor TDictionaryStressThread.Create(ADictionary: TStringIntegerDictionary; AStartValue, AOperations, ASeed: Integer);
begin
  inherited Create(True);
  FDictionary := ADictionary;
  FStartValue := AStartValue;
  FOperations := AOperations;
  FRandomState := ASeed;
  FFailureCode := 0;
  FAdded := 0;
  FreeOnTerminate := False;
end;

function TDictionaryStressThread.NextRandom(Range: Integer): Integer;
begin
  FRandomState := (FRandomState * 1103515245 + 12345) and $7fffffff;
  Result := (FRandomState shr 16) mod Range;
end;

procedure TDictionaryStressThread.Execute;
var
  I, Op, KeyValue, Value, Enumerated: Integer;
  Pair: specialize TPair<string, Integer>;
  Key: string;
begin
  for I := 0 to FOperations - 1 do
  begin
    Op := NextRandom(100);
    if Op < 35 then
    begin
      Key := 'K' + IntToStr(FStartValue + FAdded);
      if not FDictionary.TryAdd(Key, FStartValue + FAdded) then
        FFailureCode := 1;
      Inc(FAdded);
    end
    else if Op < 60 then
    begin
      KeyValue := NextRandom(FStartValue + FAdded + 1);
      if (KeyValue >= FStartValue) and (KeyValue < FStartValue + FAdded) then
      begin
        Key := 'K' + IntToStr(KeyValue);
        if not FDictionary.ContainsKey(Key) then
          FFailureCode := 2;
      end;
    end
    else if Op < 80 then
    begin
      KeyValue := NextRandom(FStartValue + FAdded + 1);
      Key := 'K' + IntToStr(KeyValue);
      if FDictionary.TryGetValue(Key, Value) then
        if Value <> KeyValue then
          FFailureCode := 3;
    end
    else if Op < 90 then
      FDictionary.ContainsKey('K' + IntToStr(NextRandom(KEY_SPACE)))
    else
    begin
      Enumerated := 0;
      for Pair in FDictionary do
      begin
        Inc(Enumerated);
        if Enumerated >= 64 then
          Break;
      end;
    end;
  end;
end;

{ TDictionaryRemoveStressThread }

constructor TDictionaryRemoveStressThread.Create(ADictionary: TStringIntegerDictionary; AStartValue, ACount: Integer);
begin
  inherited Create(True);
  FDictionary := ADictionary;
  FStartValue := AStartValue;
  FCount := ACount;
  FFailed := False;
  FreeOnTerminate := False;
end;

procedure TDictionaryRemoveStressThread.Execute;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if not FDictionary.Remove('K' + IntToStr(FStartValue + I)) then
      FFailed := True;
  end;
end;

{ TDictionaryAddStressThread }

constructor TDictionaryAddStressThread.Create(ADictionary: TStringIntegerDictionary; AStartValue, ACount: Integer);
begin
  inherited Create(True);
  FDictionary := ADictionary;
  FStartValue := AStartValue;
  FCount := ACount;
  FFailed := False;
  FAdded := 0;
  FreeOnTerminate := False;
end;

procedure TDictionaryAddStressThread.Execute;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if not FDictionary.TryAdd('K' + IntToStr(FStartValue + I), FStartValue + I) then
      FFailed := True
    else
      Inc(FAdded);
  end;
end;

{ THashSetStressThread }

constructor THashSetStressThread.Create(ASet: TIntegerHashSet; AStartValue, AOperations, ASeed: Integer);
begin
  inherited Create(True);
  FSet := ASet;
  FStartValue := AStartValue;
  FOperations := AOperations;
  FRandomState := ASeed;
  FFailed := False;
  FAdded := 0;
  SetLength(FRemoved, FOperations);
  FreeOnTerminate := False;
end;

function THashSetStressThread.NextRandom(Range: Integer): Integer;
begin
  FRandomState := (FRandomState * 1103515245 + 12345) and $7fffffff;
  Result := (FRandomState shr 16) mod Range;
end;

procedure THashSetStressThread.Execute;
var
  I, Op, Value, KeyIndex, Enumerated: Integer;
begin
  for I := 0 to FOperations - 1 do
  begin
    Op := NextRandom(100);
    if Op < 40 then
    begin
      if not FSet.Add(FStartValue + FAdded) then
        FFailed := True;
      FRemoved[FAdded] := False;
      Inc(FAdded);
    end
    else if Op < 60 then
    begin
      if FAdded > 0 then
      begin
        KeyIndex := NextRandom(FAdded);
        if (not FRemoved[KeyIndex]) and (not FSet.Contains(FStartValue + KeyIndex)) then
          FFailed := True;
      end;
    end
    else if Op < 70 then
    begin
      if FAdded > 0 then
      begin
        KeyIndex := NextRandom(FAdded);
        if not FRemoved[KeyIndex] then
          FRemoved[KeyIndex] := FSet.Remove(FStartValue + KeyIndex);
      end;
    end
    else
    begin
      Enumerated := 0;
      for Value in FSet do
      begin
        Inc(Enumerated);
        if Enumerated >= 64 then
          Break;
      end;
    end;
  end;
end;

{ THashSetRemoveStressThread }

constructor THashSetRemoveStressThread.Create(ASet: TIntegerHashSet; AStartValue, ACount: Integer);
begin
  inherited Create(True);
  FSet := ASet;
  FStartValue := AStartValue;
  FCount := ACount;
  FFailed := False;
  FreeOnTerminate := False;
end;

procedure THashSetRemoveStressThread.Execute;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if FSet.Contains(FStartValue + I) then
    begin
      if not FSet.Remove(FStartValue + I) then
        FFailed := True;
    end;
  end;
end;

{ TDequeStressProducer }

constructor TDequeStressProducer.Create(ADeque: TIntegerDeque; AStartValue, ACount, ASeed: Integer);
begin
  inherited Create(True);
  FDeque := ADeque;
  FStartValue := AStartValue;
  FCount := ACount;
  FRandomState := ASeed;
  FreeOnTerminate := False;
end;

function TDequeStressProducer.NextRandom(Range: Integer): Integer;
begin
  FRandomState := (FRandomState * 1103515245 + 12345) and $7fffffff;
  Result := (FRandomState shr 16) mod Range;
end;

procedure TDequeStressProducer.Execute;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if NextRandom(100) < 50 then
      FDeque.PushBack(FStartValue + I)
    else
      FDeque.PushFront(FStartValue + I);
  end;
end;

{ TDequeStressConsumer }

constructor TDequeStressConsumer.Create(ADeque: TIntegerDeque; ATarget, ASeed: Integer);
begin
  inherited Create(True);
  FDeque := ADeque;
  FTarget := ATarget;
  FRandomState := ASeed;
  SetLength(FPopped, ATarget);
  FPopCount := 0;
  FreeOnTerminate := False;
end;

function TDequeStressConsumer.NextRandom(Range: Integer): Integer;
begin
  FRandomState := (FRandomState * 1103515245 + 12345) and $7fffffff;
  Result := (FRandomState shr 16) mod Range;
end;

procedure TDequeStressConsumer.Execute;
var
  Value: Integer;
begin
  while FPopCount < FTarget do
  begin
    if NextRandom(100) < 50 then
    begin
      if FDeque.TryPopFront(Value) then
      begin
        FPopped[FPopCount] := Value;
        Inc(FPopCount);
      end
      else
        Sleep(0);
    end
    else
    begin
      if FDeque.TryPopBack(Value) then
      begin
        FPopped[FPopCount] := Value;
        Inc(FPopCount);
      end
      else
        Sleep(0);
    end;
  end;
end;

{ TMixedCollectionsStressThread }

constructor TMixedCollectionsStressThread.Create(AList: TIntegerList; ADeque: TIntegerDeque;
  ADictionary: TStringIntegerDictionary; ASet: TIntegerHashSet;
  AStartValue, AOperations, ASeed: Integer);
begin
  inherited Create(True);
  FList := AList;
  FDeque := ADeque;
  FDictionary := ADictionary;
  FSet := ASet;
  FStartValue := AStartValue;
  FOperations := AOperations;
  FRandomState := ASeed;
  FFailed := False;
  FListAdded := 0;
  FDictAdded := 0;
  FSetAdded := 0;
  FreeOnTerminate := False;
end;

function TMixedCollectionsStressThread.NextRandom(Range: Integer): Integer;
begin
  FRandomState := (FRandomState * 1103515245 + 12345) and $7fffffff;
  Result := (FRandomState shr 16) mod Range;
end;

procedure TMixedCollectionsStressThread.Execute;
var
  I, Op, Value: Integer;
  Key: string;
begin
  for I := 0 to FOperations - 1 do
  begin
    Op := NextRandom(400);
    case Op div 100 of
      0:
      begin
        FList.Add(FStartValue + FListAdded);
        Inc(FListAdded);
      end;
      1:
      begin
        Value := FStartValue + FDictAdded;
        Key := 'K' + IntToStr(Value);
        if not FDictionary.TryAdd(Key, Value) then
          FFailed := True;
        Inc(FDictAdded);
      end;
      2:
      begin
        if not FSet.Add(FStartValue + FSetAdded) then
          FFailed := True;
        Inc(FSetAdded);
      end;
      3:
      begin
        FDeque.PushBack(FStartValue + I);
        if not FDeque.TryPopFront(Value) then
          FFailed := True;
      end;
    end;
  end;
end;

{ TPoorHashDictionary }

constructor TPoorHashDictionary.Create(AInitialCapacity: Integer);
begin
  inherited Create(AInitialCapacity, @ForceCollisionHash, @SameKey);
end;

{ TThreadSafeRandomizedStressTests }

function TThreadSafeRandomizedStressTests.GetStressSeed(ADefaultSeed: Integer): Integer;
var
  Raw: string;
  Code: Integer;
begin
  Raw := GetEnvironmentVariable('STRESS_SEED');
  if Raw = '' then
  begin
    Result := ADefaultSeed;
    Exit;
  end;
  Val(Raw, Result, Code);
  if Code <> 0 then
    Result := ADefaultSeed;
end;

procedure TThreadSafeRandomizedStressTests.TestListRandomizedStress;
const
  THREADS = 4;
  OPS_PER_THREAD = 8000;
var
  List: TIntegerList;
  Workers: array[0..THREADS - 1] of TListStressThread;
  Removers: array[0..THREADS - 1] of TListRemoveStressThread;
  Seed, I, J, K, TotalAdded: Integer;
  AddedPerThread: array[0..THREADS - 1] of Integer;
  Snapshot: specialize TArray<Integer>;
  Expected: array of Integer;
begin
  Seed := GetStressSeed(BASE_SEED + 1);
  WriteLn(Format('Stress: seed=%d threads=%d iterations=%d collection=list mix=add,contains,indexof,items,enumerate',
    [Seed, THREADS, OPS_PER_THREAD]));
  List := TIntegerList.Create(@IntegerCompare);
  try
    for I := 0 to THREADS - 1 do
    begin
      Workers[I] := TListStressThread.Create(List, I * OPS_PER_THREAD, OPS_PER_THREAD, Seed + I);
      Workers[I].Start;
    end;
    TotalAdded := 0;
    for I := 0 to THREADS - 1 do
    begin
      Workers[I].WaitFor;
      AssertEquals(Format('Worker %d failed under stress (seed %d)', [I, Seed]),
        0, Workers[I].FailureCode);
      AddedPerThread[I] := Workers[I].Added;
      Inc(TotalAdded, AddedPerThread[I]);
      Workers[I].Free;
    end;
    AssertEquals('Stress adds must all be visible', TotalAdded, List.Count);
    Snapshot := List.ToArray;
    SortIntArray(Snapshot);
    AssertEquals('Stress snapshot must contain every added value', TotalAdded, Length(Snapshot));
    SetLength(Expected, TotalAdded);
    K := 0;
    for I := 0 to THREADS - 1 do
      for J := 0 to AddedPerThread[I] - 1 do
      begin
        Expected[K] := I * OPS_PER_THREAD + J;
        Inc(K);
      end;
    SortIntArray(Expected);
    for I := 0 to TotalAdded - 1 do
      AssertEquals('Stress snapshot must contain every added value', Expected[I], Snapshot[I]);

    for I := 0 to THREADS - 1 do
    begin
      Removers[I] := TListRemoveStressThread.Create(List, I * OPS_PER_THREAD, AddedPerThread[I]);
      Removers[I].Start;
    end;
    for I := 0 to THREADS - 1 do
    begin
      Removers[I].WaitFor;
      AssertFalse(Format('Remover %d failed (seed %d)', [I, Seed]), Removers[I].Failed);
      Removers[I].Free;
    end;
    if List.Count <> 0 then
    begin
      Snapshot := List.ToArray;
      for I := 0 to 39 do
      begin
        if I >= Length(Snapshot) then
          Break;
        WriteLn('Remaining value: ', Snapshot[I]);
      end;
    end;
    AssertEquals('Stress removals must leave the list empty', 0, List.Count);
  finally
    List.Free;
  end;
end;

procedure TThreadSafeRandomizedStressTests.TestDictionaryRandomizedStress;
const
  THREADS = 4;
  OPS_PER_THREAD = 6000;
var
  Dictionary: TStringIntegerDictionary;
  Workers: array[0..THREADS - 1] of TDictionaryStressThread;
  Removers: array[0..THREADS - 1] of TDictionaryRemoveStressThread;
  Seed, I, J, K, TotalAdded: Integer;
  AddedPerThread: array[0..THREADS - 1] of Integer;
  Keys: specialize TKeyArray<string>;
  SortedKeys: array of string;
  ExpectedKeys: array of string;
begin
  Seed := GetStressSeed(BASE_SEED + 2);
  WriteLn(Format('Stress: seed=%d threads=%d iterations=%d collection=dictionary mix=tryadd,containskey,trygetvalue,enumerate',
    [Seed, THREADS, OPS_PER_THREAD]));
  Dictionary := TStringIntegerDictionary.Create;
  try
    for I := 0 to THREADS - 1 do
    begin
      Workers[I] := TDictionaryStressThread.Create(Dictionary, I * OPS_PER_THREAD, OPS_PER_THREAD, Seed + I);
      Workers[I].Start;
    end;
    TotalAdded := 0;
    for I := 0 to THREADS - 1 do
    begin
      Workers[I].WaitFor;
      AssertEquals(Format('Worker %d failed under stress (seed %d)', [I, Seed]),
        0, Workers[I].FailureCode);
      AddedPerThread[I] := Workers[I].Added;
      Inc(TotalAdded, AddedPerThread[I]);
      Workers[I].Free;
    end;
    AssertEquals('Stress adds must all be visible', TotalAdded, Dictionary.Count);
    Keys := Dictionary.GetKeys;
    AssertEquals('Key snapshot must match the count', TotalAdded, Length(Keys));
    SetLength(SortedKeys, Length(Keys));
    for I := 0 to High(Keys) do
      SortedKeys[I] := Keys[I];
    SortStringArray(SortedKeys);
    SetLength(ExpectedKeys, TotalAdded);
    K := 0;
    for I := 0 to THREADS - 1 do
      for J := 0 to AddedPerThread[I] - 1 do
      begin
        ExpectedKeys[K] := 'K' + IntToStr(I * OPS_PER_THREAD + J);
        Inc(K);
      end;
    SortStringArray(ExpectedKeys);
    for I := 0 to TotalAdded - 1 do
      AssertEquals('Key snapshot must contain every added key', ExpectedKeys[I], SortedKeys[I]);

    for I := 0 to THREADS - 1 do
    begin
      Removers[I] := TDictionaryRemoveStressThread.Create(Dictionary, I * OPS_PER_THREAD, AddedPerThread[I]);
      Removers[I].Start;
    end;
    for I := 0 to THREADS - 1 do
    begin
      Removers[I].WaitFor;
      AssertFalse(Format('Remover %d failed (seed %d)', [I, Seed]), Removers[I].Failed);
      Removers[I].Free;
    end;
    AssertEquals('Stress removals must leave the dictionary empty', 0, Dictionary.Count);
  finally
    Dictionary.Free;
  end;
end;

procedure TThreadSafeRandomizedStressTests.TestHashSetRandomizedStress;
const
  THREADS = 4;
  OPS_PER_THREAD = 8000;
var
  Set_: TIntegerHashSet;
  Workers: array[0..THREADS - 1] of THashSetStressThread;
  Removers: array[0..THREADS - 1] of THashSetRemoveStressThread;
  Seed, I, TotalAdded, Remaining: Integer;
  AddedPerThread: array[0..THREADS - 1] of Integer;
begin
  Seed := GetStressSeed(BASE_SEED + 3);
  WriteLn(Format('Stress: seed=%d threads=%d iterations=%d collection=hashset mix=add,contains,remove,enumerate',
    [Seed, THREADS, OPS_PER_THREAD]));
  Set_ := TIntegerHashSet.Create;
  try
    for I := 0 to THREADS - 1 do
    begin
      Workers[I] := THashSetStressThread.Create(Set_, I * OPS_PER_THREAD, OPS_PER_THREAD, Seed + I);
      Workers[I].Start;
    end;
    TotalAdded := 0;
    for I := 0 to THREADS - 1 do
    begin
      Workers[I].WaitFor;
      AssertFalse(Format('Worker %d failed under stress (seed %d)', [I, Seed]), Workers[I].Failed);
      AddedPerThread[I] := Workers[I].Added;
      Inc(TotalAdded, AddedPerThread[I]);
      Workers[I].Free;
    end;
    Remaining := Set_.Count;
    AssertTrue('Stress adds must leave at least half the values present',
      Remaining >= TotalAdded div 2);

    for I := 0 to THREADS - 1 do
    begin
      Removers[I] := THashSetRemoveStressThread.Create(Set_, I * OPS_PER_THREAD, AddedPerThread[I]);
      Removers[I].Start;
    end;
    for I := 0 to THREADS - 1 do
    begin
      Removers[I].WaitFor;
      AssertFalse(Format('Remover %d failed (seed %d)', [I, Seed]), Removers[I].Failed);
      Removers[I].Free;
    end;
    AssertEquals('Stress removals must leave the set empty', 0, Set_.Count);
  finally
    Set_.Free;
  end;
end;

procedure TThreadSafeRandomizedStressTests.TestDequeRandomizedStress;
const
  PRODUCERS = 4;
  CONSUMERS = 4;
  ITEMS_PER_PRODUCER = 4000;
var
  Deque: TIntegerDeque;
  ProducerThreads: array[0..PRODUCERS - 1] of TDequeStressProducer;
  ConsumerThreads: array[0..CONSUMERS - 1] of TDequeStressConsumer;
  Seed, I, J, Total: Integer;
  Popped: array of Integer;
begin
  Seed := GetStressSeed(BASE_SEED + 4);
  WriteLn(Format('Stress: seed=%d producers=%d consumers=%d iterations=%d collection=deque mix=pushfront,pushback,trypopfront,trypopback',
    [Seed, PRODUCERS, CONSUMERS, ITEMS_PER_PRODUCER]));
  Deque := TIntegerDeque.Create;
  try
    for I := 0 to PRODUCERS - 1 do
    begin
      ProducerThreads[I] := TDequeStressProducer.Create(Deque, I * ITEMS_PER_PRODUCER,
        ITEMS_PER_PRODUCER, Seed + I);
      ProducerThreads[I].Start;
    end;
    for I := 0 to CONSUMERS - 1 do
    begin
      ConsumerThreads[I] := TDequeStressConsumer.Create(Deque, ITEMS_PER_PRODUCER, Seed + 100 + I);
      ConsumerThreads[I].Start;
    end;
    for I := 0 to PRODUCERS - 1 do
    begin
      ProducerThreads[I].WaitFor;
      ProducerThreads[I].Free;
    end;
    Total := 0;
    for I := 0 to CONSUMERS - 1 do
    begin
      ConsumerThreads[I].WaitFor;
      Inc(Total, ConsumerThreads[I].PopCount);
    end;
    AssertEquals('All pushed values must be popped', PRODUCERS * ITEMS_PER_PRODUCER, Total);
    SetLength(Popped, Total);
    Total := 0;
    for I := 0 to CONSUMERS - 1 do
    begin
      for J := 0 to ConsumerThreads[I].PopCount - 1 do
      begin
        Popped[Total] := ConsumerThreads[I].Popped[J];
        Inc(Total);
      end;
      ConsumerThreads[I].Free;
    end;
    SortIntArray(Popped);
    for I := 0 to PRODUCERS * ITEMS_PER_PRODUCER - 1 do
      AssertEquals('Popped multiset must contain every pushed value once', I, Popped[I]);
    AssertEquals('Deque must be empty after stress', 0, Deque.Count);
  finally
    Deque.Free;
  end;
end;

procedure TThreadSafeRandomizedStressTests.TestPoorHashDictionaryRandomizedStress;
const
  THREADS = 4;
  ITEMS_PER_THREAD = 1500;
var
  Dictionary: TPoorHashDictionary;
  Workers: array[0..THREADS - 1] of TDictionaryAddStressThread;
  Seed, I, Total: Integer;
begin
  Seed := GetStressSeed(BASE_SEED + 5);
  WriteLn(Format('Stress: seed=%d threads=%d iterations=%d collection=dictionary(poor-hash) mix=tryadd,containskey',
    [Seed, THREADS, ITEMS_PER_THREAD]));
  Dictionary := TPoorHashDictionary.Create;
  try
    for I := 0 to THREADS - 1 do
    begin
      Workers[I] := TDictionaryAddStressThread.Create(Dictionary, I * ITEMS_PER_THREAD,
        ITEMS_PER_THREAD);
      Workers[I].Start;
    end;
    Total := 0;
    for I := 0 to THREADS - 1 do
    begin
      Workers[I].WaitFor;
      AssertFalse(Format('Poor-hash worker %d failed (seed %d)', [I, Seed]), Workers[I].Failed);
      Inc(Total, Workers[I].Added);
      Workers[I].Free;
    end;
    AssertEquals('All colliding keys must be added', THREADS * ITEMS_PER_THREAD, Total);
    AssertEquals('Colliding dictionary count must be exact', Total, Dictionary.Count);
    for I := 0 to Total - 1 do
    begin
      if (I mod 100) = 0 then
        AssertTrue('Colliding keys must stay findable', Dictionary.ContainsKey('K' + IntToStr(I)));
    end;
  finally
    Dictionary.Free;
  end;
end;

procedure TThreadSafeRandomizedStressTests.TestMixedCollectionsRandomizedStress;
const
  THREADS = 4;
  OPS_PER_THREAD = 6000;
var
  List: TIntegerList;
  Deque: TIntegerDeque;
  Dictionary: TStringIntegerDictionary;
  Set_: TIntegerHashSet;
  Workers: array[0..THREADS - 1] of TMixedCollectionsStressThread;
  Seed, I, ListTotal, DictTotal, SetTotal: Integer;
begin
  Seed := GetStressSeed(BASE_SEED + 6);
  WriteLn(Format('Stress: seed=%d threads=%d iterations=%d collection=list+deque+dictionary+hashset mix=add,tryadd,pushback,trypopfront',
    [Seed, THREADS, OPS_PER_THREAD]));
  List := TIntegerList.Create(@IntegerCompare);
  Deque := TIntegerDeque.Create;
  Dictionary := TStringIntegerDictionary.Create;
  Set_ := TIntegerHashSet.Create;
  try
    for I := 0 to THREADS - 1 do
    begin
      Workers[I] := TMixedCollectionsStressThread.Create(List, Deque, Dictionary, Set_,
        I * OPS_PER_THREAD, OPS_PER_THREAD, Seed + I);
      Workers[I].Start;
    end;
    ListTotal := 0;
    DictTotal := 0;
    SetTotal := 0;
    for I := 0 to THREADS - 1 do
    begin
      Workers[I].WaitFor;
      AssertFalse(Format('Mixed worker %d failed (seed %d)', [I, Seed]), Workers[I].Failed);
      Inc(ListTotal, Workers[I].ListAdded);
      Inc(DictTotal, Workers[I].DictAdded);
      Inc(SetTotal, Workers[I].SetAdded);
      Workers[I].Free;
    end;
    AssertEquals('Mixed stress list count', ListTotal, List.Count);
    AssertEquals('Mixed stress dictionary count', DictTotal, Dictionary.Count);
    AssertEquals('Mixed stress hash-set count', SetTotal, Set_.Count);
    AssertEquals('Mixed stress deque must be drained', 0, Deque.Count);

    List.Clear;
    Dictionary.Clear;
    Set_.Clear;
    AssertEquals('Clear must empty the list', 0, List.Count);
    AssertEquals('Clear must empty the dictionary', 0, Dictionary.Count);
    AssertEquals('Clear must empty the hash set', 0, Set_.Count);
  finally
    List.Free;
    Deque.Free;
    Dictionary.Free;
    Set_.Free;
  end;
end;

initialization
  RegisterTest(TThreadSafeRandomizedStressTests);
end.
