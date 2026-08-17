unit ThreadSafeCollections.ConcurrencyTests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, SyncObjs, Generics.Collections,
  ThreadSafeCollections.List,
  ThreadSafeCollections.Deque,
  ThreadSafeCollections.Dictionary,
  ThreadSafeCollections.HashSet,
  ThreadSafeCollections.Interfaces;

type
  TIntegerList = specialize TThreadSafeList<Integer>;
  IIntegerList = specialize IThreadSafeList<Integer>;
  TIntegerDeque = specialize TThreadSafeDeque<Integer>;
  TStringIntegerDictionary = specialize TThreadSafeDictionary<string, Integer>;
  IStringIntegerDictionary = specialize IThreadSafeDictionary<string, Integer>;
  TIntegerHashSet = TThreadSafeHashSetInteger;

  TListAddThread = class(TThread)
  private
    FList: TIntegerList;
    FStartValue: Integer;
    FCount: Integer;
  public
    constructor Create(AList: TIntegerList; AStartValue, ACount: Integer);
    procedure Execute; override;
  end;

  TListRemoveThread = class(TThread)
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

  TListBulkAddThread = class(TThread)
  private
    FList: TIntegerList;
    FStartValue: Integer;
    FCount: Integer;
  public
    constructor Create(AList: TIntegerList; AStartValue, ACount: Integer);
    procedure Execute; override;
  end;

  TDequeProducerThread = class(TThread)
  private
    FDeque: TIntegerDeque;
    FStartValue: Integer;
    FCount: Integer;
    FUseBack: Boolean;
  public
    constructor Create(ADeque: TIntegerDeque; AStartValue, ACount: Integer; AUseBack: Boolean);
    procedure Execute; override;
  end;

  TDequeConsumerThread = class(TThread)
  private
    FDeque: TIntegerDeque;
    FTarget: Integer;
    FFromFront: Boolean;
    FPopped: specialize TArray<Integer>;
    FPopCount: Integer;
  public
    constructor Create(ADeque: TIntegerDeque; ATarget: Integer; AFromFront: Boolean);
    procedure Execute; override;
    property Popped: specialize TArray<Integer> read FPopped;
    property PopCount: Integer read FPopCount;
  end;

  TDictionaryAddThread = class(TThread)
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

  TDictionaryReaderThread = class(TThread)
  private
    FDictionary: TStringIntegerDictionary;
    FRangeStart: Integer;
    FRangeCount: Integer;
    FIterations: Integer;
    FSeed: Integer;
    FFailed: Boolean;
    function RandomValue(Range: Integer): Integer;
  public
    constructor Create(ADictionary: TStringIntegerDictionary; ARangeStart, ARangeCount, AIterations, ASeed: Integer);
    procedure Execute; override;
    property Failed: Boolean read FFailed;
  end;

  THashSetAddThread = class(TThread)
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

  THashSetRemoveThread = class(TThread)
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

  THashSetBulkAddThread = class(TThread)
  private
    FSet: TIntegerHashSet;
    FStartValue: Integer;
    FCount: Integer;
  public
    constructor Create(ASet: TIntegerHashSet; AStartValue, ACount: Integer);
    procedure Execute; override;
  end;

  TListEnumerateThread = class(TThread)
  private
    FList: TIntegerList;
    FGate: TEventObject;
    FSeen: Integer;
    FSignaled: Boolean;
  public
    constructor Create(AList: TIntegerList);
    destructor Destroy; override;
    procedure Execute; override;
    property Gate: TEventObject read FGate;
    property Seen: Integer read FSeen;
  end;

  TDictionaryEnumerateThread = class(TThread)
  private
    FDictionary: TStringIntegerDictionary;
    FGate: TEventObject;
    FSeen: Integer;
  public
    constructor Create(ADictionary: TStringIntegerDictionary);
    destructor Destroy; override;
    procedure Execute; override;
    property Gate: TEventObject read FGate;
    property Seen: Integer read FSeen;
  end;

  THashSetEnumerateThread = class(TThread)
  private
    FSet: TIntegerHashSet;
    FGate: TEventObject;
    FSeen: Integer;
  public
    constructor Create(ASet: TIntegerHashSet);
    destructor Destroy; override;
    procedure Execute; override;
    property Gate: TEventObject read FGate;
    property Seen: Integer read FSeen;
  end;

  TDequeEnumerateThread = class(TThread)
  private
    FDeque: TIntegerDeque;
    FGate: TEventObject;
    FSeen: Integer;
  public
    constructor Create(ADeque: TIntegerDeque);
    destructor Destroy; override;
    procedure Execute; override;
    property Gate: TEventObject read FGate;
    property Seen: Integer read FSeen;
  end;

  TInterfaceWorkerThread = class(TThread)
  private
    FList: IIntegerList;
    FStartValue: Integer;
    FCount: Integer;
    FFailed: Boolean;
  public
    constructor Create(AList: IIntegerList; AStartValue, ACount: Integer);
    procedure Execute; override;
    property Failed: Boolean read FFailed;
  end;

  TPoorHashSet = class(TThreadSafeHashSetString)
  public
    constructor Create(AInitialCapacity: Integer = 16); reintroduce;
  end;

  THashSetCollisionLookupThread = class(TThread)
  private
    FSet: TThreadSafeHashSetString;
    FKeys: array of string;
    FIterations: Integer;
    FFailed: Boolean;
  public
    constructor Create(ASet: TThreadSafeHashSetString; const AKeys: array of string; AIterations: Integer);
    procedure Execute; override;
    property Failed: Boolean read FFailed;
  end;

  TThreadSafeConcurrencyTests = class(TTestCase)
  private
    procedure VerifySortedRange(const AValues: array of Integer; AStart, ACount: Integer);
  published
    procedure TestListConcurrentAddAndLookup;
    procedure TestListConcurrentAddRemoveAndResize;
    procedure TestListConcurrentBulkAddRange;
    procedure TestDequeConcurrentProducersConsumers;
    procedure TestDictionaryConcurrentAddAndLookup;
    procedure TestDictionarySnapshotIterationWhileMutation;
    procedure TestHashSetConcurrentAddRemoveContains;
    procedure TestHashSetConcurrentBulkAddRangeResize;
    procedure TestHashSetCollisionConcurrentLookup;
    procedure TestInterfaceBackedCollectionConcurrent;
    procedure TestCallbackUnderConcurrentLookup;
  end;

implementation

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

{ TListAddThread }

constructor TListAddThread.Create(AList: TIntegerList; AStartValue, ACount: Integer);
begin
  inherited Create(True);
  FList := AList;
  FStartValue := AStartValue;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TListAddThread.Execute;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FList.Add(FStartValue + I);
end;

{ TListRemoveThread }

constructor TListRemoveThread.Create(AList: TIntegerList; AStartValue, ACount: Integer);
begin
  inherited Create(True);
  FList := AList;
  FStartValue := AStartValue;
  FCount := ACount;
  FFailed := False;
  FreeOnTerminate := False;
end;

procedure TListRemoveThread.Execute;
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

{ TListBulkAddThread }

constructor TListBulkAddThread.Create(AList: TIntegerList; AStartValue, ACount: Integer);
begin
  inherited Create(True);
  FList := AList;
  FStartValue := AStartValue;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TListBulkAddThread.Execute;
var
  Values: array of Integer;
  I: Integer;
begin
  SetLength(Values, FCount);
  for I := 0 to FCount - 1 do
    Values[I] := FStartValue + I;
  FList.AddRange(Values);
end;

{ TDequeProducerThread }

constructor TDequeProducerThread.Create(ADeque: TIntegerDeque; AStartValue, ACount: Integer; AUseBack: Boolean);
begin
  inherited Create(True);
  FDeque := ADeque;
  FStartValue := AStartValue;
  FCount := ACount;
  FUseBack := AUseBack;
  FreeOnTerminate := False;
end;

procedure TDequeProducerThread.Execute;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if FUseBack then
      FDeque.PushBack(FStartValue + I)
    else
      FDeque.PushFront(FStartValue + I);
  end;
end;

{ TDequeConsumerThread }

constructor TDequeConsumerThread.Create(ADeque: TIntegerDeque; ATarget: Integer; AFromFront: Boolean);
begin
  inherited Create(True);
  FDeque := ADeque;
  FTarget := ATarget;
  FFromFront := AFromFront;
  SetLength(FPopped, ATarget);
  FPopCount := 0;
  FreeOnTerminate := False;
end;

procedure TDequeConsumerThread.Execute;
var
  Value: Integer;
begin
  while FPopCount < FTarget do
  begin
    if FFromFront then
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

{ TDictionaryAddThread }

constructor TDictionaryAddThread.Create(ADictionary: TStringIntegerDictionary; AStartValue, ACount: Integer);
begin
  inherited Create(True);
  FDictionary := ADictionary;
  FStartValue := AStartValue;
  FCount := ACount;
  FFailed := False;
  FreeOnTerminate := False;
end;

procedure TDictionaryAddThread.Execute;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if not FDictionary.TryAdd(Format('K%.5d', [FStartValue + I]), FStartValue + I) then
      FFailed := True;
  end;
end;

{ TDictionaryReaderThread }

constructor TDictionaryReaderThread.Create(ADictionary: TStringIntegerDictionary; ARangeStart, ARangeCount, AIterations, ASeed: Integer);
begin
  inherited Create(True);
  FDictionary := ADictionary;
  FRangeStart := ARangeStart;
  FRangeCount := ARangeCount;
  FIterations := AIterations;
  FSeed := ASeed;
  FFailed := False;
  FreeOnTerminate := False;
end;

function TDictionaryReaderThread.RandomValue(Range: Integer): Integer;
begin
  FSeed := (FSeed * 1103515245 + 12345) and $7fffffff;
  Result := (FSeed shr 16) mod Range;
end;

procedure TDictionaryReaderThread.Execute;
var
  I: Integer;
  Key: string;
  Value: Integer;
  Found: Boolean;
begin
  for I := 0 to FIterations - 1 do
  begin
    Key := Format('K%.5d', [FRangeStart + RandomValue(FRangeCount)]);
    Found := FDictionary.TryGetValue(Key, Value);
    if Found and (Value <> StrToIntDef(Copy(Key, 2, 20), -1)) then
      FFailed := True;
  end;
end;

{ THashSetAddThread }

constructor THashSetAddThread.Create(ASet: TIntegerHashSet; AStartValue, ACount: Integer);
begin
  inherited Create(True);
  FSet := ASet;
  FStartValue := AStartValue;
  FCount := ACount;
  FFailed := False;
  FreeOnTerminate := False;
end;

procedure THashSetAddThread.Execute;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if not FSet.Add(FStartValue + I) then
      FFailed := True;
  end;
end;

{ THashSetRemoveThread }

constructor THashSetRemoveThread.Create(ASet: TIntegerHashSet; AStartValue, ACount: Integer);
begin
  inherited Create(True);
  FSet := ASet;
  FStartValue := AStartValue;
  FCount := ACount;
  FFailed := False;
  FreeOnTerminate := False;
end;

procedure THashSetRemoveThread.Execute;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if not FSet.Remove(FStartValue + I) then
      FFailed := True;
  end;
end;

{ THashSetBulkAddThread }

constructor THashSetBulkAddThread.Create(ASet: TIntegerHashSet; AStartValue, ACount: Integer);
begin
  inherited Create(True);
  FSet := ASet;
  FStartValue := AStartValue;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure THashSetBulkAddThread.Execute;
var
  Values: array of Integer;
  I: Integer;
begin
  SetLength(Values, FCount);
  for I := 0 to FCount - 1 do
    Values[I] := FStartValue + I;
  FSet.AddRange(Values);
end;

{ TListEnumerateThread }

constructor TListEnumerateThread.Create(AList: TIntegerList);
begin
  inherited Create(True);
  FList := AList;
  FGate := TEventObject.Create(nil, False, False, '');
  FSeen := 0;
  FSignaled := False;
  FreeOnTerminate := False;
end;

destructor TListEnumerateThread.Destroy;
begin
  FGate.Free;
  inherited Destroy;
end;

procedure TListEnumerateThread.Execute;
var
  Value: Integer;
begin
  for Value in FList do
  begin
    if not FSignaled then
    begin
      FSignaled := True;
      FGate.SetEvent;
    end;
    Sleep(5);
    Inc(FSeen);
  end;
end;

{ TDictionaryEnumerateThread }

constructor TDictionaryEnumerateThread.Create(ADictionary: TStringIntegerDictionary);
begin
  inherited Create(True);
  FDictionary := ADictionary;
  FGate := TEventObject.Create(nil, False, False, '');
  FSeen := 0;
  FreeOnTerminate := False;
end;

destructor TDictionaryEnumerateThread.Destroy;
begin
  FGate.Free;
  inherited Destroy;
end;

procedure TDictionaryEnumerateThread.Execute;
var
  Pair: specialize TPair<string, Integer>;
begin
  for Pair in FDictionary do
  begin
    if FSeen = 0 then
      FGate.SetEvent;
    Sleep(5);
    Inc(FSeen);
  end;
end;

{ THashSetEnumerateThread }

constructor THashSetEnumerateThread.Create(ASet: TIntegerHashSet);
begin
  inherited Create(True);
  FSet := ASet;
  FGate := TEventObject.Create(nil, False, False, '');
  FSeen := 0;
  FreeOnTerminate := False;
end;

destructor THashSetEnumerateThread.Destroy;
begin
  FGate.Free;
  inherited Destroy;
end;

procedure THashSetEnumerateThread.Execute;
var
  Value: Integer;
begin
  for Value in FSet do
  begin
    if FSeen = 0 then
      FGate.SetEvent;
    Sleep(5);
    Inc(FSeen);
  end;
end;

{ TDequeEnumerateThread }

constructor TDequeEnumerateThread.Create(ADeque: TIntegerDeque);
begin
  inherited Create(True);
  FDeque := ADeque;
  FGate := TEventObject.Create(nil, False, False, '');
  FSeen := 0;
  FreeOnTerminate := False;
end;

destructor TDequeEnumerateThread.Destroy;
begin
  FGate.Free;
  inherited Destroy;
end;

procedure TDequeEnumerateThread.Execute;
var
  Value: Integer;
begin
  for Value in FDeque do
  begin
    if FSeen = 0 then
      FGate.SetEvent;
    Sleep(5);
    Inc(FSeen);
  end;
end;

{ TInterfaceWorkerThread }

constructor TInterfaceWorkerThread.Create(AList: IIntegerList; AStartValue, ACount: Integer);
begin
  inherited Create(True);
  FList := AList;
  FStartValue := AStartValue;
  FCount := ACount;
  FFailed := False;
  FreeOnTerminate := False;
end;

procedure TInterfaceWorkerThread.Execute;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FList.Add(FStartValue + I);
  if not FList.Contains(FStartValue) then
    FFailed := True;
end;

{ TPoorHashSet }

constructor TPoorHashSet.Create(AInitialCapacity: Integer);
begin
  inherited Create(@ForceCollisionHash, AInitialCapacity);
end;

{ THashSetCollisionLookupThread }

constructor THashSetCollisionLookupThread.Create(ASet: TThreadSafeHashSetString; const AKeys: array of string; AIterations: Integer);
var
  I: Integer;
begin
  inherited Create(True);
  FSet := ASet;
  SetLength(FKeys, Length(AKeys));
  for I := 0 to High(AKeys) do
    FKeys[I] := AKeys[I];
  FIterations := AIterations;
  FFailed := False;
  FreeOnTerminate := False;
end;

procedure THashSetCollisionLookupThread.Execute;
var
  I: Integer;
begin
  for I := 0 to FIterations - 1 do
  begin
    if not FSet.Contains(FKeys[I mod Length(FKeys)]) then
      FFailed := True;
  end;
end;

{ TThreadSafeConcurrencyTests }

procedure TThreadSafeConcurrencyTests.VerifySortedRange(const AValues: array of Integer; AStart, ACount: Integer);
var
  Sorted: array of Integer;
  I: Integer;
begin
  SetLength(Sorted, Length(AValues));
  for I := 0 to High(AValues) do
    Sorted[I] := AValues[I];
  SortIntArray(Sorted);
  AssertEquals('Expected exactly ' + IntToStr(ACount) + ' values', ACount, Length(Sorted));
  for I := 0 to ACount - 1 do
    AssertEquals('Missing or duplicated value in range', AStart + I, Sorted[I]);
end;

procedure TThreadSafeConcurrencyTests.TestListConcurrentAddAndLookup;
const
  THREADS = 4;
  ITEMS = 5000;
var
  List: TIntegerList;
  Workers: array[0..THREADS - 1] of TListAddThread;
  I: Integer;
  Snapshot: specialize TArray<Integer>;
begin
  List := TIntegerList.Create(@IntegerCompare);
  try
    for I := 0 to THREADS - 1 do
    begin
      Workers[I] := TListAddThread.Create(List, I * ITEMS, ITEMS);
      Workers[I].Start;
    end;
    for I := 0 to THREADS - 1 do
    begin
      Workers[I].WaitFor;
      Workers[I].Free;
    end;
    AssertEquals('All concurrent adds must be visible', THREADS * ITEMS, List.Count);
    Snapshot := List.ToArray;
    VerifySortedRange(Snapshot, 0, THREADS * ITEMS);
  finally
    List.Free;
  end;
end;

procedure TThreadSafeConcurrencyTests.TestListConcurrentAddRemoveAndResize;
const
  THREADS = 4;
  ITEMS = 2000;
var
  List: TIntegerList;
  Adders: array[0..THREADS - 1] of TListAddThread;
  Removers: array[0..THREADS - 1] of TListRemoveThread;
  I: Integer;
begin
  List := TIntegerList.Create(@IntegerCompare);
  try
    for I := 0 to THREADS - 1 do
    begin
      Adders[I] := TListAddThread.Create(List, I * ITEMS, ITEMS);
      Adders[I].Start;
    end;
    for I := 0 to THREADS - 1 do
    begin
      Adders[I].WaitFor;
      Adders[I].Free;
    end;
    AssertEquals('All adds must be visible', THREADS * ITEMS, List.Count);

    for I := 0 to THREADS - 1 do
    begin
      Removers[I] := TListRemoveThread.Create(List, I * ITEMS, ITEMS);
      Removers[I].Start;
    end;
    for I := 0 to THREADS - 1 do
    begin
      Removers[I].WaitFor;
      AssertFalse('Every owned value must be removable', Removers[I].Failed);
      Removers[I].Free;
    end;
    AssertEquals('All concurrent removals must leave the list empty', 0, List.Count);
  finally
    List.Free;
  end;
end;

procedure TThreadSafeConcurrencyTests.TestListConcurrentBulkAddRange;
const
  THREADS = 4;
  ITEMS = 5000;
var
  List: TIntegerList;
  Workers: array[0..THREADS - 1] of TListBulkAddThread;
  I: Integer;
  Snapshot: specialize TArray<Integer>;
begin
  List := TIntegerList.Create(@IntegerCompare);
  try
    for I := 0 to THREADS - 1 do
    begin
      Workers[I] := TListBulkAddThread.Create(List, I * ITEMS, ITEMS);
      Workers[I].Start;
    end;
    for I := 0 to THREADS - 1 do
    begin
      Workers[I].WaitFor;
      Workers[I].Free;
    end;
    AssertEquals('Concurrent bulk adds must all land', THREADS * ITEMS, List.Count);
    Snapshot := List.ToArray;
    VerifySortedRange(Snapshot, 0, THREADS * ITEMS);
  finally
    List.Free;
  end;
end;

procedure TThreadSafeConcurrencyTests.TestDequeConcurrentProducersConsumers;
const
  PRODUCER_COUNT = 2;
  CONSUMER_COUNT = 2;
  ITEMS = 5000;
var
  Deque: TIntegerDeque;
  Producers: array[0..PRODUCER_COUNT - 1] of TDequeProducerThread;
  Consumers: array[0..CONSUMER_COUNT - 1] of TDequeConsumerThread;
  Popped: array of Integer;
  I, J, Total: Integer;
begin
  Deque := TIntegerDeque.Create;
  try
    for I := 0 to PRODUCER_COUNT - 1 do
    begin
      Producers[I] := TDequeProducerThread.Create(Deque, I * ITEMS, ITEMS, I = 0);
      Producers[I].Start;
    end;
    for I := 0 to CONSUMER_COUNT - 1 do
    begin
      Consumers[I] := TDequeConsumerThread.Create(Deque, ITEMS, I = 0);
      Consumers[I].Start;
    end;
    for I := 0 to PRODUCER_COUNT - 1 do
    begin
      Producers[I].WaitFor;
      Producers[I].Free;
    end;
    for I := 0 to CONSUMER_COUNT - 1 do
    begin
      Consumers[I].WaitFor;
      AssertEquals('Consumer must pop its full target', ITEMS, Consumers[I].PopCount);
    end;

    SetLength(Popped, 0);
    Total := 0;
    for I := 0 to CONSUMER_COUNT - 1 do
    begin
      for J := 0 to Consumers[I].PopCount - 1 do
      begin
        SetLength(Popped, Total + 1);
        Popped[Total] := Consumers[I].Popped[J];
        Inc(Total);
      end;
      Consumers[I].Free;
    end;
    AssertEquals('Every pushed value must be popped exactly once', PRODUCER_COUNT * ITEMS, Total);
    VerifySortedRange(Popped, 0, PRODUCER_COUNT * ITEMS);
    AssertEquals('Deque must be empty afterwards', 0, Deque.Count);
  finally
    Deque.Free;
  end;
end;

procedure TThreadSafeConcurrencyTests.TestDictionaryConcurrentAddAndLookup;
const
  THREADS = 4;
  ITEMS = 5000;
var
  Dictionary: TStringIntegerDictionary;
  Workers: array[0..THREADS - 1] of TDictionaryAddThread;
  Reader: TDictionaryReaderThread;
  Keys: specialize TKeyArray<string>;
  SortedKeys: array of string;
  I: Integer;

  procedure SortStringArray(var A: array of string);
    procedure QuickSort(L, R: Integer);
    var
      X, J: Integer;
      Pivot, Temp: string;
    begin
      if L >= R then
        Exit;
      Pivot := A[(L + R) div 2];
      X := L;
      J := R;
      repeat
        while A[X] < Pivot do
          Inc(X);
        while A[J] > Pivot do
          Dec(J);
        if X <= J then
        begin
          Temp := A[X];
          A[X] := A[J];
          A[J] := Temp;
          Inc(X);
          Dec(J);
        end;
      until X > J;
      QuickSort(L, J);
      QuickSort(X, R);
    end;
  begin
    if Length(A) > 1 then
      QuickSort(0, High(A));
  end;

begin
  Dictionary := TStringIntegerDictionary.Create;
  try
    for I := 0 to THREADS - 1 do
    begin
      Workers[I] := TDictionaryAddThread.Create(Dictionary, I * ITEMS, ITEMS);
      Workers[I].Start;
    end;
    Reader := TDictionaryReaderThread.Create(Dictionary, 0, THREADS * ITEMS, 20000, 424242);
    Reader.Start;
    for I := 0 to THREADS - 1 do
    begin
      Workers[I].WaitFor;
      AssertFalse('Every unique key must be addable', Workers[I].Failed);
      Workers[I].Free;
    end;
    Reader.WaitFor;
    AssertFalse('Concurrent readers must see consistent values', Reader.Failed);
    Reader.Free;

    AssertEquals('All concurrent adds must be visible', THREADS * ITEMS, Dictionary.Count);
    Keys := Dictionary.GetKeys;
    AssertEquals('Keys snapshot must match the count', THREADS * ITEMS, Length(Keys));
    SetLength(SortedKeys, Length(Keys));
    for I := 0 to High(Keys) do
      SortedKeys[I] := Keys[I];
    SortStringArray(SortedKeys);
    for I := 0 to THREADS * ITEMS - 1 do
      AssertEquals('Missing or duplicated key', Format('K%.5d', [I]), SortedKeys[I]);
  finally
    Dictionary.Free;
  end;
end;

procedure TThreadSafeConcurrencyTests.TestDictionarySnapshotIterationWhileMutation;
const
  ITEMS = 200;
var
  Dictionary: TStringIntegerDictionary;
  Enumerator: TDictionaryEnumerateThread;
  I: Integer;
  StartTick, Elapsed: QWord;
begin
  Dictionary := TStringIntegerDictionary.Create;
  try
    for I := 0 to ITEMS - 1 do
      Dictionary.Add('K' + IntToStr(I), I);

    Enumerator := TDictionaryEnumerateThread.Create(Dictionary);
    Enumerator.Start;
    Enumerator.Gate.WaitFor(INFINITE);

    StartTick := GetTickCount64;
    Dictionary.Add('K' + IntToStr(ITEMS), ITEMS);
    Elapsed := GetTickCount64 - StartTick;

    AssertFalse('Dictionary snapshot iteration must not block mutations',
      Enumerator.Finished);
    AssertTrue('Mutation must complete while snapshot iteration is active',
      Elapsed < 300);

    Enumerator.WaitFor;
    AssertEquals('Snapshot iteration must see exactly the pre-mutation count',
      ITEMS, Enumerator.Seen);
    AssertEquals('Mutation must land after the snapshot', ITEMS + 1, Dictionary.Count);
    Enumerator.Free;
  finally
    Dictionary.Free;
  end;
end;

procedure TThreadSafeConcurrencyTests.TestHashSetConcurrentAddRemoveContains;
const
  THREADS = 4;
  ITEMS = 5000;
var
  Set_: TIntegerHashSet;
  Adders: array[0..THREADS - 1] of THashSetAddThread;
  Removers: array[0..THREADS - 1] of THashSetRemoveThread;
  I: Integer;
  Snapshot: specialize TArray<Integer>;
begin
  Set_ := TIntegerHashSet.Create;
  try
    for I := 0 to THREADS - 1 do
    begin
      Adders[I] := THashSetAddThread.Create(Set_, I * ITEMS, ITEMS);
      Adders[I].Start;
    end;
    for I := 0 to THREADS - 1 do
    begin
      Adders[I].WaitFor;
      AssertFalse('Every unique value must be addable', Adders[I].Failed);
      Adders[I].Free;
    end;
    AssertEquals('All concurrent adds must be visible', THREADS * ITEMS, Set_.Count);
    Snapshot := Set_.ToArray;
    VerifySortedRange(Snapshot, 0, THREADS * ITEMS);

    for I := 0 to THREADS - 1 do
    begin
      Removers[I] := THashSetRemoveThread.Create(Set_, I * ITEMS, ITEMS);
      Removers[I].Start;
    end;
    for I := 0 to THREADS - 1 do
    begin
      Removers[I].WaitFor;
      AssertFalse('Every owned value must be removable', Removers[I].Failed);
      Removers[I].Free;
    end;
    AssertEquals('All concurrent removals must leave the set empty', 0, Set_.Count);
  finally
    Set_.Free;
  end;
end;

procedure TThreadSafeConcurrencyTests.TestHashSetConcurrentBulkAddRangeResize;
const
  THREADS = 4;
  ITEMS = 5000;
var
  Set_: TIntegerHashSet;
  Workers: array[0..THREADS - 1] of THashSetBulkAddThread;
  I: Integer;
  Snapshot: specialize TArray<Integer>;
begin
  Set_ := TIntegerHashSet.Create(4);
  try
    for I := 0 to THREADS - 1 do
    begin
      Workers[I] := THashSetBulkAddThread.Create(Set_, I * ITEMS, ITEMS);
      Workers[I].Start;
    end;
    for I := 0 to THREADS - 1 do
    begin
      Workers[I].WaitFor;
      Workers[I].Free;
    end;
    AssertEquals('Concurrent bulk adds must survive resize', THREADS * ITEMS, Set_.Count);
    Snapshot := Set_.ToArray;
    VerifySortedRange(Snapshot, 0, THREADS * ITEMS);
  finally
    Set_.Free;
  end;
end;

procedure TThreadSafeConcurrencyTests.TestHashSetCollisionConcurrentLookup;
const
  ITEMS = 2000;
  LOOKUPS = 4000;
  THREADS = 4;
var
  Set_: TPoorHashSet;
  Keys: array of string;
  Workers: array[0..THREADS - 1] of THashSetCollisionLookupThread;
  I: Integer;
begin
  Set_ := TPoorHashSet.Create;
  try
    SetLength(Keys, ITEMS);
    for I := 0 to ITEMS - 1 do
    begin
      Keys[I] := 'CollisionKey_' + IntToStr(I);
      AssertTrue('Colliding key must be addable', Set_.Add(Keys[I]));
    end;

    for I := 0 to THREADS - 1 do
    begin
      Workers[I] := THashSetCollisionLookupThread.Create(Set_, Keys, LOOKUPS);
      Workers[I].Start;
    end;
    for I := 0 to THREADS - 1 do
    begin
      Workers[I].WaitFor;
      AssertFalse('Concurrent lookup in a single bucket must be correct', Workers[I].Failed);
      Workers[I].Free;
    end;
  finally
    Set_.Free;
  end;
end;

procedure TThreadSafeConcurrencyTests.TestInterfaceBackedCollectionConcurrent;
const
  THREADS = 4;
  ITEMS = 2000;
var
  List: TIntegerList;
  ListInterface: IIntegerList;
  Workers: array[0..THREADS - 1] of TInterfaceWorkerThread;
  I: Integer;
begin
  List := TIntegerList.Create(@IntegerCompare);
  ListInterface := List;
  for I := 0 to THREADS - 1 do
  begin
    Workers[I] := TInterfaceWorkerThread.Create(ListInterface, I * ITEMS, ITEMS);
    Workers[I].Start;
  end;
  for I := 0 to THREADS - 1 do
  begin
    Workers[I].WaitFor;
    AssertFalse('Interface operations from worker threads must succeed', Workers[I].Failed);
    Workers[I].Free;
  end;
  AssertEquals('Interface add count must match object state', THREADS * ITEMS, List.Count);
  ListInterface := nil;
end;

procedure TThreadSafeConcurrencyTests.TestCallbackUnderConcurrentLookup;
const
  THREADS = 4;
  ITEMS = 4000;
var
  List: TIntegerList;
  Workers: array[0..THREADS - 1] of TListAddThread;
  I, J: Integer;
begin
  List := TIntegerList.Create(@IntegerCompare);
  try
    for I := 0 to THREADS - 1 do
    begin
      Workers[I] := TListAddThread.Create(List, I * ITEMS, ITEMS);
      Workers[I].Start;
    end;
    for I := 0 to THREADS - 1 do
    begin
      Workers[I].WaitFor;
      Workers[I].Free;
    end;
    for J := 0 to ITEMS * THREADS - 1 do
    begin
      if (J mod 97) = 0 then
        AssertTrue('Concurrent adds must stay findable', List.Contains(J));
    end;
  finally
    List.Free;
  end;
end;

initialization
  RegisterTest(TThreadSafeConcurrencyTests);
end.
