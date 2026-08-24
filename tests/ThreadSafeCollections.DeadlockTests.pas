unit ThreadSafeCollections.DeadlockTests;

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
  TIntegerDeque = specialize TThreadSafeDeque<Integer>;
  TStringIntegerDictionary = specialize TThreadSafeDictionary<string, Integer>;
  TIntegerHashSet = TThreadSafeHashSetInteger;

  TListBulkThread = class(TThread)
  private
    FTarget: TIntegerList;
    FSource: TIntegerList;
  public
    constructor Create(ATarget, ASource: TIntegerList);
    procedure Execute; override;
  end;

  TDictionaryBulkThread = class(TThread)
  private
    FTarget: TStringIntegerDictionary;
    FSource: TStringIntegerDictionary;
  public
    constructor Create(ATarget, ASource: TStringIntegerDictionary);
    procedure Execute; override;
  end;

  THashSetBulkThread = class(TThread)
  private
    FTarget: TIntegerHashSet;
    FSource: TIntegerHashSet;
    FIntersect: Boolean;
  public
    constructor Create(ATarget, ASource: TIntegerHashSet; AIntersect: Boolean);
    procedure Execute; override;
  end;

  TListAddWorker = class(TThread)
  private
    FList: TIntegerList;
    FStartValue: Integer;
    FCount: Integer;
  public
    constructor Create(AList: TIntegerList; AStartValue, ACount: Integer);
    procedure Execute; override;
  end;

  TDictionaryAddWorker = class(TThread)
  private
    FDictionary: TStringIntegerDictionary;
    FStartValue: Integer;
    FCount: Integer;
  public
    constructor Create(ADictionary: TStringIntegerDictionary; AStartValue, ACount: Integer);
    procedure Execute; override;
  end;

  TListSlowEnumerator = class(TThread)
  private
    FList: TIntegerList;
    FGate: TEventObject;
    FSeen: Integer;
  public
    constructor Create(AList: TIntegerList);
    destructor Destroy; override;
    procedure Execute; override;
    property Gate: TEventObject read FGate;
    property Seen: Integer read FSeen;
  end;

  THashSetSlowEnumerator = class(TThread)
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

  TDequeSlowEnumerator = class(TThread)
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

  TLockHolderThread = class(TThread)
  private
    FList: TIntegerList;
    FInside: Boolean;
    FReleased: Boolean;
  public
    constructor Create(AList: TIntegerList);
    procedure Execute; override;
    property Inside: Boolean read FInside;
    property Released: Boolean read FReleased;
  end;

  TLockContenderThread = class(TThread)
  private
    FList: TIntegerList;
    FHolder: TLockHolderThread;
    FSawInside: Boolean;
  public
    constructor Create(AList: TIntegerList; AHolder: TLockHolderThread);
    procedure Execute; override;
    property SawInside: Boolean read FSawInside;
  end;

  TThreadSafeDeadlockTests = class(TTestCase)
  published
    procedure TestOppositeLockOrderDictionaryBulk;
    procedure TestOppositeLockOrderHashSetBulk;
    procedure TestOppositeLockOrderListBulk;
    procedure TestCrossCollectionSelfSourceBounded;
    procedure TestLockHoldingEnumerationBlocksMutationList;
    procedure TestLockHoldingEnumerationBlocksMutationHashSet;
    procedure TestLockHoldingEnumerationBlocksMutationDeque;
    procedure TestManualLockTokenSerialization;
    procedure TestManualLockTokenSameThreadReentrancy;
    procedure TestDestructionAfterConcurrentWork;
    procedure TestEnumerationCompletedBeforeDestruction;
  end;

function WaitForThreadBounded(AThread: TThread; const ATestName: string): Boolean;

implementation

const
  BOUNDED_TIMEOUT_MS = 60000;
  POLL_INTERVAL_MS = 50;

function WaitForThreadBounded(AThread: TThread; const ATestName: string): Boolean;
var
  Elapsed: Integer;
begin
  Elapsed := 0;
  while not AThread.Finished do
  begin
    Sleep(POLL_INTERVAL_MS);
    Inc(Elapsed, POLL_INTERVAL_MS);
    if Elapsed >= BOUNDED_TIMEOUT_MS then
    begin
      WriteLn(Format('DEADLOCK: %s did not complete within %d ms',
        [ATestName, BOUNDED_TIMEOUT_MS]));
      Result := False;
      Exit;
    end;
  end;
  Result := True;
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

{ TListBulkThread }

constructor TListBulkThread.Create(ATarget, ASource: TIntegerList);
begin
  inherited Create(True);
  FTarget := ATarget;
  FSource := ASource;
  FreeOnTerminate := False;
end;

procedure TListBulkThread.Execute;
begin
  FTarget.AddRange(FSource);
end;

{ TDictionaryBulkThread }

constructor TDictionaryBulkThread.Create(ATarget, ASource: TStringIntegerDictionary);
begin
  inherited Create(True);
  FTarget := ATarget;
  FSource := ASource;
  FreeOnTerminate := False;
end;

procedure TDictionaryBulkThread.Execute;
begin
  FTarget.AddRange(FSource);
end;

{ THashSetBulkThread }

constructor THashSetBulkThread.Create(ATarget, ASource: TIntegerHashSet; AIntersect: Boolean);
begin
  inherited Create(True);
  FTarget := ATarget;
  FSource := ASource;
  FIntersect := AIntersect;
  FreeOnTerminate := False;
end;

procedure THashSetBulkThread.Execute;
begin
  if FIntersect then
    FTarget.IntersectWith(FSource)
  else
    FTarget.AddRange(FSource);
end;

{ TListAddWorker }

constructor TListAddWorker.Create(AList: TIntegerList; AStartValue, ACount: Integer);
begin
  inherited Create(True);
  FList := AList;
  FStartValue := AStartValue;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TListAddWorker.Execute;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FList.Add(FStartValue + I);
end;

{ TDictionaryAddWorker }

constructor TDictionaryAddWorker.Create(ADictionary: TStringIntegerDictionary; AStartValue, ACount: Integer);
begin
  inherited Create(True);
  FDictionary := ADictionary;
  FStartValue := AStartValue;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TDictionaryAddWorker.Execute;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FDictionary.TryAdd('K' + IntToStr(FStartValue + I), FStartValue + I);
end;

{ TListSlowEnumerator }

constructor TListSlowEnumerator.Create(AList: TIntegerList);
begin
  inherited Create(True);
  FList := AList;
  FGate := TEventObject.Create(nil, False, False, '');
  FSeen := 0;
  FreeOnTerminate := False;
end;

destructor TListSlowEnumerator.Destroy;
begin
  FGate.Free;
  inherited Destroy;
end;

procedure TListSlowEnumerator.Execute;
var
  Value: Integer;
begin
  for Value in FList do
  begin
    if FSeen = 0 then
      FGate.SetEvent;
    Sleep(5);
    Inc(FSeen);
  end;
end;

{ THashSetSlowEnumerator }

constructor THashSetSlowEnumerator.Create(ASet: TIntegerHashSet);
begin
  inherited Create(True);
  FSet := ASet;
  FGate := TEventObject.Create(nil, False, False, '');
  FSeen := 0;
  FreeOnTerminate := False;
end;

destructor THashSetSlowEnumerator.Destroy;
begin
  FGate.Free;
  inherited Destroy;
end;

procedure THashSetSlowEnumerator.Execute;
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

{ TDequeSlowEnumerator }

constructor TDequeSlowEnumerator.Create(ADeque: TIntegerDeque);
begin
  inherited Create(True);
  FDeque := ADeque;
  FGate := TEventObject.Create(nil, False, False, '');
  FSeen := 0;
  FreeOnTerminate := False;
end;

destructor TDequeSlowEnumerator.Destroy;
begin
  FGate.Free;
  inherited Destroy;
end;

procedure TDequeSlowEnumerator.Execute;
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

{ TLockHolderThread }

constructor TLockHolderThread.Create(AList: TIntegerList);
begin
  inherited Create(True);
  FList := AList;
  FInside := False;
  FReleased := False;
  FreeOnTerminate := False;
end;

procedure TLockHolderThread.Execute;
var
  Token: ILockToken;
begin
  Token := FList.Lock;
  FInside := True;
  Sleep(300);
  FInside := False;
  FReleased := True;
  Token := nil;
end;

{ TLockContenderThread }

constructor TLockContenderThread.Create(AList: TIntegerList; AHolder: TLockHolderThread);
begin
  inherited Create(True);
  FList := AList;
  FHolder := AHolder;
  FSawInside := False;
  FreeOnTerminate := False;
end;

procedure TLockContenderThread.Execute;
var
  Token: ILockToken;
begin
  Token := FList.Lock;
  FSawInside := FHolder.Inside;
  Token := nil;
end;

{ TThreadSafeDeadlockTests }

procedure TThreadSafeDeadlockTests.TestOppositeLockOrderDictionaryBulk;
var
  DictA, DictB: TStringIntegerDictionary;
  ThreadAB, ThreadBA: TDictionaryBulkThread;
  I: Integer;
begin
  DictA := TStringIntegerDictionary.Create;
  DictB := TStringIntegerDictionary.Create;
  try
    for I := 0 to 1999 do
    begin
      DictA.TryAdd('A' + IntToStr(I), I);
      DictB.TryAdd('B' + IntToStr(I), I);
    end;

    ThreadAB := TDictionaryBulkThread.Create(DictA, DictB);
    ThreadBA := TDictionaryBulkThread.Create(DictB, DictA);
    ThreadAB.Start;
    ThreadBA.Start;

    if not WaitForThreadBounded(ThreadAB, 'TestOppositeLockOrderDictionaryBulk A<-B') then
      Halt(2);
    if not WaitForThreadBounded(ThreadBA, 'TestOppositeLockOrderDictionaryBulk B<-A') then
      Halt(2);
    ThreadAB.Free;
    ThreadBA.Free;

    AssertEquals('Bulk add A<-B must include both key sets', 4000, DictA.Count);
    AssertEquals('Bulk add B<-A must include both key sets', 4000, DictB.Count);
  finally
    DictA.Free;
    DictB.Free;
  end;
end;

procedure TThreadSafeDeadlockTests.TestOppositeLockOrderHashSetBulk;
var
  SetA, SetB: TIntegerHashSet;
  ThreadAB, ThreadBA: THashSetBulkThread;
  I: Integer;
begin
  SetA := TIntegerHashSet.Create;
  SetB := TIntegerHashSet.Create;
  try
    for I := 0 to 1999 do
    begin
      SetA.Add(I);
      SetB.Add(I + 1000);
    end;

    ThreadAB := THashSetBulkThread.Create(SetA, SetB, True);
    ThreadBA := THashSetBulkThread.Create(SetB, SetA, True);
    ThreadAB.Start;
    ThreadBA.Start;

    if not WaitForThreadBounded(ThreadAB, 'TestOppositeLockOrderHashSetBulk A∩B') then
      Halt(2);
    if not WaitForThreadBounded(ThreadBA, 'TestOppositeLockOrderHashSetBulk B∩A') then
      Halt(2);
    ThreadAB.Free;
    ThreadBA.Free;

    AssertEquals('Intersection A∩B must keep the shared range', 1000, SetA.Count);
    AssertEquals('Intersection B∩A must keep the shared range', 1000, SetB.Count);
  finally
    SetA.Free;
    SetB.Free;
  end;
end;

procedure TThreadSafeDeadlockTests.TestOppositeLockOrderListBulk;
var
  ListA, ListB: TIntegerList;
  ThreadAB, ThreadBA: TListBulkThread;
  I: Integer;
begin
  ListA := TIntegerList.Create(@IntegerCompare);
  ListB := TIntegerList.Create(@IntegerCompare);
  try
    for I := 0 to 1999 do
    begin
      ListA.Add(I);
      ListB.Add(I + 1000);
    end;

    ThreadAB := TListBulkThread.Create(ListA, ListB);
    ThreadBA := TListBulkThread.Create(ListB, ListA);
    ThreadAB.Start;
    ThreadBA.Start;

    if not WaitForThreadBounded(ThreadAB, 'TestOppositeLockOrderListBulk A<-B') then
      Halt(2);
    if not WaitForThreadBounded(ThreadBA, 'TestOppositeLockOrderListBulk B<-A') then
      Halt(2);
    ThreadAB.Free;
    ThreadBA.Free;

    AssertTrue('Bulk add A<-B must include at least the initial B content',
      ListA.Count >= 4000);
    AssertTrue('Bulk add B<-A must include at least the initial A content',
      ListB.Count >= 4000);
  finally
    ListA.Free;
    ListB.Free;
  end;
end;

procedure TThreadSafeDeadlockTests.TestCrossCollectionSelfSourceBounded;
var
  Dict: TStringIntegerDictionary;
  Set_: TIntegerHashSet;
  Thread: TThread;
  I: Integer;
begin
  Dict := TStringIntegerDictionary.Create;
  try
    for I := 0 to 999 do
      Dict.TryAdd('K' + IntToStr(I), I);
    Thread := TDictionaryBulkThread.Create(Dict, Dict);
    Thread.Start;
    if not WaitForThreadBounded(Thread, 'Dictionary self-source AddRange') then
      Halt(2);
    Thread.Free;
    AssertEquals('Self-source AddRange must not corrupt the count', 1000, Dict.Count);
  finally
    Dict.Free;
  end;

  Set_ := TIntegerHashSet.Create;
  try
    for I := 0 to 999 do
      Set_.Add(I);
    Thread := THashSetBulkThread.Create(Set_, Set_, False);
    Thread.Start;
    if not WaitForThreadBounded(Thread, 'HashSet self-source AddRange') then
      Halt(2);
    Thread.Free;
    AssertEquals('Self-source AddRange must not corrupt the count', 1000, Set_.Count);
  finally
    Set_.Free;
  end;
end;

procedure TThreadSafeDeadlockTests.TestLockHoldingEnumerationBlocksMutationList;
var
  List: TIntegerList;
  Enumerator: TListSlowEnumerator;
  I: Integer;
  StartTick, Elapsed: QWord;
begin
  List := TIntegerList.Create(@IntegerCompare);
  try
    for I := 0 to 199 do
      List.Add(I);

    Enumerator := TListSlowEnumerator.Create(List);
    Enumerator.Start;
    Enumerator.Gate.WaitFor(INFINITE);

    StartTick := GetTickCount64;
    List.Add(999999);
    Elapsed := GetTickCount64 - StartTick;

    AssertTrue('List mutation must block while a lock-holding enumerator is active',
      Elapsed >= 400);
    if not WaitForThreadBounded(Enumerator, 'List lock-holding enumeration') then
      Halt(2);
    AssertEquals('Enumerator must see the pre-mutation count', 200, Enumerator.Seen);
    Enumerator.Free;
    AssertEquals('Mutation must land after enumeration', 201, List.Count);
  finally
    List.Free;
  end;
end;

procedure TThreadSafeDeadlockTests.TestLockHoldingEnumerationBlocksMutationHashSet;
var
  Set_: TIntegerHashSet;
  Enumerator: THashSetSlowEnumerator;
  I: Integer;
  StartTick, Elapsed: QWord;
begin
  Set_ := TIntegerHashSet.Create;
  try
    for I := 0 to 199 do
      Set_.Add(I);

    Enumerator := THashSetSlowEnumerator.Create(Set_);
    Enumerator.Start;
    Enumerator.Gate.WaitFor(INFINITE);

    StartTick := GetTickCount64;
    Set_.Add(999999);
    Elapsed := GetTickCount64 - StartTick;

    AssertTrue('HashSet mutation must block while a lock-holding enumerator is active',
      Elapsed >= 400);
    if not WaitForThreadBounded(Enumerator, 'HashSet lock-holding enumeration') then
      Halt(2);
    AssertEquals('Enumerator must see the pre-mutation count', 200, Enumerator.Seen);
    Enumerator.Free;
    AssertEquals('Mutation must land after enumeration', 201, Set_.Count);
  finally
    Set_.Free;
  end;
end;

procedure TThreadSafeDeadlockTests.TestLockHoldingEnumerationBlocksMutationDeque;
var
  Deque: TIntegerDeque;
  Enumerator: TDequeSlowEnumerator;
  I: Integer;
  StartTick, Elapsed: QWord;
begin
  Deque := TIntegerDeque.Create;
  try
    for I := 0 to 199 do
      Deque.PushBack(I);

    Enumerator := TDequeSlowEnumerator.Create(Deque);
    Enumerator.Start;
    Enumerator.Gate.WaitFor(INFINITE);

    StartTick := GetTickCount64;
    Deque.PushBack(999999);
    Elapsed := GetTickCount64 - StartTick;

    AssertTrue('Deque mutation must block while a lock-holding enumerator is active',
      Elapsed >= 400);
    if not WaitForThreadBounded(Enumerator, 'Deque lock-holding enumeration') then
      Halt(2);
    AssertEquals('Enumerator must see the pre-mutation count', 200, Enumerator.Seen);
    Enumerator.Free;
    AssertEquals('Mutation must land after enumeration', 201, Deque.Count);
  finally
    Deque.Free;
  end;
end;

procedure TThreadSafeDeadlockTests.TestManualLockTokenSerialization;
var
  List: TIntegerList;
  Holder: TLockHolderThread;
  Contender: TLockContenderThread;
begin
  List := TIntegerList.Create(@IntegerCompare);
  try
    Holder := TLockHolderThread.Create(List);
    Contender := TLockContenderThread.Create(List, Holder);
    Holder.Start;
    Contender.Start;

    if not WaitForThreadBounded(Holder, 'Manual lock holder') then
      Halt(2);
    if not WaitForThreadBounded(Contender, 'Manual lock contender') then
      Halt(2);

    AssertTrue('Holder must finish inside the lock', Holder.Released);
    AssertFalse('Contender must not enter while the holder is inside', Contender.SawInside);
    Holder.Free;
    Contender.Free;
  finally
    List.Free;
  end;
end;

procedure TThreadSafeDeadlockTests.TestManualLockTokenSameThreadReentrancy;
var
  List: TIntegerList;
  Token: ILockToken;
  StartTick, Elapsed: QWord;
begin
  // v0.8.9: the collection lock is re-entrant for the owning thread, so
  // calling public methods while holding a Lock token must complete bounded
  // on every platform (previously only Windows, where TCriticalSection is
  // recursive, was safe; POSIX could deadlock).
  List := TIntegerList.Create(@IntegerCompare);
  try
    Token := List.Lock;
    StartTick := GetTickCount64;
    List.Add(42);
    Elapsed := GetTickCount64 - StartTick;
    AssertTrue('Same-thread re-entry under a lock token must complete bounded',
      Elapsed < 5000);
    Token := nil;
    AssertEquals('Re-entrant add must be visible', 1, List.Count);
  finally
    List.Free;
  end;
end;

procedure TThreadSafeDeadlockTests.TestDestructionAfterConcurrentWork;
const
  THREADS = 4;
  ITEMS = 2000;
var
  List: TIntegerList;
  Workers: array[0..THREADS - 1] of TListAddWorker;
  I: Integer;
begin
  List := TIntegerList.Create(@IntegerCompare);
  for I := 0 to THREADS - 1 do
  begin
    Workers[I] := TListAddWorker.Create(List, I * ITEMS, ITEMS);
    Workers[I].Start;
  end;
  for I := 0 to THREADS - 1 do
  begin
    if not WaitForThreadBounded(Workers[I], 'Destruction-after-work worker') then
      Halt(2);
    Workers[I].Free;
  end;
  AssertEquals('All concurrent adds must be visible before destruction', THREADS * ITEMS, List.Count);
  List.Free;

  List := TIntegerList.Create(@IntegerCompare);
  try
    List.Add(7);
    AssertEquals('A fresh collection must work after destroying a stressed one', 1, List.Count);
  finally
    List.Free;
  end;
end;

procedure TThreadSafeDeadlockTests.TestEnumerationCompletedBeforeDestruction;
var
  List: TIntegerList;
  Value, Total: Integer;
begin
  List := TIntegerList.Create(@IntegerCompare);
  try
    List.AddRange([1, 2, 3, 4, 5]);
    Total := 0;
    for Value in List do
      Inc(Total);
    AssertEquals('Enumeration must complete before destruction', 5, Total);
  finally
    List.Free;
  end;
end;

initialization
  RegisterTest(TThreadSafeDeadlockTests);
end.
