program Benchmark;

{$mode objfpc}{$H+}{$J-}

{
  Benchmark for ThreadSafeCollections-FP
  =======================================
  Measures throughput (ops/sec) for the four collections across the core
  operations.  Each scenario is run RUNS times; the best and worst runs are
  dropped and the remaining times are averaged (trimmed mean).

  Collections benchmarked
    - TThreadSafeList<Integer>
    - TThreadSafeDictionary<string, Integer>
    - TThreadSafeHashSet<Integer>
    - TThreadSafeDeque<Integer>

  Scenarios per collection
    Single-threaded
      Add / PushBack        ITEM_COUNT items
      Contains / TryGetValue / Peek
      Remove / PopFront
      Iterate (for-in)
      Sort (List only)

    Multi-threaded  (THREAD_COUNT concurrent threads, ITEM_COUNT total)
      Concurrent Add
      Concurrent mixed read+write

  Output
    One line per scenario:
      [Collection] [Scenario]  N items  avg X ms  best Y ms  ops/s Z
}

uses
  SysUtils, Classes, DateUtils, Math, SyncObjs,
  ThreadSafeCollections.List,
  ThreadSafeCollections.Dictionary,
  ThreadSafeCollections.HashSet,
  ThreadSafeCollections.Deque,
  HashFunctions;

const
  ITEM_COUNT   = 100000;   // items per single-threaded scenario
  THREAD_COUNT = 4;        // threads for multi-threaded scenarios
  RUNS         = 5;        // total runs per scenario (best+worst trimmed)
  TRIM         = 1;        // runs to drop from each end before averaging

{ ============================================================
  Helpers
  ============================================================ }

type
  TTimings = array of Int64;   // milliseconds per run

{ Run AProc RUNS times, return trimmed-mean ms and best ms. }
procedure RunScenario(AProc: TProcedure; out AvgMs, BestMs: Int64);
var
  Timings: TTimings;
  Sorted: TTimings;
  I, J, K: Integer;
  T0: TDateTime;
  Sum, Tmp: Int64;
begin
  SetLength(Timings, RUNS);
  for I := 0 to RUNS - 1 do
  begin
    T0 := Now;
    AProc;
    Timings[I] := MilliSecondsBetween(Now, T0);
  end;

  // simple insertion sort for RUNS elements
  Sorted := Copy(Timings, 0, RUNS);
  for J := 1 to High(Sorted) do
  begin
    Tmp := Sorted[J];
    K := J - 1;
    while (K >= 0) and (Sorted[K] > Tmp) do
    begin
      Sorted[K + 1] := Sorted[K];
      Dec(K);
    end;
    Sorted[K + 1] := Tmp;
  end;

  BestMs := Sorted[0];
  Sum := 0;
  for I := TRIM to RUNS - 1 - TRIM do
    Sum := Sum + Sorted[I];
  AvgMs := Sum div (RUNS - 2 * TRIM);
end;

procedure PrintResult(const Collection, Scenario: string;
                      N: Integer; AvgMs, BestMs: Int64);
var
  OpsPerSec: Int64;
begin
  if AvgMs > 0 then
    OpsPerSec := Round(N / (AvgMs / 1000.0))
  else
    OpsPerSec := 0;
  WriteLn(Format('  %-14s  %-32s  %6d items  avg %5d ms  best %5d ms  %12d ops/s',
    [Collection, Scenario, N, AvgMs, BestMs, OpsPerSec]));
end;

procedure Separator(const Title: string);
begin
  WriteLn;
  WriteLn('--- ', Title, ' ', StringOfChar('-', 60 - Length(Title)));
end;

{ ============================================================
  Integer comparison helper (required by TThreadSafeList)
  ============================================================ }

function CmpInt(const A, B: Integer): Integer;
begin
  if A < B then Result := -1
  else if A > B then Result := 1
  else Result := 0;
end;

{ ============================================================
  Pre-generated data (built once, reused across runs)
  ============================================================ }

type
  TIntList    = specialize TThreadSafeList<Integer>;
  TStrIntDict = specialize TThreadSafeDictionary<string, Integer>;
  TIntSet     = TThreadSafeHashSetInteger;
  TIntDeque   = specialize TThreadSafeDeque<Integer>;

var
  GIntegers:  array[0..ITEM_COUNT - 1] of Integer;
  GStrings:   array[0..ITEM_COUNT - 1] of string;

procedure BuildData;
var
  I, J, Tmp: Integer;
begin
  RandSeed := 42;
  for I := 0 to ITEM_COUNT - 1 do
  begin
    GIntegers[I] := I;            // unique sequential integers
    GStrings[I]  := 'key_' + IntToStr(I);
  end;
  // shuffle integers so Sort has real work to do
  for I := ITEM_COUNT - 1 downto 1 do
  begin
    J := Random(I + 1);
    Tmp := GIntegers[I];
    GIntegers[I] := GIntegers[J];
    GIntegers[J] := Tmp;
  end;
end;

{ ============================================================
  TThreadSafeList<Integer> benchmarks
  ============================================================ }

var
  GList: TIntList;

procedure ListAdd;
var I: Integer;
begin
  GList := TIntList.Create(@CmpInt);
  try
    for I := 0 to ITEM_COUNT - 1 do
      GList.Add(GIntegers[I]);
  finally
    GList.Free;
    GList := nil;
  end;
end;

// List.Contains is a linear scan (O(n)), so we test with a smaller count
// to avoid an O(n^2) benchmark that takes minutes.
const CONTAINS_COUNT = 1000;

procedure ListContains;
var I: Integer; B: Boolean;
begin
  GList := TIntList.Create(@CmpInt);
  try
    for I := 0 to ITEM_COUNT - 1 do
      GList.Add(GIntegers[I]);
    for I := 0 to CONTAINS_COUNT - 1 do
      B := GList.Contains(GIntegers[I]);
  finally
    GList.Free;
    GList := nil;
  end;
  B := B;
end;

procedure ListSort;
var I: Integer;
begin
  GList := TIntList.Create(@CmpInt);
  try
    for I := 0 to ITEM_COUNT - 1 do
      GList.Add(GIntegers[I]);
    GList.Sort;
  finally
    GList.Free;
    GList := nil;
  end;
end;

procedure ListIterate;
var
  Iter: TIntList.TEnumerator;
  I: Integer;
begin
  GList := TIntList.Create(@CmpInt);
  try
    for I := 0 to ITEM_COUNT - 1 do
      GList.Add(GIntegers[I]);
    Iter := GList.GetEnumerator;
    try
      while Iter.MoveNext do ;
    finally
      Iter.Free;
    end;
  finally
    GList.Free;
    GList := nil;
  end;
end;

procedure ListDelete;
var I: Integer;
begin
  GList := TIntList.Create(@CmpInt);
  try
    for I := 0 to ITEM_COUNT - 1 do
      GList.Add(GIntegers[I]);
    for I := ITEM_COUNT - 1 downto 0 do
      GList.Delete(I);
  finally
    GList.Free;
    GList := nil;
  end;
end;

{ Multi-threaded List add }

type
  TListAddThread = class(TThread)
  private
    FStart, FCount: Integer;
  public
    constructor Create(AStart, ACount: Integer);
    procedure Execute; override;
  end;

constructor TListAddThread.Create(AStart, ACount: Integer);
begin
  inherited Create(True);
  FStart := AStart;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TListAddThread.Execute;
var I: Integer;
begin
  for I := FStart to FStart + FCount - 1 do
    GList.Add(GIntegers[I]);
end;

procedure ListMTAdd;
var
  Threads: array[0..THREAD_COUNT - 1] of TListAddThread;
  PerThread, I: Integer;
begin
  GList := TIntList.Create(@CmpInt);
  try
    PerThread := ITEM_COUNT div THREAD_COUNT;
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I] := TListAddThread.Create(I * PerThread, PerThread);
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I].Start;
    for I := 0 to THREAD_COUNT - 1 do
    begin
      Threads[I].WaitFor;
      Threads[I].Free;
    end;
  finally
    GList.Free;
    GList := nil;
  end;
end;

{ Multi-threaded List mixed read+write }

type
  TListMixedThread = class(TThread)
  private
    FStart, FCount: Integer;
  public
    constructor Create(AStart, ACount: Integer);
    procedure Execute; override;
  end;

constructor TListMixedThread.Create(AStart, ACount: Integer);
begin
  inherited Create(True);
  FStart := AStart;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TListMixedThread.Execute;
var I: Integer;
begin
  // Only Add — Contains on a large unsorted list is O(n) per call,
  // making a mixed Add+Contains benchmark impractical at ITEM_COUNT scale.
  for I := FStart to FStart + FCount - 1 do
    GList.Add(GIntegers[I]);
end;

procedure ListMTMixed;
var
  Threads: array[0..THREAD_COUNT - 1] of TListMixedThread;
  PerThread, I: Integer;
begin
  GList := TIntList.Create(@CmpInt);
  try
    PerThread := ITEM_COUNT div THREAD_COUNT;
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I] := TListMixedThread.Create(I * PerThread, PerThread);
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I].Start;
    for I := 0 to THREAD_COUNT - 1 do
    begin
      Threads[I].WaitFor;
      Threads[I].Free;
    end;
  finally
    GList.Free;
    GList := nil;
  end;
end;

{ ============================================================
  TThreadSafeDictionary<string,Integer> benchmarks
  ============================================================ }

var
  GDict: TStrIntDict;

procedure DictAdd;
var I: Integer;
begin
  GDict := TStrIntDict.Create;
  try
    for I := 0 to ITEM_COUNT - 1 do
      GDict.Add(GStrings[I], I);
  finally
    GDict.Free;
    GDict := nil;
  end;
end;

procedure DictTryGet;
var I, V: Integer; B: Boolean;
begin
  GDict := TStrIntDict.Create;
  try
    for I := 0 to ITEM_COUNT - 1 do
      GDict.Add(GStrings[I], I);
    for I := 0 to ITEM_COUNT - 1 do
      B := GDict.TryGetValue(GStrings[I], V);
  finally
    GDict.Free;
    GDict := nil;
  end;
  B := B;
end;

procedure DictRemove;
var I: Integer;
begin
  GDict := TStrIntDict.Create;
  try
    for I := 0 to ITEM_COUNT - 1 do
      GDict.Add(GStrings[I], I);
    for I := 0 to ITEM_COUNT - 1 do
      GDict.Remove(GStrings[I]);
  finally
    GDict.Free;
    GDict := nil;
  end;
end;

procedure DictIterate;
var
  Iter: TStrIntDict.TEnumerator;
  I: Integer;
begin
  GDict := TStrIntDict.Create;
  try
    for I := 0 to ITEM_COUNT - 1 do
      GDict.Add(GStrings[I], I);
    Iter := GDict.GetEnumerator;
    try
      while Iter.MoveNext do ;
    finally
      Iter.Free;
    end;
  finally
    GDict.Free;
    GDict := nil;
  end;
end;

{ Multi-threaded Dictionary add }

type
  TDictAddThread = class(TThread)
  private
    FStart, FCount: Integer;
  public
    constructor Create(AStart, ACount: Integer);
    procedure Execute; override;
  end;

constructor TDictAddThread.Create(AStart, ACount: Integer);
begin
  inherited Create(True);
  FStart := AStart;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TDictAddThread.Execute;
var I: Integer;
begin
  for I := FStart to FStart + FCount - 1 do
    GDict.Add(GStrings[I], I);
end;

procedure DictMTAdd;
var
  Threads: array[0..THREAD_COUNT - 1] of TDictAddThread;
  PerThread, I: Integer;
begin
  GDict := TStrIntDict.Create;
  try
    PerThread := ITEM_COUNT div THREAD_COUNT;
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I] := TDictAddThread.Create(I * PerThread, PerThread);
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I].Start;
    for I := 0 to THREAD_COUNT - 1 do
    begin
      Threads[I].WaitFor;
      Threads[I].Free;
    end;
  finally
    GDict.Free;
    GDict := nil;
  end;
end;

{ Multi-threaded Dictionary mixed read+write }

type
  TDictMixedThread = class(TThread)
  private
    FStart, FCount: Integer;
  public
    constructor Create(AStart, ACount: Integer);
    procedure Execute; override;
  end;

constructor TDictMixedThread.Create(AStart, ACount: Integer);
begin
  inherited Create(True);
  FStart := AStart;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TDictMixedThread.Execute;
var I, V: Integer; B: Boolean;
begin
  for I := FStart to FStart + FCount - 1 do
  begin
    if I mod 2 = 0 then
      GDict.Add(GStrings[I], I)
    else
      B := GDict.TryGetValue(GStrings[I], V);
  end;
  B := B;
end;

procedure DictMTMixed;
var
  Threads: array[0..THREAD_COUNT - 1] of TDictMixedThread;
  PerThread, I: Integer;
begin
  GDict := TStrIntDict.Create;
  try
    PerThread := ITEM_COUNT div THREAD_COUNT;
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I] := TDictMixedThread.Create(I * PerThread, PerThread);
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I].Start;
    for I := 0 to THREAD_COUNT - 1 do
    begin
      Threads[I].WaitFor;
      Threads[I].Free;
    end;
  finally
    GDict.Free;
    GDict := nil;
  end;
end;

{ ============================================================
  TThreadSafeHashSet<Integer> benchmarks
  ============================================================ }

var
  GSet: TIntSet;

procedure SetAdd;
var I: Integer;
begin
  GSet := TIntSet.Create;
  try
    for I := 0 to ITEM_COUNT - 1 do
      GSet.Add(GIntegers[I]);
  finally
    GSet.Free;
    GSet := nil;
  end;
end;

procedure SetContains;
var I: Integer; B: Boolean;
begin
  GSet := TIntSet.Create;
  try
    for I := 0 to ITEM_COUNT - 1 do
      GSet.Add(GIntegers[I]);
    for I := 0 to ITEM_COUNT - 1 do
      B := GSet.Contains(GIntegers[I]);
  finally
    GSet.Free;
    GSet := nil;
  end;
  B := B;
end;

procedure SetRemove;
var I: Integer;
begin
  GSet := TIntSet.Create;
  try
    for I := 0 to ITEM_COUNT - 1 do
      GSet.Add(GIntegers[I]);
    for I := 0 to ITEM_COUNT - 1 do
      GSet.Remove(GIntegers[I]);
  finally
    GSet.Free;
    GSet := nil;
  end;
end;

procedure SetIterate;
var
  Iter: specialize TThreadSafeHashSet<Integer>.TEnumerator;
  I: Integer;
begin
  GSet := TIntSet.Create;
  try
    for I := 0 to ITEM_COUNT - 1 do
      GSet.Add(GIntegers[I]);
    Iter := GSet.GetEnumerator;
    try
      while Iter.MoveNext do ;
    finally
      Iter.Free;
    end;
  finally
    GSet.Free;
    GSet := nil;
  end;
end;

{ Multi-threaded HashSet add }

type
  TSetAddThread = class(TThread)
  private
    FStart, FCount: Integer;
  public
    constructor Create(AStart, ACount: Integer);
    procedure Execute; override;
  end;

constructor TSetAddThread.Create(AStart, ACount: Integer);
begin
  inherited Create(True);
  FStart := AStart;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TSetAddThread.Execute;
var I: Integer;
begin
  for I := FStart to FStart + FCount - 1 do
    GSet.Add(GIntegers[I]);
end;

procedure SetMTAdd;
var
  Threads: array[0..THREAD_COUNT - 1] of TSetAddThread;
  PerThread, I: Integer;
begin
  GSet := TIntSet.Create;
  try
    PerThread := ITEM_COUNT div THREAD_COUNT;
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I] := TSetAddThread.Create(I * PerThread, PerThread);
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I].Start;
    for I := 0 to THREAD_COUNT - 1 do
    begin
      Threads[I].WaitFor;
      Threads[I].Free;
    end;
  finally
    GSet.Free;
    GSet := nil;
  end;
end;

{ Multi-threaded HashSet mixed read+write }

type
  TSetMixedThread = class(TThread)
  private
    FStart, FCount: Integer;
  public
    constructor Create(AStart, ACount: Integer);
    procedure Execute; override;
  end;

constructor TSetMixedThread.Create(AStart, ACount: Integer);
begin
  inherited Create(True);
  FStart := AStart;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TSetMixedThread.Execute;
var I: Integer; B: Boolean;
begin
  for I := FStart to FStart + FCount - 1 do
  begin
    if I mod 2 = 0 then
      GSet.Add(GIntegers[I])
    else
      B := GSet.Contains(GIntegers[I]);
  end;
  B := B;
end;

procedure SetMTMixed;
var
  Threads: array[0..THREAD_COUNT - 1] of TSetMixedThread;
  PerThread, I: Integer;
begin
  GSet := TIntSet.Create;
  try
    PerThread := ITEM_COUNT div THREAD_COUNT;
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I] := TSetMixedThread.Create(I * PerThread, PerThread);
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I].Start;
    for I := 0 to THREAD_COUNT - 1 do
    begin
      Threads[I].WaitFor;
      Threads[I].Free;
    end;
  finally
    GSet.Free;
    GSet := nil;
  end;
end;

{ ============================================================
  TThreadSafeDeque<Integer> benchmarks
  ============================================================ }

var
  GDeque: TIntDeque;

procedure DequePushBack;
var I: Integer;
begin
  GDeque := TIntDeque.Create;
  try
    for I := 0 to ITEM_COUNT - 1 do
      GDeque.PushBack(GIntegers[I]);
  finally
    GDeque.Free;
    GDeque := nil;
  end;
end;

procedure DequePushFront;
var I: Integer;
begin
  GDeque := TIntDeque.Create;
  try
    for I := 0 to ITEM_COUNT - 1 do
      GDeque.PushFront(GIntegers[I]);
  finally
    GDeque.Free;
    GDeque := nil;
  end;
end;

procedure DequePopBack;
var I, V: Integer;
begin
  GDeque := TIntDeque.Create;
  try
    for I := 0 to ITEM_COUNT - 1 do
      GDeque.PushBack(GIntegers[I]);
    for I := 0 to ITEM_COUNT - 1 do
      V := GDeque.PopBack;
  finally
    GDeque.Free;
    GDeque := nil;
  end;
  V := V;
end;

procedure DequePopFront;
var I, V: Integer;
begin
  GDeque := TIntDeque.Create;
  try
    for I := 0 to ITEM_COUNT - 1 do
      GDeque.PushBack(GIntegers[I]);
    for I := 0 to ITEM_COUNT - 1 do
      V := GDeque.PopFront;
  finally
    GDeque.Free;
    GDeque := nil;
  end;
  V := V;
end;

procedure DequeIterate;
var
  Iter: TIntDeque.TEnumerator;
  I: Integer;
begin
  GDeque := TIntDeque.Create;
  try
    for I := 0 to ITEM_COUNT - 1 do
      GDeque.PushBack(GIntegers[I]);
    Iter := GDeque.GetEnumerator;
    try
      while Iter.MoveNext do ;
    finally
      Iter.Free;
    end;
  finally
    GDeque.Free;
    GDeque := nil;
  end;
end;

{ Multi-threaded Deque push/pop }

type
  TDequePushThread = class(TThread)
  private
    FStart, FCount: Integer;
  public
    constructor Create(AStart, ACount: Integer);
    procedure Execute; override;
  end;

constructor TDequePushThread.Create(AStart, ACount: Integer);
begin
  inherited Create(True);
  FStart := AStart;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TDequePushThread.Execute;
var I: Integer;
begin
  for I := FStart to FStart + FCount - 1 do
    GDeque.PushBack(GIntegers[I]);
end;

procedure DequeMTPush;
var
  Threads: array[0..THREAD_COUNT - 1] of TDequePushThread;
  PerThread, I: Integer;
begin
  GDeque := TIntDeque.Create;
  try
    PerThread := ITEM_COUNT div THREAD_COUNT;
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I] := TDequePushThread.Create(I * PerThread, PerThread);
    for I := 0 to THREAD_COUNT - 1 do
      Threads[I].Start;
    for I := 0 to THREAD_COUNT - 1 do
    begin
      Threads[I].WaitFor;
      Threads[I].Free;
    end;
  finally
    GDeque.Free;
    GDeque := nil;
  end;
end;

{ Multi-threaded Deque mixed producer/consumer }

type
  TDequeProducerThread = class(TThread)
  private
    FStart, FCount: Integer;
  public
    constructor Create(AStart, ACount: Integer);
    procedure Execute; override;
  end;

  TDequeConsumerThread = class(TThread)
  private
    FCount: Integer;
  public
    constructor Create(ACount: Integer);
    procedure Execute; override;
  end;

constructor TDequeProducerThread.Create(AStart, ACount: Integer);
begin
  inherited Create(True);
  FStart := AStart;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TDequeProducerThread.Execute;
var I: Integer;
begin
  for I := FStart to FStart + FCount - 1 do
    GDeque.PushBack(GIntegers[I]);
end;

constructor TDequeConsumerThread.Create(ACount: Integer);
begin
  inherited Create(True);
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TDequeConsumerThread.Execute;
var I, V: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    while not GDeque.TryPopFront(V) do
      Sleep(0);
  end;
end;

procedure DequeMTProducerConsumer;
var
  Producers: array[0..THREAD_COUNT div 2 - 1] of TDequeProducerThread;
  Consumers: array[0..THREAD_COUNT div 2 - 1] of TDequeConsumerThread;
  HalfThreads, PerProducer, PerConsumer, I: Integer;
begin
  HalfThreads := THREAD_COUNT div 2;
  PerProducer  := ITEM_COUNT div HalfThreads;
  PerConsumer  := ITEM_COUNT div HalfThreads;

  GDeque := TIntDeque.Create;
  try
    for I := 0 to HalfThreads - 1 do
      Producers[I] := TDequeProducerThread.Create(I * PerProducer, PerProducer);
    for I := 0 to HalfThreads - 1 do
      Consumers[I] := TDequeConsumerThread.Create(PerConsumer);

    for I := 0 to HalfThreads - 1 do Producers[I].Start;
    for I := 0 to HalfThreads - 1 do Consumers[I].Start;
    for I := 0 to HalfThreads - 1 do begin Producers[I].WaitFor; Producers[I].Free; end;
    for I := 0 to HalfThreads - 1 do begin Consumers[I].WaitFor; Consumers[I].Free; end;
  finally
    GDeque.Free;
    GDeque := nil;
  end;
end;

{ ============================================================
  Main
  ============================================================ }

var
  Avg, Best: Int64;

begin
  WriteLn('ThreadSafeCollections-FP Benchmark');
  WriteLn('====================================');
  WriteLn(Format('  Items per run : %d', [ITEM_COUNT]));
  WriteLn(Format('  Runs          : %d  (trimming best+worst %d each)', [RUNS, TRIM]));
  WriteLn(Format('  Threads (MT)  : %d', [THREAD_COUNT]));
  WriteLn;

  BuildData;

  { ---- List ---- }
  Separator('TThreadSafeList<Integer>  —  single-threaded');
  RunScenario(@ListAdd,      Avg, Best); PrintResult('List', 'Add',          ITEM_COUNT, Avg, Best);
  RunScenario(@ListContains, Avg, Best); PrintResult('List', 'Contains (1k)', CONTAINS_COUNT, Avg, Best);
  RunScenario(@ListSort,     Avg, Best); PrintResult('List', 'Sort',         ITEM_COUNT, Avg, Best);
  RunScenario(@ListIterate,  Avg, Best); PrintResult('List', 'Iterate',      ITEM_COUNT, Avg, Best);
  RunScenario(@ListDelete,   Avg, Best); PrintResult('List', 'Delete (all)', ITEM_COUNT, Avg, Best);

  Separator('TThreadSafeList<Integer>  —  multi-threaded (' + IntToStr(THREAD_COUNT) + ' threads)');
  RunScenario(@ListMTAdd,    Avg, Best); PrintResult('List', 'MT Add',         ITEM_COUNT, Avg, Best);
  RunScenario(@ListMTMixed,  Avg, Best); PrintResult('List', 'MT Add (4 threads)', ITEM_COUNT, Avg, Best);

  { ---- Dictionary ---- }
  Separator('TThreadSafeDictionary<string,Integer>  —  single-threaded');
  RunScenario(@DictAdd,     Avg, Best); PrintResult('Dictionary', 'Add',        ITEM_COUNT, Avg, Best);
  RunScenario(@DictTryGet,  Avg, Best); PrintResult('Dictionary', 'TryGetValue',ITEM_COUNT, Avg, Best);
  RunScenario(@DictIterate, Avg, Best); PrintResult('Dictionary', 'Iterate',    ITEM_COUNT, Avg, Best);
  RunScenario(@DictRemove,  Avg, Best); PrintResult('Dictionary', 'Remove',     ITEM_COUNT, Avg, Best);

  Separator('TThreadSafeDictionary<string,Integer>  —  multi-threaded (' + IntToStr(THREAD_COUNT) + ' threads)');
  RunScenario(@DictMTAdd,   Avg, Best); PrintResult('Dictionary', 'MT Add',         ITEM_COUNT, Avg, Best);
  RunScenario(@DictMTMixed, Avg, Best); PrintResult('Dictionary', 'MT Add+TryGet',  ITEM_COUNT, Avg, Best);

  { ---- HashSet ---- }
  Separator('TThreadSafeHashSet<Integer>  —  single-threaded');
  RunScenario(@SetAdd,      Avg, Best); PrintResult('HashSet', 'Add',      ITEM_COUNT, Avg, Best);
  RunScenario(@SetContains, Avg, Best); PrintResult('HashSet', 'Contains', ITEM_COUNT, Avg, Best);
  RunScenario(@SetIterate,  Avg, Best); PrintResult('HashSet', 'Iterate',  ITEM_COUNT, Avg, Best);
  RunScenario(@SetRemove,   Avg, Best); PrintResult('HashSet', 'Remove',   ITEM_COUNT, Avg, Best);

  Separator('TThreadSafeHashSet<Integer>  —  multi-threaded (' + IntToStr(THREAD_COUNT) + ' threads)');
  RunScenario(@SetMTAdd,    Avg, Best); PrintResult('HashSet', 'MT Add',         ITEM_COUNT, Avg, Best);
  RunScenario(@SetMTMixed,  Avg, Best); PrintResult('HashSet', 'MT Add+Contains',ITEM_COUNT, Avg, Best);

  { ---- Deque ---- }
  Separator('TThreadSafeDeque<Integer>  —  single-threaded');
  RunScenario(@DequePushBack,  Avg, Best); PrintResult('Deque', 'PushBack',  ITEM_COUNT, Avg, Best);
  RunScenario(@DequePushFront, Avg, Best); PrintResult('Deque', 'PushFront', ITEM_COUNT, Avg, Best);
  RunScenario(@DequePopFront,  Avg, Best); PrintResult('Deque', 'PopFront',  ITEM_COUNT, Avg, Best);
  RunScenario(@DequePopBack,   Avg, Best); PrintResult('Deque', 'PopBack',   ITEM_COUNT, Avg, Best);
  RunScenario(@DequeIterate,   Avg, Best); PrintResult('Deque', 'Iterate',   ITEM_COUNT, Avg, Best);

  Separator('TThreadSafeDeque<Integer>  —  multi-threaded (' + IntToStr(THREAD_COUNT) + ' threads)');
  RunScenario(@DequeMTPush,             Avg, Best); PrintResult('Deque', 'MT PushBack',        ITEM_COUNT, Avg, Best);
  RunScenario(@DequeMTProducerConsumer, Avg, Best); PrintResult('Deque', 'MT Producer/Consumer',ITEM_COUNT, Avg, Best);

  WriteLn;
  WriteLn('Done.');
end.
