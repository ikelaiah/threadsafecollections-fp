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
      Add / PushBack        GItemCount items
      Contains / TryGetValue / Peek
      Remove / PopFront
      Iterate (for-in)
      Sort (List only)

    Multi-threaded  (THREAD_COUNT concurrent threads, GItemCount total)
      Concurrent Add
      Concurrent mixed read+write

  Output
    One line per scenario:
      [Collection] [Scenario]  N items  avg X ms  best Y ms  ops/s Z
}

uses
  SysUtils, Classes, DateUtils, Math, SyncObjs, Windows,
  ThreadSafeCollections.List,
  ThreadSafeCollections.Dictionary,
  ThreadSafeCollections.HashSet,
  ThreadSafeCollections.Deque,
  HashFunctions;

const
  MAX_ITEMS    = 1000000;  // maximum collection size (pre-allocates data arrays)
  THREAD_COUNT = 4;        // threads for multi-threaded scenarios
  RUNS         = 5;        // total runs per scenario (best+worst trimmed)
  TRIM         = 1;        // runs to drop from each end before averaging

  SIZES: array[0..3] of Integer = (1000, 10000, 100000, 1000000);

var
  GItemCount:     Integer;  // active size for current pass
  GContainsCount: Integer;  // 1% of GItemCount for O(n) Contains

{ ============================================================
  Helpers
  ============================================================ }

type
  TTimings = array of Int64;   // milliseconds per run

{ Run AProc RUNS times, return trimmed-mean ms and best ms. }
var
  GPerfFreq: Int64;  // counts per second, initialised once in main block

procedure RunScenario(AProc: TProcedure; out AvgUs, BestUs: Int64);
var
  Timings: TTimings;
  Sorted: TTimings;
  I, J, K: Integer;
  C0, C1: Int64;
  Sum, Tmp: Int64;
begin
  SetLength(Timings, RUNS);
  for I := 0 to RUNS - 1 do
  begin
    QueryPerformanceCounter(C0);
    AProc;
    QueryPerformanceCounter(C1);
    Timings[I] := Round((C1 - C0) * 1000000.0 / GPerfFreq);
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

  BestUs := Sorted[0];
  Sum := 0;
  for I := TRIM to RUNS - 1 - TRIM do
    Sum := Sum + Sorted[I];
  AvgUs := Sum div (RUNS - 2 * TRIM);
end;

var
  GCsvFile: TextFile;
  GTimestamp: string;

procedure PrintResult(const Collection, Scenario: string;
                      N: Integer; AvgUs, BestUs: Int64);
var
  OpsPerSec: Int64;
begin
  if AvgUs > 0 then
    OpsPerSec := Round(N / (AvgUs / 1000000.0))
  else
    OpsPerSec := 0;
  // Console
  WriteLn(Format('  %-14s  %-32s  %7d items  avg %7d us  best %7d us  %12d ops/s',
    [Collection, Scenario, N, AvgUs, BestUs, OpsPerSec]));
  // CSV: timestamp,collection,scenario,items,avg_us,best_us,ops_per_sec
  WriteLn(GCsvFile, Format('%s,%s,%s,%d,%d,%d,%d',
    [GTimestamp, Collection, Scenario, N, AvgUs, BestUs, OpsPerSec]));
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

const
  SHORT_STR_LEN = 8;
  LONG_STR_LEN  = 200;

var
  GIntegers:     array[0..MAX_ITEMS - 1] of Integer;
  GStrings:      array[0..MAX_ITEMS - 1] of string;
  GShortStrings: array[0..MAX_ITEMS - 1] of string;
  GLongStrings:  array[0..MAX_ITEMS - 1] of string;
  GRandomOrder:  array[0..MAX_ITEMS - 1] of Integer;

function MakeString(const Prefix: string; Index, TotalLen: Integer): string;
var
  S: string;
begin
  S := Prefix + IntToStr(Index);
  while Length(S) < TotalLen do
    S := S + 'x';
  SetLength(S, TotalLen);
  Result := S;
end;

procedure BuildData(N: Integer);
var
  I, J, Tmp: Integer;
begin
  GItemCount     := N;
  GContainsCount := Max(1, N div 100);  // 1% of N, minimum 1
  RandSeed := 42;
  for I := 0 to N - 1 do
  begin
    GIntegers[I]     := I;
    GStrings[I]      := 'key_' + IntToStr(I);
    GShortStrings[I] := MakeString('k', I, SHORT_STR_LEN);
    GLongStrings[I]  := MakeString('key_', I, LONG_STR_LEN);
    GRandomOrder[I]  := I;
  end;
  // Shuffle integers (for Sort benchmark)
  for I := N - 1 downto 1 do
  begin
    J := Random(I + 1);
    Tmp := GIntegers[I];
    GIntegers[I] := GIntegers[J];
    GIntegers[J] := Tmp;
  end;
  // Shuffle GRandomOrder (for random-read benchmarks)
  for I := N - 1 downto 1 do
  begin
    J := Random(I + 1);
    Tmp := GRandomOrder[I];
    GRandomOrder[I] := GRandomOrder[J];
    GRandomOrder[J] := Tmp;
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
    for I := 0 to GItemCount - 1 do
      GList.Add(GIntegers[I]);
  finally
    GList.Free;
    GList := nil;
  end;
end;

// List.Contains is a linear scan (O(n)) — use GContainsCount (1% of GItemCount)
// to avoid an O(n^2) benchmark that takes minutes.

procedure ListContains;
var I: Integer; B: Boolean;
begin
  GList := TIntList.Create(@CmpInt);
  try
    for I := 0 to GItemCount - 1 do
      GList.Add(GIntegers[I]);
    for I := 0 to GContainsCount - 1 do
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
    for I := 0 to GItemCount - 1 do
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
    for I := 0 to GItemCount - 1 do
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
    for I := 0 to GItemCount - 1 do
      GList.Add(GIntegers[I]);
    for I := GItemCount - 1 downto 0 do
      GList.Delete(I);
  finally
    GList.Free;
    GList := nil;
  end;
end;

procedure ListReadSequential;
var I, V: Integer;
begin
  GList := TIntList.Create(@CmpInt);
  try
    for I := 0 to GItemCount - 1 do
      GList.Add(GIntegers[I]);
    for I := 0 to GItemCount - 1 do
      V := GList[I];
  finally
    GList.Free;
    GList := nil;
  end;
  V := V;
end;

procedure ListReadRandom;
var I, V: Integer;
begin
  GList := TIntList.Create(@CmpInt);
  try
    for I := 0 to GItemCount - 1 do
      GList.Add(GIntegers[I]);
    for I := 0 to GItemCount - 1 do
      V := GList[GRandomOrder[I]];
  finally
    GList.Free;
    GList := nil;
  end;
  V := V;
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
    PerThread := GItemCount div THREAD_COUNT;
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
    for I := 0 to GItemCount - 1 do
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
    for I := 0 to GItemCount - 1 do
      GDict.Add(GStrings[I], I);
    for I := 0 to GItemCount - 1 do
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
    for I := 0 to GItemCount - 1 do
      GDict.Add(GStrings[I], I);
    for I := 0 to GItemCount - 1 do
      GDict.Remove(GStrings[I]);
  finally
    GDict.Free;
    GDict := nil;
  end;
end;

procedure DictShortStringAdd;
var I: Integer;
begin
  GDict := TStrIntDict.Create;
  try
    for I := 0 to GItemCount - 1 do
      GDict.Add(GShortStrings[I], I);
  finally
    GDict.Free;
    GDict := nil;
  end;
end;

procedure DictShortStringRead;
var I, V: Integer; B: Boolean;
begin
  GDict := TStrIntDict.Create;
  try
    for I := 0 to GItemCount - 1 do
      GDict.Add(GShortStrings[I], I);
    for I := 0 to GItemCount - 1 do
      B := GDict.TryGetValue(GShortStrings[I], V);
  finally
    GDict.Free;
    GDict := nil;
  end;
  B := B;
end;

procedure DictLongStringAdd;
var I: Integer;
begin
  GDict := TStrIntDict.Create;
  try
    for I := 0 to GItemCount - 1 do
      GDict.Add(GLongStrings[I], I);
  finally
    GDict.Free;
    GDict := nil;
  end;
end;

procedure DictLongStringRead;
var I, V: Integer; B: Boolean;
begin
  GDict := TStrIntDict.Create;
  try
    for I := 0 to GItemCount - 1 do
      GDict.Add(GLongStrings[I], I);
    for I := 0 to GItemCount - 1 do
      B := GDict.TryGetValue(GLongStrings[I], V);
  finally
    GDict.Free;
    GDict := nil;
  end;
  B := B;
end;

procedure DictReadSequential;
var I, V: Integer; B: Boolean;
begin
  GDict := TStrIntDict.Create;
  try
    for I := 0 to GItemCount - 1 do
      GDict.Add(GStrings[I], I);
    for I := 0 to GItemCount - 1 do
      B := GDict.TryGetValue(GStrings[I], V);
  finally
    GDict.Free;
    GDict := nil;
  end;
  B := B;
end;

procedure DictReadRandom;
var I, V: Integer; B: Boolean;
begin
  GDict := TStrIntDict.Create;
  try
    for I := 0 to GItemCount - 1 do
      GDict.Add(GStrings[I], I);
    for I := 0 to GItemCount - 1 do
      B := GDict.TryGetValue(GStrings[GRandomOrder[I]], V);
  finally
    GDict.Free;
    GDict := nil;
  end;
  B := B;
end;

procedure DictIterate;
var
  Iter: TStrIntDict.TEnumerator;
  I: Integer;
begin
  GDict := TStrIntDict.Create;
  try
    for I := 0 to GItemCount - 1 do
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
    PerThread := GItemCount div THREAD_COUNT;
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
    PerThread := GItemCount div THREAD_COUNT;
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
    for I := 0 to GItemCount - 1 do
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
    for I := 0 to GItemCount - 1 do
      GSet.Add(GIntegers[I]);
    for I := 0 to GItemCount - 1 do
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
    for I := 0 to GItemCount - 1 do
      GSet.Add(GIntegers[I]);
    for I := 0 to GItemCount - 1 do
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
    for I := 0 to GItemCount - 1 do
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

procedure SetReadRandom;
var I: Integer; B: Boolean;
begin
  GSet := TIntSet.Create;
  try
    for I := 0 to GItemCount - 1 do
      GSet.Add(I);
    for I := 0 to GItemCount - 1 do
      B := GSet.Contains(GRandomOrder[I]);
  finally
    GSet.Free;
    GSet := nil;
  end;
  B := B;
end;

var
  GStrSet: TThreadSafeHashSetString;

procedure SetShortStringAdd;
var I: Integer;
begin
  GStrSet := TThreadSafeHashSetString.Create;
  try
    for I := 0 to GItemCount - 1 do
      GStrSet.Add(GShortStrings[I]);
  finally
    GStrSet.Free;
    GStrSet := nil;
  end;
end;

procedure SetShortStringContains;
var I: Integer; B: Boolean;
begin
  GStrSet := TThreadSafeHashSetString.Create;
  try
    for I := 0 to GItemCount - 1 do
      GStrSet.Add(GShortStrings[I]);
    for I := 0 to GItemCount - 1 do
      B := GStrSet.Contains(GShortStrings[I]);
  finally
    GStrSet.Free;
    GStrSet := nil;
  end;
  B := B;
end;

procedure SetLongStringAdd;
var I: Integer;
begin
  GStrSet := TThreadSafeHashSetString.Create;
  try
    for I := 0 to GItemCount - 1 do
      GStrSet.Add(GLongStrings[I]);
  finally
    GStrSet.Free;
    GStrSet := nil;
  end;
end;

procedure SetLongStringContains;
var I: Integer; B: Boolean;
begin
  GStrSet := TThreadSafeHashSetString.Create;
  try
    for I := 0 to GItemCount - 1 do
      GStrSet.Add(GLongStrings[I]);
    for I := 0 to GItemCount - 1 do
      B := GStrSet.Contains(GLongStrings[I]);
  finally
    GStrSet.Free;
    GStrSet := nil;
  end;
  B := B;
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
    PerThread := GItemCount div THREAD_COUNT;
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
    PerThread := GItemCount div THREAD_COUNT;
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
    for I := 0 to GItemCount - 1 do
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
    for I := 0 to GItemCount - 1 do
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
    for I := 0 to GItemCount - 1 do
      GDeque.PushBack(GIntegers[I]);
    for I := 0 to GItemCount - 1 do
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
    for I := 0 to GItemCount - 1 do
      GDeque.PushBack(GIntegers[I]);
    for I := 0 to GItemCount - 1 do
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
    for I := 0 to GItemCount - 1 do
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
    PerThread := GItemCount div THREAD_COUNT;
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
  PerProducer  := GItemCount div HalfThreads;
  PerConsumer  := GItemCount div HalfThreads;

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
  RunAllScenarios — runs every benchmark at current GItemCount
  ============================================================ }

procedure RunAllScenarios;
var
  Avg, Best: Int64;  // microseconds
  T: string;
begin
  T := IntToStr(GItemCount);
  WriteLn;
  WriteLn('════════════════════════════════════════════════════');
  WriteLn('  N = ', GItemCount, ' items');
  WriteLn('════════════════════════════════════════════════════');

  { ---- List ---- }
  Separator('TThreadSafeList<Integer>  —  single-threaded');
  RunScenario(@ListAdd,            Avg, Best); PrintResult('List', 'Add',             GItemCount,     Avg, Best);
  RunScenario(@ListContains,       Avg, Best); PrintResult('List', 'Contains (1%)',   GContainsCount, Avg, Best);
  RunScenario(@ListReadSequential, Avg, Best); PrintResult('List', 'Read sequential', GItemCount,     Avg, Best);
  RunScenario(@ListReadRandom,     Avg, Best); PrintResult('List', 'Read random',     GItemCount,     Avg, Best);
  RunScenario(@ListSort,           Avg, Best); PrintResult('List', 'Sort',            GItemCount,     Avg, Best);
  RunScenario(@ListIterate,        Avg, Best); PrintResult('List', 'Iterate',         GItemCount,     Avg, Best);
  RunScenario(@ListDelete,         Avg, Best); PrintResult('List', 'Delete (all)',    GItemCount,     Avg, Best);

  Separator('TThreadSafeList  —  MT ' + T + ' items / ' + IntToStr(THREAD_COUNT) + ' threads');
  RunScenario(@ListMTAdd, Avg, Best); PrintResult('List', 'MT Add', GItemCount, Avg, Best);

  { ---- Dictionary ---- }
  Separator('TThreadSafeDictionary  —  single-threaded');
  RunScenario(@DictAdd,            Avg, Best); PrintResult('Dictionary', 'Add (~10 char keys)',  GItemCount, Avg, Best);
  RunScenario(@DictTryGet,         Avg, Best); PrintResult('Dictionary', 'TryGetValue',          GItemCount, Avg, Best);
  RunScenario(@DictReadSequential, Avg, Best); PrintResult('Dictionary', 'Read sequential',      GItemCount, Avg, Best);
  RunScenario(@DictReadRandom,     Avg, Best); PrintResult('Dictionary', 'Read random',          GItemCount, Avg, Best);
  RunScenario(@DictShortStringAdd, Avg, Best); PrintResult('Dictionary', 'Add (8 char keys)',    GItemCount, Avg, Best);
  RunScenario(@DictShortStringRead,Avg, Best); PrintResult('Dictionary', 'Read (8 char keys)',   GItemCount, Avg, Best);
  RunScenario(@DictLongStringAdd,  Avg, Best); PrintResult('Dictionary', 'Add (200 char keys)',  GItemCount, Avg, Best);
  RunScenario(@DictLongStringRead, Avg, Best); PrintResult('Dictionary', 'Read (200 char keys)', GItemCount, Avg, Best);
  RunScenario(@DictIterate,        Avg, Best); PrintResult('Dictionary', 'Iterate',              GItemCount, Avg, Best);
  RunScenario(@DictRemove,         Avg, Best); PrintResult('Dictionary', 'Remove',               GItemCount, Avg, Best);

  Separator('TThreadSafeDictionary  —  MT ' + T + ' items / ' + IntToStr(THREAD_COUNT) + ' threads');
  RunScenario(@DictMTAdd,   Avg, Best); PrintResult('Dictionary', 'MT Add',        GItemCount, Avg, Best);
  RunScenario(@DictMTMixed, Avg, Best); PrintResult('Dictionary', 'MT Add+TryGet', GItemCount, Avg, Best);

  { ---- HashSet ---- }
  Separator('TThreadSafeHashSet<Integer>  —  single-threaded');
  RunScenario(@SetAdd,             Avg, Best); PrintResult('HashSet', 'Add (int)',           GItemCount, Avg, Best);
  RunScenario(@SetContains,        Avg, Best); PrintResult('HashSet', 'Contains (int)',       GItemCount, Avg, Best);
  RunScenario(@SetReadRandom,      Avg, Best); PrintResult('HashSet', 'Contains random',      GItemCount, Avg, Best);
  RunScenario(@SetIterate,         Avg, Best); PrintResult('HashSet', 'Iterate',              GItemCount, Avg, Best);
  RunScenario(@SetRemove,          Avg, Best); PrintResult('HashSet', 'Remove',               GItemCount, Avg, Best);

  Separator('TThreadSafeHashSet<string>  —  single-threaded');
  RunScenario(@SetShortStringAdd,      Avg, Best); PrintResult('HashSet', 'Add (8 char)',        GItemCount, Avg, Best);
  RunScenario(@SetShortStringContains, Avg, Best); PrintResult('HashSet', 'Contains (8 char)',   GItemCount, Avg, Best);
  RunScenario(@SetLongStringAdd,       Avg, Best); PrintResult('HashSet', 'Add (200 char)',       GItemCount, Avg, Best);
  RunScenario(@SetLongStringContains,  Avg, Best); PrintResult('HashSet', 'Contains (200 char)',  GItemCount, Avg, Best);

  Separator('TThreadSafeHashSet  —  MT ' + T + ' items / ' + IntToStr(THREAD_COUNT) + ' threads');
  RunScenario(@SetMTAdd,   Avg, Best); PrintResult('HashSet', 'MT Add',          GItemCount, Avg, Best);
  RunScenario(@SetMTMixed, Avg, Best); PrintResult('HashSet', 'MT Add+Contains', GItemCount, Avg, Best);

  { ---- Deque ---- }
  Separator('TThreadSafeDeque<Integer>  —  single-threaded');
  RunScenario(@DequePushBack,  Avg, Best); PrintResult('Deque', 'PushBack',  GItemCount, Avg, Best);
  RunScenario(@DequePushFront, Avg, Best); PrintResult('Deque', 'PushFront', GItemCount, Avg, Best);
  RunScenario(@DequePopFront,  Avg, Best); PrintResult('Deque', 'PopFront',  GItemCount, Avg, Best);
  RunScenario(@DequePopBack,   Avg, Best); PrintResult('Deque', 'PopBack',   GItemCount, Avg, Best);
  RunScenario(@DequeIterate,   Avg, Best); PrintResult('Deque', 'Iterate',   GItemCount, Avg, Best);

  Separator('TThreadSafeDeque  —  MT ' + T + ' items / ' + IntToStr(THREAD_COUNT) + ' threads');
  RunScenario(@DequeMTPush,             Avg, Best); PrintResult('Deque', 'MT PushBack',          GItemCount, Avg, Best);
  RunScenario(@DequeMTProducerConsumer, Avg, Best); PrintResult('Deque', 'MT Producer/Consumer',  GItemCount, Avg, Best);
end;

{ ============================================================
  Main
  ============================================================ }

var
  CsvFilename: string;
  SizeIdx: Integer;

begin
  QueryPerformanceFrequency(GPerfFreq);
  GTimestamp  := FormatDateTime('yyyymmdd_hhnnss', Now);
  CsvFilename := 'benchmark_' + GTimestamp + '.csv';

  AssignFile(GCsvFile, CsvFilename);
  Rewrite(GCsvFile);
  WriteLn(GCsvFile, 'timestamp,collection,scenario,items,avg_us,best_us,ops_per_sec');

  WriteLn('ThreadSafeCollections-FP Benchmark');
  WriteLn('====================================');
  WriteLn(Format('  Sizes         : 1k / 10k / 100k / 1M', []));
  WriteLn(Format('  Runs per size : %d  (trimming best+worst %d each)', [RUNS, TRIM]));
  WriteLn(Format('  Threads (MT)  : %d', [THREAD_COUNT]));
  WriteLn(Format('  CSV output    : %s', [CsvFilename]));

  for SizeIdx := 0 to High(SIZES) do
  begin
    BuildData(SIZES[SizeIdx]);
    RunAllScenarios;
  end;

  WriteLn;
  WriteLn('Done. Results saved to: ', CsvFilename);
  CloseFile(GCsvFile);
end.
