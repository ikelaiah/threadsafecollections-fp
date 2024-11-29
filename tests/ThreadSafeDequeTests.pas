unit ThreadSafeDequeTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  ThreadSafeCollections.Deque, DateUtils;

type
  TIntegerDeque = specialize TThreadSafeDeque<Integer>;

  TThreadSafeDequeTests = class(TTestCase)
  private
    FDeque: TIntegerDeque;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPushFrontAndPopFront;
    procedure TestPushBackAndPopBack;
    procedure TestMixedOperations;
    procedure TestClear;
    procedure TestPeekOperations;
    procedure TestTryOperations;
    procedure TestIsEmpty;
    procedure TestToArray;
    procedure TestContains;
    procedure TestPushRange;
    procedure TestMultiThreadPushPop;
    procedure TestLockingMechanism;
  end;

type
  TTestThread = class(TThread)
  private
    FDeque: TIntegerDeque;
    FStartValue: Integer;
    FCount: Integer;
    FIterations: Integer;
    FSeed: Integer;
  public
    constructor Create(ADeque: TIntegerDeque; AStartValue, ACount, AIterations, ASeed: Integer);
    procedure Execute; override;
  private
    function Random(Range: Integer): Integer;
  end;

type
  TLockTestThread = class(TThread)
  private
    FDeque: TIntegerDeque;
    FLockCount: Integer;
    FIterations: Integer;
  public
    constructor Create(ADeque: TIntegerDeque; AIterations: Integer);
    procedure Execute; override;
    property LockCount: Integer read FLockCount;
  end;

implementation

procedure TThreadSafeDequeTests.SetUp;
begin
  FDeque := TIntegerDeque.Create;
end;

procedure TThreadSafeDequeTests.TearDown;
begin
  FDeque.Free;
end;

procedure TThreadSafeDequeTests.TestPushFrontAndPopFront;
begin
  FDeque.PushFront(42);
  FDeque.PushFront(24);
  
  AssertEquals('Count should be 2', 2, FDeque.Count);
  AssertEquals('First pop should return 24', 24, FDeque.PopFront);
  AssertEquals('Second pop should return 42', 42, FDeque.PopFront);
  AssertEquals('Count should be 0', 0, FDeque.Count);
end;

procedure TThreadSafeDequeTests.TestPushBackAndPopBack;
begin
  FDeque.PushBack(42);
  FDeque.PushBack(24);
  
  AssertEquals('Count should be 2', 2, FDeque.Count);
  AssertEquals('First pop should return 24', 24, FDeque.PopBack);
  AssertEquals('Second pop should return 42', 42, FDeque.PopBack);
  AssertEquals('Count should be 0', 0, FDeque.Count);
end;

procedure TThreadSafeDequeTests.TestMixedOperations;
begin
  FDeque.PushFront(1);
  FDeque.PushBack(2);
  FDeque.PushFront(3);
  
  AssertEquals('Count should be 3', 3, FDeque.Count);
  AssertEquals('First pop should return 3', 3, FDeque.PopFront);
  AssertEquals('Last pop should return 2', 2, FDeque.PopBack);
  AssertEquals('Remaining value should be 1', 1, FDeque.PopFront);
end;

procedure TThreadSafeDequeTests.TestClear;
begin
  FDeque.PushFront(42);
  FDeque.PushBack(24);
  
  AssertEquals('Count should be 2', 2, FDeque.Count);
  FDeque.Clear;
  AssertEquals('Count should be 0 after clear', 0, FDeque.Count);
end;

procedure TThreadSafeDequeTests.TestPeekOperations;
begin
  FDeque.PushFront(42);
  FDeque.PushBack(24);
  
  AssertEquals('PeekFront should return 42', 42, FDeque.PeekFront);
  AssertEquals('PeekBack should return 24', 24, FDeque.PeekBack);
  AssertEquals('Count should still be 2', 2, FDeque.Count);
end;

procedure TThreadSafeDequeTests.TestTryOperations;
var
  Value: Integer;
begin
  AssertFalse('TryPopFront should return False when empty', FDeque.TryPopFront(Value));
  AssertFalse('TryPopBack should return False when empty', FDeque.TryPopBack(Value));
  AssertFalse('TryPeekFront should return False when empty', FDeque.TryPeekFront(Value));
  AssertFalse('TryPeekBack should return False when empty', FDeque.TryPeekBack(Value));
  
  FDeque.PushFront(42);
  
  AssertTrue('TryPeekFront should return True', FDeque.TryPeekFront(Value));
  AssertEquals('TryPeekFront value should be 42', 42, Value);
  
  AssertTrue('TryPopFront should return True', FDeque.TryPopFront(Value));
  AssertEquals('TryPopFront value should be 42', 42, Value);
  
  AssertEquals('Count should be 0', 0, FDeque.Count);
end;

procedure TThreadSafeDequeTests.TestIsEmpty;
begin
  AssertTrue('New deque should be empty', FDeque.IsEmpty);
  FDeque.PushBack(1);
  AssertFalse('Deque with item should not be empty', FDeque.IsEmpty);
  FDeque.PopBack;
  AssertTrue('Deque should be empty after removing item', FDeque.IsEmpty);
end;

procedure TThreadSafeDequeTests.TestToArray;
var
  Arr: specialize TArray<Integer>;
begin
  FDeque.PushBack(1);
  FDeque.PushBack(2);
  FDeque.PushBack(3);
  
  Arr := FDeque.ToArray;
  AssertEquals('Array length should match', 3, Length(Arr));
  AssertEquals('First element should be 1', 1, Arr[0]);
  AssertEquals('Second element should be 2', 2, Arr[1]);
  AssertEquals('Third element should be 3', 3, Arr[2]);
end;

procedure TThreadSafeDequeTests.TestContains;
begin
  FDeque.PushBack(1);
  FDeque.PushBack(2);
  
  AssertTrue('Should contain 1', FDeque.Contains(1));
  AssertTrue('Should contain 2', FDeque.Contains(2));
  AssertFalse('Should not contain 3', FDeque.Contains(3));
end;

procedure TThreadSafeDequeTests.TestPushRange;
var
  Items: array[0..2] of Integer;
begin
  Items[0] := 1;
  Items[1] := 2;
  Items[2] := 3;
  
  FDeque.PushRangeBack(Items);
  AssertEquals('Should have 3 items', 3, FDeque.Count);
  AssertEquals('First item should be 1', 1, FDeque.PopFront);
  
  FDeque.Clear;
  
  FDeque.PushRangeFront(Items);
  AssertEquals('Should have 3 items', 3, FDeque.Count);
  AssertEquals('First item should be 3', 3, FDeque.PopFront);
end;

procedure TThreadSafeDequeTests.TestMultiThreadPushPop;
const
  THREAD_COUNT = 4;
  ITEMS_PER_THREAD = 1000;
  ITERATIONS = 10;
var
  Threads: array[0..THREAD_COUNT-1] of TTestThread;
  I: Integer;
  ExpectedSum, ActualSum: Int64;
  StartValue, EndValue: Integer;
  StartTime: TDateTime;
  ThreadSum: Int64;
begin
  StartTime := Now;
  ExpectedSum := 0;
  
  // Create threads that will push and pop items
  for I := 0 to THREAD_COUNT-1 do
  begin
    StartValue := I * ITEMS_PER_THREAD;
    EndValue := StartValue + ITEMS_PER_THREAD - 1;
    
    // Calculate sum for one iteration of this thread
    ThreadSum := Int64(EndValue - StartValue + 1) * Int64(StartValue + EndValue) div 2;
    // Multiply by iterations since each thread runs multiple times
    ExpectedSum := ExpectedSum + (ThreadSum * ITERATIONS);
    
    Threads[I] := TTestThread.Create(FDeque, StartValue, ITEMS_PER_THREAD, ITERATIONS, I + 1);
    Threads[I].Start;
  end;
  
  // Wait for all threads to complete
  for I := 0 to THREAD_COUNT-1 do
  begin
    Threads[I].WaitFor;
    Threads[I].Free;
  end;
  
  // Verify results
  ActualSum := 0;
  while FDeque.Count > 0 do
    ActualSum := ActualSum + FDeque.PopFront;
    
  AssertEquals('Sum of all items should match', ExpectedSum, ActualSum);
  WriteLn(Format('Test took %d ms with %d threads doing %d iterations each', 
    [MilliSecondsBetween(Now, StartTime), THREAD_COUNT, ITERATIONS]));
end;

procedure TThreadSafeDequeTests.TestLockingMechanism;
const
  THREAD_COUNT = 4;
  ITERATIONS = 1000;
var
  Threads: array[0..THREAD_COUNT-1] of TLockTestThread;
  I: Integer;
  TotalLocks: Integer;
  StartTime: TDateTime;
begin
  StartTime := Now;
  
  // Create threads that will acquire and release locks
  for I := 0 to THREAD_COUNT-1 do
  begin
    Threads[I] := TLockTestThread.Create(FDeque, ITERATIONS);
    Threads[I].Start;
  end;
  
  // Wait for all threads to complete
  TotalLocks := 0;
  for I := 0 to THREAD_COUNT-1 do
  begin
    Threads[I].WaitFor;
    TotalLocks := TotalLocks + Threads[I].LockCount;
    Threads[I].Free;
  end;
  
  // Verify results
  AssertEquals('All lock attempts should succeed', 
    THREAD_COUNT * ITERATIONS, TotalLocks);
    
  WriteLn(Format('Lock test took %d ms, %d successful locks across %d threads', 
    [MilliSecondsBetween(Now, StartTime), TotalLocks, THREAD_COUNT]));
end;

{ TTestThread }

constructor TTestThread.Create(ADeque: TIntegerDeque; AStartValue, ACount, AIterations, ASeed: Integer);
begin
  inherited Create(True);
  FDeque := ADeque;
  FStartValue := AStartValue;
  FCount := ACount;
  FIterations := AIterations;
  FSeed := ASeed;
  FreeOnTerminate := False;
end;

function TTestThread.Random(Range: Integer): Integer;
begin
  FSeed := (FSeed * 1103515245 + 12345) and $7fffffff;
  Result := (FSeed shr 16) mod Range;
end;

procedure TTestThread.Execute;
var
  I, J: Integer;
  Value: Integer;
begin
  for J := 1 to FIterations do
  begin
    // Push items
    for I := FStartValue to FStartValue + FCount - 1 do
    begin
      FDeque.PushBack(I);
      if Random(100) < 10 then  // 10% chance to sleep
        Sleep(1 + Random(3));   // Reduced sleep time
    end;
    
    // Pop some items and push them to the front
    for I := 0 to FCount div 2 - 1 do
    begin
      if FDeque.TryPopBack(Value) then
      begin
        FDeque.PushFront(Value);
        if Random(100) < 20 then  // 20% chance to sleep
          Sleep(1 + Random(2));   // Reduced sleep time
      end;
    end;
  end;
end;

{ TLockTestThread }

constructor TLockTestThread.Create(ADeque: TIntegerDeque; AIterations: Integer);
begin
  inherited Create(True);
  FDeque := ADeque;
  FIterations := AIterations;
  FLockCount := 0;
  FreeOnTerminate := False;
end;

procedure TLockTestThread.Execute;
var
  I: Integer;
  LockToken: ILockToken;
begin
  for I := 1 to FIterations do
  begin
    try
      // Get lock token
      LockToken := FDeque.Lock;
      
      // Simulate some work
      Sleep(Random(2));
      
      // Lock will be automatically released when LockToken goes out of scope
      Inc(FLockCount);
    except
      // Count failed lock attempts (should never happen)
      on E: Exception do
        WriteLn('Lock failed: ', E.Message);
    end;
  end;
end;

initialization
  RegisterTest(TThreadSafeDequeTests);
end.
