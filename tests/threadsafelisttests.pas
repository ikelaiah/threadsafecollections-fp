unit ThreadSafeListTests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  ThreadSafeCollections.List, DateUtils, ThreadSafeCollections.Interfaces;

type

  // Test thread for stress testing
  TTestThread = class(TThread)
  private
    FList: specialize TThreadSafeList<Integer>;
    FStartValue: Integer;
    FOperations: Integer;
  public
    constructor Create(AList: specialize TThreadSafeList<Integer>;
      AStartValue, AOperations: Integer);
    procedure Execute; override;
  end;

  // Add this type after other test thread types
  TLockTestThread = class(TThread)
  private
    FList: specialize TThreadSafeList<Integer>;
    FLockCount: Integer;
    FIterations: Integer;
  public
    constructor Create(AList: specialize TThreadSafeList<Integer>; AIterations: Integer);
    procedure Execute; override;
    property LockCount: Integer read FLockCount;
  end;

  TThreadSafeListTest = class(TTestCase)
  private
    FIntList: specialize TThreadSafeList<Integer>;
    FStrList: specialize TThreadSafeList<string>;
    FBoolList: specialize TThreadSafeList<Boolean>;
    FRealList: specialize TThreadSafeList<Real>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Basic functionality tests
    procedure TestCreation;
    procedure TestCreationWithNilComparer;
    procedure TestAddInteger;
    procedure TestAddString;
    procedure TestDelete;
    procedure TestIndexOf;
    procedure TestFirstLast;
    procedure TestReplace;
    procedure TestSort;
    procedure TestSortDescending;
    procedure TestIsSorted;

    // Edge cases
    procedure TestEmptyList;
    procedure TestSingleElement;
    procedure TestDuplicateElements;
    procedure TestBoundaries;

    // Specific type tests
    procedure TestBooleanList;
    procedure TestRealList;

    // Stress tests
    procedure TestMultiThreadAccess;
    procedure TestLargeDataSet;
    procedure TestRandomOperations;
    procedure TestLargeDataSetSortingPerformance;

    // Iterator tests
    procedure TestIterator;
    procedure TestIteratorThreadSafety;
    procedure TestIteratorExceptionSafety;

    // Add this test method to TThreadSafeListTest published section
    procedure TestLockingMechanism;
  end;

implementation

const
  STRESS_THREAD_COUNT = 10;
  OPERATIONS_PER_THREAD = 1000;
  LARGE_DATASET_SIZE = 100000;

{ TTestThread }

constructor TTestThread.Create(AList: specialize TThreadSafeList<Integer>;
  AStartValue, AOperations: Integer);
begin
  inherited Create(True);
  FList := AList;
  FStartValue := AStartValue;
  FOperations := AOperations;
  FreeOnTerminate := False;
end;

procedure TTestThread.Execute;
var
  I: Integer;
  Operation: Integer;
begin
  for I := 0 to FOperations - 1 do
  begin
    Operation := Random(3); // 0: Add, 1: Delete, 2: IndexOf
    case Operation of
      0: FList.Add(FStartValue + I);
      1: if FList.Count > 0 then
         try
           FList.Delete(Random(FList.Count));
         except
           // Ignore concurrent deletion errors
         end;
      2: FList.IndexOf(FStartValue + Random(I + 1));
    end;
  end;
end;

{ TLockTestThread }

constructor TLockTestThread.Create(AList: specialize TThreadSafeList<Integer>; AIterations: Integer);
begin
  inherited Create(True);
  FList := AList;
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
      LockToken := FList.Lock;
      
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

{ TThreadSafeListTest }

procedure TThreadSafeListTest.SetUp;
begin
  FIntList := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  FStrList := specialize TThreadSafeList<string>.Create(@StringComparer);
  FBoolList := specialize TThreadSafeList<Boolean>.Create(@BooleanComparer);
  FRealList := specialize TThreadSafeList<Real>.Create(@RealComparer);
end;

procedure TThreadSafeListTest.TearDown;
begin
  FIntList.Free;
  FStrList.Free;
  FBoolList.Free;
  FRealList.Free;
end;

procedure TThreadSafeListTest.TestCreation;
begin
  AssertNotNull('Integer list should be created', FIntList);
  AssertEquals('New list should be empty', 0, FIntList.Count);
  AssertTrue('New list should be sorted', FIntList.IsSorted);
end;

procedure TThreadSafeListTest.TestCreationWithNilComparer;
var
  TempList: specialize TThreadSafeList<Integer>;
begin
  try
    TempList := specialize TThreadSafeList<Integer>.Create(nil);
    Fail('Should raise exception with nil comparer');
  except
    on E: Exception do
      AssertTrue('Correct exception raised', True);
  end;
end;

procedure TThreadSafeListTest.TestAddInteger;
begin
  FIntList.Add(5);
  FIntList.Add(3);
  FIntList.Add(7);

  AssertEquals('Count should be 3', 3, FIntList.Count);
  AssertEquals('First element should be 5', 5, FIntList[0]);
  AssertFalse('List should not be sorted', FIntList.IsSorted);
end;

procedure TThreadSafeListTest.TestAddString;
begin
  FStrList.Add('Hello');
  FStrList.Add('World');

  AssertEquals('Count should be 2', 2, FStrList.Count);
  AssertEquals('First element should be Hello', 'Hello', FStrList[0]);
end;

procedure TThreadSafeListTest.TestDelete;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  FIntList.Delete(1);
  AssertEquals('Count should be 2', 2, FIntList.Count);
  AssertEquals('Second element should be 3', 3, FIntList[1]);

  try
    FIntList.Delete(5);
    Fail('Should raise exception for invalid index');
  except
    on E: Exception do
      AssertTrue('Correct exception raised', True);
  end;
end;

procedure TThreadSafeListTest.TestMultiThreadAccess;
var
  Threads: array[0..STRESS_THREAD_COUNT-1] of TTestThread;
  I: Integer;
  StartTick, EndTick: QWord;
begin
  StartTick := GetTickCount64;

  // Create and start threads
  for I := 0 to STRESS_THREAD_COUNT-1 do
  begin
    Threads[I] := TTestThread.Create(FIntList, I * OPERATIONS_PER_THREAD,
      OPERATIONS_PER_THREAD);
    Threads[I].Start;
  end;

  // Wait for all threads to complete
  for I := 0 to STRESS_THREAD_COUNT-1 do
  begin
    Threads[I].WaitFor;
    Threads[I].Free;
  end;

  EndTick := GetTickCount64;
  WriteLn(Format('Multi-thread operations took: %d ms', [EndTick - StartTick]));

  // Verify list integrity
  try
    FIntList.Sort;
    AssertTrue('List should be sortable after stress test', FIntList.IsSorted);
  except
    Fail('List integrity compromised during stress test');
  end;
end;

procedure TThreadSafeListTest.TestLargeDataSet;
var
  I: Integer;
  StartTick, EndTick: QWord;
begin
  StartTick := GetTickCount64;

  // Add large number of items
  for I := 0 to LARGE_DATASET_SIZE - 1 do
    FIntList.Add(Random(LARGE_DATASET_SIZE));

  // Sort and verify
  FIntList.Sort;
  AssertTrue('Large dataset should be sortable', FIntList.IsSorted);

  // Verify order
  for I := 1 to FIntList.Count - 1 do
    AssertTrue('Items should be in order',
      FIntList[I-1] <= FIntList[I]);

  EndTick := GetTickCount64;
  WriteLn(Format('Large dataset operations took: %d ms', [EndTick - StartTick]));

  // Performance check (adjust threshold as needed)
  AssertTrue('Large dataset operations should complete within reasonable time',
    EndTick - StartTick < 5000);
end;

procedure TThreadSafeListTest.TestRandomOperations;
var
  I, Idx: Integer;
  Operations: Integer;
begin
  Operations := 10000;

  for I := 1 to Operations do
  begin
    case Random(4) of
      0: FIntList.Add(Random(1000));
      1: if FIntList.Count > 0 then
         begin
           Idx := Random(FIntList.Count);
           FIntList.Replace(Idx, Random(1000));
         end;
      2: if FIntList.Count > 0 then
         begin
           Idx := Random(FIntList.Count);
           FIntList.Delete(Idx);
         end;
      3: FIntList.IndexOf(Random(1000));
    end;
  end;

  // Verify list integrity
  AssertTrue('List should survive random operations', True);
end;

procedure TThreadSafeListTest.TestIndexOf;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  AssertEquals('Should find existing element', 1, FIntList.IndexOf(2));
  AssertEquals('Should return -1 for non-existing element', -1, FIntList.IndexOf(5));
end;

procedure TThreadSafeListTest.TestFirstLast;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  AssertEquals('First should be 1', 1, FIntList.First);
  AssertEquals('Last should be 3', 3, FIntList.Last);

  try
    FIntList := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
    FIntList.First;
    Fail('Should raise exception on empty list First');
  except
    on E: Exception do
      AssertTrue('Correct exception raised', True);
  end;

  try
    FIntList.Last;
    Fail('Should raise exception on empty list Last');
  except
    on E: Exception do
      AssertTrue('Correct exception raised', True);
  end;
end;

procedure TThreadSafeListTest.TestReplace;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  FIntList.Replace(1, 5);
  AssertEquals('Element should be replaced', 5, FIntList[1]);

  try
    FIntList.Replace(10, 5);
    Fail('Should raise exception for invalid index');
  except
    on E: Exception do
      AssertTrue('Correct exception raised', True);
  end;
end;

procedure TThreadSafeListTest.TestSort;
begin
  FIntList.Add(3);
  FIntList.Add(1);
  FIntList.Add(2);

  FIntList.Sort;
  AssertEquals('First element after sort', 1, FIntList[0]);
  AssertEquals('Second element after sort', 2, FIntList[1]);
  AssertEquals('Third element after sort', 3, FIntList[2]);
  AssertTrue('List should be sorted', FIntList.IsSorted);
end;

procedure TThreadSafeListTest.TestIsSorted;
begin
  AssertTrue('Empty list should be sorted', FIntList.IsSorted);

  FIntList.Add(1);
  AssertTrue('Single element list should be sorted', FIntList.IsSorted);

  FIntList.Add(2);
  AssertTrue('Ascending elements should be sorted', FIntList.IsSorted);

  FIntList.Add(1);
  AssertFalse('List should not be sorted after adding smaller element', FIntList.IsSorted);
end;

procedure TThreadSafeListTest.TestEmptyList;
begin
  AssertEquals('Empty list count should be 0', 0, FIntList.Count);
  AssertTrue('Empty list should be sorted', FIntList.IsSorted);
  AssertEquals('Find in empty list should return -1', -1, FIntList.IndexOf(1));
end;

procedure TThreadSafeListTest.TestSingleElement;
begin
  FIntList.Add(1);
  AssertEquals('Single element list count should be 1', 1, FIntList.Count);
  AssertTrue('Single element list should be sorted', FIntList.IsSorted);
  AssertEquals('First and last should be same', FIntList.First, FIntList.Last);
end;

procedure TThreadSafeListTest.TestDuplicateElements;
begin
  FIntList.Add(1);
  FIntList.Add(1);
  FIntList.Add(1);

  AssertEquals('Count should be 3', 3, FIntList.Count);
  AssertTrue('List with duplicates should be sorted', FIntList.IsSorted);
  AssertEquals('Should find first occurrence', 0, FIntList.IndexOf(1));
end;

procedure TThreadSafeListTest.TestBoundaries;
begin
  try
    FIntList.Delete(-1);
    Fail('Should raise exception for negative index');
  except
    on E: Exception do
      AssertTrue('Correct exception raised', True);
  end;

  try
    FIntList.GetItem(0);
    Fail('Should raise exception for empty list access');
  except
    on E: Exception do
      AssertTrue('Correct exception raised', True);
  end;
end;

procedure TThreadSafeListTest.TestBooleanList;
begin
  FBoolList.Add(True);
  FBoolList.Add(False);
  FBoolList.Add(True);

  FBoolList.Sort;
  AssertEquals('First element should be False', False, FBoolList[0]);
  AssertEquals('Last element should be True', True, FBoolList[2]);
end;

procedure TThreadSafeListTest.TestRealList;
begin
  FRealList.Add(3.14);
  FRealList.Add(1.41);
  FRealList.Add(2.71);

  FRealList.Sort;
  AssertTrue('First element should be smallest', FRealList[0] < FRealList[1]);
  AssertTrue('Last element should be largest', FRealList[1] < FRealList[2]);
end;

procedure TThreadSafeListTest.TestLargeDataSetSortingPerformance;
const
  SORT_SIZE = 100000;
var
  I: Integer;
  StartTick, EndTick: QWord;
  TempStr: string;
  CharSet: string;
  IsSortedCorrectly: Boolean;
begin
  CharSet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';

  // Test Integer sorting
  WriteLn('Integer Test:');
  for I := 0 to SORT_SIZE - 1 do
    FIntList.Add(SORT_SIZE - I);  // Add in reverse order to ensure we need sorting
    
  // Print first few elements before sort
  WriteLn('Before sort (first 5): ');
  for I := 0 to 4 do
    Write(FIntList[I], ' ');
  WriteLn;
    
  StartTick := GetTickCount64;
  FIntList.Sort;
  EndTick := GetTickCount64;
  WriteLn(Format('Sorting %d integers took: %d ms', [SORT_SIZE, EndTick - StartTick]));
  
  // Print first few elements after sort
  WriteLn('After sort (first 5): ');
  for I := 0 to 4 do
    Write(FIntList[I], ' ');
  WriteLn;
    
  // Verify sorting thoroughly
  IsSortedCorrectly := True;
  for I := 1 to FIntList.Count - 1 do
    if FIntList[I-1] > FIntList[I] then
    begin
      WriteLn(Format('Sort error at position %d: %d > %d', 
        [I, FIntList[I-1], FIntList[I]]));
      IsSortedCorrectly := False;
      Break;
    end;
  
  AssertTrue('Integer list should be properly sorted', IsSortedCorrectly);
  WriteLn('Integer sort verification: ', IsSortedCorrectly);
  WriteLn;

  FIntList.Free;
  FIntList := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);

  // Test String sorting
  WriteLn('String Test:');
  for I := 0 to SORT_SIZE - 1 do
  begin
    SetLength(TempStr, 10);
    TempStr := '';
    while Length(TempStr) < 10 do
      TempStr := TempStr + CharSet[Random(Length(CharSet)) + 1];
    FStrList.Add(TempStr);
  end;

  // Print first few strings before sort
  WriteLn('Before sort (first 3): ');
  for I := 0 to 2 do
    WriteLn(FStrList[I]);
    
  StartTick := GetTickCount64;
  FStrList.Sort;
  EndTick := GetTickCount64;
  WriteLn(Format('Sorting %d strings (10 chars each) took: %d ms', 
    [SORT_SIZE, EndTick - StartTick]));

  // Print first few strings after sort
  WriteLn('After sort (first 3): ');
  for I := 0 to 2 do
    WriteLn(FStrList[I]);
    
  // Verify string sorting
  IsSortedCorrectly := True;
  for I := 1 to FStrList.Count - 1 do
    if CompareStr(FStrList[I-1], FStrList[I]) > 0 then
    begin
      WriteLn(Format('Sort error at position %d: %s > %s', 
        [I, FStrList[I-1], FStrList[I]]));
      IsSortedCorrectly := False;
      Break;
    end;
    
  AssertTrue('String list should be properly sorted', IsSortedCorrectly);
  WriteLn('String sort verification: ', IsSortedCorrectly);
  WriteLn;

  FStrList.Free;
  FStrList := specialize TThreadSafeList<string>.Create(@StringComparer);

  // Test Real sorting
  WriteLn('Real Test:');
  for I := 0 to SORT_SIZE - 1 do
    FRealList.Add((SORT_SIZE - I) * 1.5);  // Add in reverse order with decimals
    
  // Print first few elements before sort
  WriteLn('Before sort (first 5): ');
  for I := 0 to 4 do
    Write(Format('%.2f ', [FRealList[I]]));
  WriteLn;
    
  StartTick := GetTickCount64;
  FRealList.Sort;
  EndTick := GetTickCount64;
  WriteLn(Format('Sorting %d reals took: %d ms', [SORT_SIZE, EndTick - StartTick]));
  
  // Print first few elements after sort
  WriteLn('After sort (first 5): ');
  for I := 0 to 4 do
    Write(Format('%.2f ', [FRealList[I]]));
  WriteLn;
    
  // Verify real sorting
  IsSortedCorrectly := True;
  for I := 1 to FRealList.Count - 1 do
    if FRealList[I-1] > FRealList[I] then
    begin
      WriteLn(Format('Sort error at position %d: %.2f > %.2f', 
        [I, FRealList[I-1], FRealList[I]]));
      IsSortedCorrectly := False;
      Break;
    end;
    
  AssertTrue('Real list should be properly sorted', IsSortedCorrectly);
  WriteLn('Real sort verification: ', IsSortedCorrectly);
  WriteLn;

  FRealList.Free;
  FRealList := specialize TThreadSafeList<Real>.Create(@RealComparer);
end;

procedure TThreadSafeListTest.TestSortDescending;
begin
  // Test Integer descending sort
  FIntList.Add(1);
  FIntList.Add(3);
  FIntList.Add(2);

  FIntList.Sort(False);  // Sort descending
  AssertEquals('First element after descending sort', 3, FIntList[0]);
  AssertEquals('Second element after descending sort', 2, FIntList[1]);
  AssertEquals('Third element after descending sort', 1, FIntList[2]);
  AssertTrue('List should be sorted', FIntList.IsSorted);

  // Test String descending sort
  FStrList.Add('Apple');
  FStrList.Add('Cherry');
  FStrList.Add('Banana');

  FStrList.Sort(False);  // Sort descending
  AssertEquals('First string after descending sort', 'Cherry', FStrList[0]);
  AssertEquals('Second string after descending sort', 'Banana', FStrList[1]);
  AssertEquals('Third string after descending sort', 'Apple', FStrList[2]);
  AssertTrue('String list should be sorted', FStrList.IsSorted);

  // Test Real descending sort
  FRealList.Add(1.1);
  FRealList.Add(3.3);
  FRealList.Add(2.2);

  FRealList.Sort(False);  // Sort descending
  AssertTrue('First element should be largest', FRealList[0] > FRealList[1]);
  AssertTrue('Last element should be smallest', FRealList[1] > FRealList[2]);
  AssertTrue('Real list should be sorted', FRealList.IsSorted);
end;

procedure TThreadSafeListTest.TestIterator;
var
  ExpectedSum, ActualSum: Integer;
  Value: Integer;
  I: Integer;
begin
  // Add some test data
  for I := 1 to 5 do
    FIntList.Add(I);

  // Calculate expected sum
  ExpectedSum := 15; // 1 + 2 + 3 + 4 + 5

  // Test iteration using for-in loop
  ActualSum := 0;
  for Value in FIntList do
    ActualSum := ActualSum + Value;

  AssertEquals('Sum through iterator should match expected', ExpectedSum, ActualSum);

  // Test iterator with empty list
  FIntList.Free;
  FIntList := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  
  ActualSum := 0;
  for Value in FIntList do
    ActualSum := ActualSum + Value;
  
  AssertEquals('Empty list iteration should result in zero sum', 0, ActualSum);

  // Test iterator with single element
  FIntList.Add(42);
  ActualSum := 0;
  for Value in FIntList do
    ActualSum := ActualSum + Value;
    
  AssertEquals('Single element iteration should work', 42, ActualSum);

  // Test iterator after modifications
  FIntList.Add(10);
  FIntList.Add(20);
  FIntList.Delete(0); // Remove 42
  
  ExpectedSum := 30; // 10 + 20
  ActualSum := 0;
  for Value in FIntList do
    ActualSum := ActualSum + Value;
    
  AssertEquals('Iterator should work after modifications', ExpectedSum, ActualSum);
end;

procedure TThreadSafeListTest.TestIteratorThreadSafety;
var
  SecondList: specialize TThreadSafeList<Integer>;
  Value: Integer;
  I: Integer;
begin
  SecondList := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    // Add test data to both lists
    for I := 1 to 1000 do
    begin
      FIntList.Add(I);
      SecondList.Add(I * 2);
    end;

    // Test nested iteration (should work because each gets its own lock)
    try
      for Value in FIntList do
        for I in SecondList do
          if I mod 100 = 0 then
            AssertTrue('Nested iteration should work', True);
            
      AssertTrue('Nested iteration completed successfully', True);
    except
      on E: Exception do
        Fail('Nested iteration should not raise exception: ' + E.Message);
    end;

    // Test iteration with concurrent modifications
    try
      for Value in FIntList do
      begin
        if Value mod 100 = 0 then
        begin
          SecondList.Add(Value * 10);
          SecondList.Delete(0);
        end;
      end;
      AssertTrue('Iteration with concurrent modifications succeeded', True);
    except
      on E: Exception do
        Fail('Iteration with concurrent modifications failed: ' + E.Message);
    end;
  finally
    SecondList.Free;
  end;
end;

procedure TThreadSafeListTest.TestIteratorExceptionSafety;
var
  Value: Integer;
  ExceptionRaised: Boolean;
begin
  // Add some test data
  for Value := 1 to 5 do
    FIntList.Add(Value);

  // Test that iterator properly releases lock even if loop breaks
  ExceptionRaised := False;
  try
    for Value in FIntList do
    begin
      if Value = 3 then
        raise Exception.Create('Test exception');
    end;
  except
    on E: Exception do
      ExceptionRaised := True;
  end;

  AssertTrue('Exception should have been raised', ExceptionRaised);
  
  // Verify list is still accessible after exception
  try
    FIntList.Add(6);
    AssertEquals('Should be able to add after iterator exception', 6, FIntList.Count);
  except
    on E: Exception do
      Fail('List should be accessible after iterator exception: ' + E.Message);
  end;
end;

procedure TThreadSafeListTest.TestLockingMechanism;
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
    Threads[I] := TLockTestThread.Create(FIntList, ITERATIONS);
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

initialization

  RegisterTest(TThreadSafeListTest);
end.

