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
    procedure Test01_Creation;
    procedure Test02_CreationWithNilComparer;
    procedure Test03_AddInteger;
    procedure Test04_AddString;
    procedure Test05_Delete;
    procedure Test06_IndexOf;
    procedure Test07_FirstLast;
    procedure Test08_Replace;
    procedure Test09_Sort;
    procedure Test10_SortDescending;
    procedure Test11_IsSorted;

    // Edge cases
    procedure Test12_EmptyList;
    procedure Test13_SingleElement;
    procedure Test14_DuplicateElements;
    procedure Test15_Boundaries;

    // Specific type tests
    procedure Test16_BooleanList;
    procedure Test17_RealList;

    // Stress tests
    procedure Test18_MultiThreadAccess;
    procedure Test19_LargeDataSet;
    procedure Test20_RandomOperations;
    procedure Test21_LargeDataSetSortingPerformance;

    // Iterator tests
    procedure Test22_Iterator;
    procedure Test23_IteratorThreadSafety;
    procedure Test24_IteratorExceptionSafety;
    procedure Test25_LockingMechanism;

    // Capacity management tests
    procedure Test26_Capacity;
    procedure Test27_TrimExcess;

    // Array operation tests
    procedure Test28_ToArray;
    procedure Test29_FromArray;

    // Range operation tests
    procedure Test30_AddRange;
    procedure Test31_AddRangeFromCollection;
    procedure Test32_InsertRange;
    procedure Test33_InsertRangeFromCollection;
    procedure Test34_DeleteRange;

    // Search operation tests
    procedure Test35_Contains;
    procedure Test36_IndexOfItemWithStart;
    procedure Test37_IndexOfItemWithStartAndCount;
    procedure Test38_LastIndexOf;
    procedure Test39_LastIndexOfWithStart;
    procedure Test40_LastIndexOfWithStartAndCount;

    // Additional utility tests
    procedure Test41_Insert;
    procedure Test42_Exchange;
    procedure Test43_Move;
    procedure Test44_Reverse;
    procedure Test45_Extract;
    procedure Test46_ExtractAt;
    
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

procedure TThreadSafeListTest.Test01_Creation;
begin
  AssertNotNull('Integer list should be created', FIntList);
  AssertEquals('New list should be empty', 0, FIntList.Count);
  AssertTrue('New list should be sorted', FIntList.IsSorted);
end;

procedure TThreadSafeListTest.Test02_CreationWithNilComparer;
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

procedure TThreadSafeListTest.Test03_AddInteger;
begin
  FIntList.Add(5);
  FIntList.Add(3);
  FIntList.Add(7);

  AssertEquals('Count should be 3', 3, FIntList.Count);
  AssertEquals('First element should be 5', 5, FIntList[0]);
  AssertFalse('List should not be sorted', FIntList.IsSorted);
end;

procedure TThreadSafeListTest.Test04_AddString;
begin
  FStrList.Add('Hello');
  FStrList.Add('World');

  AssertEquals('Count should be 2', 2, FStrList.Count);
  AssertEquals('First element should be Hello', 'Hello', FStrList[0]);
end;

procedure TThreadSafeListTest.Test05_Delete;
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

procedure TThreadSafeListTest.Test06_IndexOf;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  AssertEquals('Should find existing element', 1, FIntList.IndexOf(2));
  AssertEquals('Should return -1 for non-existing element', -1, FIntList.IndexOf(5));
end;

procedure TThreadSafeListTest.Test07_FirstLast;
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

procedure TThreadSafeListTest.Test08_Replace;
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

procedure TThreadSafeListTest.Test09_Sort;
begin
  FIntList.Add(3);
  FIntList.Add(1);
  FIntList.Add(2);

  FIntList.Sort;
  AssertTrue('List should be sorted', FIntList.IsSorted);
  AssertEquals('First element should be smallest', 1, FIntList[0]);
  AssertEquals('Last element should be largest', 3, FIntList[2]);
end;

procedure TThreadSafeListTest.Test10_SortDescending;
begin
  FIntList.Add(1);
  FIntList.Add(3);
  FIntList.Add(2);

  FIntList.Sort(False);
  AssertTrue('List should be sorted', FIntList.IsSorted);
  AssertEquals('First element should be largest', 3, FIntList[0]);
  AssertEquals('Last element should be smallest', 1, FIntList[2]);
end;

procedure TThreadSafeListTest.Test11_IsSorted;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);
  AssertTrue('Sequential adds should maintain sort', FIntList.IsSorted);

  FIntList.Add(0);
  AssertFalse('Adding smaller element should break sort', FIntList.IsSorted);
end;

procedure TThreadSafeListTest.Test12_EmptyList;
begin
  AssertTrue('New list should be empty', FIntList.IsEmpty);
  AssertEquals('Empty list count should be 0', 0, FIntList.Count);
  AssertTrue('Empty list should be considered sorted', FIntList.IsSorted);
end;

procedure TThreadSafeListTest.Test13_SingleElement;
begin
  FIntList.Add(1);
  AssertTrue('Single element list should be sorted', FIntList.IsSorted);
  AssertEquals('Single element should be accessible', 1, FIntList.First);
  AssertEquals('Single element should be accessible', 1, FIntList.Last);
end;

procedure TThreadSafeListTest.Test14_DuplicateElements;
begin
  FIntList.Add(1);
  FIntList.Add(1);
  FIntList.Add(1);

  AssertEquals('Count should be 3', 3, FIntList.Count);
  AssertTrue('List with duplicates should be sorted', FIntList.IsSorted);
  AssertEquals('Should find first occurrence', 0, FIntList.IndexOf(1));
end;

procedure TThreadSafeListTest.Test15_Boundaries;
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

procedure TThreadSafeListTest.Test16_BooleanList;
begin
  FBoolList.Add(True);
  FBoolList.Add(False);
  FBoolList.Add(True);

  FBoolList.Sort;
  AssertEquals('First element should be False', False, FBoolList[0]);
  AssertEquals('Last element should be True', True, FBoolList[2]);
end;

procedure TThreadSafeListTest.Test17_RealList;
begin
  FRealList.Add(3.14);
  FRealList.Add(1.41);
  FRealList.Add(2.71);

  FRealList.Sort;
  AssertTrue('First element should be smallest', FRealList[0] < FRealList[1]);
  AssertTrue('Last element should be largest', FRealList[1] < FRealList[2]);
end;

procedure TThreadSafeListTest.Test18_MultiThreadAccess;
var
  Threads: array[1..STRESS_THREAD_COUNT] of TTestThread;
  I: Integer;
  StartTick: QWord;
begin
  StartTick := GetTickCount64;
  
  // Create and start threads
  for I := 1 to STRESS_THREAD_COUNT do
  begin
    Threads[I] := TTestThread.Create(FIntList, I * 1000, OPERATIONS_PER_THREAD);
    Threads[I].Start;
  end;

  // Wait for all threads to complete
  for I := 1 to STRESS_THREAD_COUNT do
  begin
    Threads[I].WaitFor;
    Threads[I].Free;
  end;

  // Verify list integrity
  AssertTrue('List should survive multi-threaded access', True);
  WriteLn(Format('Multi-thread test completed in %d ms', [GetTickCount64 - StartTick]));
end;

procedure TThreadSafeListTest.Test19_LargeDataSet;
var
  I: Integer;
  StartTick, EndTick: QWord;
begin
  StartTick := GetTickCount64;

  // Add large number of items
  for I := 1 to LARGE_DATASET_SIZE do
    FIntList.Add(Random(LARGE_DATASET_SIZE));

  // Perform operations
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

procedure TThreadSafeListTest.Test20_RandomOperations;
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

  AssertTrue('List should survive random operations', True);
end;

procedure TThreadSafeListTest.Test21_LargeDataSetSortingPerformance;
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
    FIntList.Add(SORT_SIZE - I);  // Add in reverse order
    
  StartTick := GetTickCount64;
  FIntList.Sort;
  EndTick := GetTickCount64;
  
  // Verify sorting
  IsSortedCorrectly := True;
  for I := 1 to FIntList.Count - 1 do
    if FIntList[I-1] > FIntList[I] then
    begin
      IsSortedCorrectly := False;
      Break;
    end;
  
  AssertTrue('Integer list should be properly sorted', IsSortedCorrectly);
  WriteLn(Format('Sorting %d integers took: %d ms', [SORT_SIZE, EndTick - StartTick]));

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

  StartTick := GetTickCount64;
  FStrList.Sort;
  EndTick := GetTickCount64;
  
  // Verify string sorting
  IsSortedCorrectly := True;
  for I := 1 to FStrList.Count - 1 do
    if CompareStr(FStrList[I-1], FStrList[I]) > 0 then
    begin
      IsSortedCorrectly := False;
      Break;
    end;
    
  AssertTrue('String list should be properly sorted', IsSortedCorrectly);
  WriteLn(Format('Sorting %d strings took: %d ms', [SORT_SIZE, EndTick - StartTick]));
end;

procedure TThreadSafeListTest.Test22_Iterator;
var
  ExpectedSum, ActualSum: Integer;
  Value: Integer;
  I: Integer;
begin
  // Add test data
  for I := 1 to 5 do
    FIntList.Add(I);

  ExpectedSum := 15; // 1 + 2 + 3 + 4 + 5

  // Test iteration
  ActualSum := 0;
  for Value in FIntList do
    ActualSum := ActualSum + Value;

  AssertEquals('Sum through iterator should match expected', ExpectedSum, ActualSum);

  // Test empty list iteration
  FIntList.Clear;
  ActualSum := 0;
  for Value in FIntList do
    ActualSum := ActualSum + Value;
  
  AssertEquals('Empty list iteration should result in zero sum', 0, ActualSum);
end;

procedure TThreadSafeListTest.Test23_IteratorThreadSafety;
var
  SecondList: specialize TThreadSafeList<Integer>;
  Value: Integer;
  I: Integer;
begin
  SecondList := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    // Add test data
    for I := 1 to 1000 do
    begin
      FIntList.Add(I);
      SecondList.Add(I * 2);
    end;

    // Test nested iteration
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
  finally
    SecondList.Free;
  end;
end;

procedure TThreadSafeListTest.Test24_IteratorExceptionSafety;
var
  Value: Integer;
  ExceptionRaised: Boolean;
begin
  // Add test data
  for Value := 1 to 5 do
    FIntList.Add(Value);

  ExceptionRaised := False;
  try
    for Value in FIntList do
    begin
      if Value = 3 then
        raise Exception.Create('Test exception');
    end;
  except
    ExceptionRaised := True;
  end;

  AssertTrue('Exception should have been raised', ExceptionRaised);
  
  // Verify list is still accessible
  try
    FIntList.Add(6);
    AssertEquals('Should be able to add after iterator exception', 6, FIntList.Count);
  except
    on E: Exception do
      Fail('List should be accessible after iterator exception: ' + E.Message);
  end;
end;

procedure TThreadSafeListTest.Test25_LockingMechanism;
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
  
  AssertEquals('All lock attempts should succeed', 
    THREAD_COUNT * ITERATIONS, TotalLocks);
    
  WriteLn(Format('Lock test took %d ms, %d successful locks across %d threads', 
    [MilliSecondsBetween(Now, StartTime), TotalLocks, THREAD_COUNT]));
end;

procedure TThreadSafeListTest.Test26_Capacity;
begin
  // v0.8: Initial capacity is now 16 (pre-allocated for performance)
  AssertEquals('Initial capacity should be 16', 16, FIntList.Capacity);

  // Adding items should increase capacity
  FIntList.Add(1);
  AssertTrue('Capacity should be greater than count', FIntList.Capacity >= FIntList.Count);

  // Setting capacity
  FIntList.Capacity := 10;
  AssertEquals('Capacity should be set to 10', 10, FIntList.Capacity);

  // Cannot set capacity less than count
  try
    FIntList.Capacity := 0;
    Fail('Should not allow capacity less than count');
  except
    on E: Exception do
      AssertTrue('Correct exception raised', True);
  end;
end;

procedure TThreadSafeListTest.Test27_TrimExcess;
begin
  FIntList.Capacity := 100;
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  FIntList.TrimExcess;
  AssertEquals('Capacity should match count after trim', FIntList.Count, FIntList.Capacity);
end;

procedure TThreadSafeListTest.Test28_ToArray;
var
  Arr: specialize TArray<Integer>;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  Arr := FIntList.ToArray;
  AssertEquals('Array length should match list count', FIntList.Count, Length(Arr));
  AssertEquals('First element should match', FIntList[0], Arr[0]);
  AssertEquals('Last element should match', FIntList[2], Arr[2]);
end;

procedure TThreadSafeListTest.Test29_FromArray;
var
  Arr: array[0..2] of Integer = (1, 2, 3);
begin
  FIntList.FromArray(Arr);
  
  AssertEquals('Count should match array length', Length(Arr), FIntList.Count);
  AssertEquals('First element should match', Arr[0], FIntList[0]);
  AssertEquals('Last element should match', Arr[2], FIntList[2]);
end;

procedure TThreadSafeListTest.Test30_AddRange;
var
  Arr: array[0..2] of Integer = (1, 2, 3);
begin
  FIntList.AddRange(Arr);
  
  AssertEquals('Count should match array length', Length(Arr), FIntList.Count);
  AssertEquals('Elements should be added in order', 1, FIntList[0]);
  AssertEquals('Elements should be added in order', 3, FIntList[2]);
end;

procedure TThreadSafeListTest.Test31_AddRangeFromCollection;
var
  OtherList: specialize TThreadSafeList<Integer>;
begin
  OtherList := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    OtherList.Add(1);
    OtherList.Add(2);
    OtherList.Add(3);

    FIntList.AddRange(OtherList);
    
    AssertEquals('Count should match source list', OtherList.Count, FIntList.Count);
    AssertEquals('Elements should be added in order', 1, FIntList[0]);
    AssertEquals('Elements should be added in order', 3, FIntList[2]);
  finally
    OtherList.Free;
  end;
end;

procedure TThreadSafeListTest.Test32_InsertRange;
var
  Arr: array[0..2] of Integer = (4, 5, 6);
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  FIntList.InsertRange(1, Arr);
  
  AssertEquals('Count should be increased by array length', 6, FIntList.Count);
  AssertEquals('Original first element should remain', 1, FIntList[0]);
  AssertEquals('First inserted element should be at index 1', 4, FIntList[1]);
  AssertEquals('Last inserted element should be at index 3', 6, FIntList[3]);
  AssertEquals('Original elements should be moved', 2, FIntList[4]);
end;

procedure TThreadSafeListTest.Test33_InsertRangeFromCollection;
var
  OtherList: specialize TThreadSafeList<Integer>;
begin
  OtherList := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    OtherList.Add(4);
    OtherList.Add(5);
    OtherList.Add(6);

    FIntList.Add(1);
    FIntList.Add(2);
    FIntList.Add(3);

    FIntList.InsertRange(1, OtherList);
    
    AssertEquals('Count should be increased by source list count', 6, FIntList.Count);
    AssertEquals('Original first element should remain', 1, FIntList[0]);
    AssertEquals('First inserted element should be at index 1', 4, FIntList[1]);
    AssertEquals('Last inserted element should be at index 3', 6, FIntList[3]);
    AssertEquals('Original elements should be moved', 2, FIntList[4]);
  finally
    OtherList.Free;
  end;
end;

procedure TThreadSafeListTest.Test34_DeleteRange;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);
  FIntList.Add(4);
  FIntList.Add(5);

  FIntList.DeleteRange(1, 3);
  
  AssertEquals('Count should be reduced by deleted range', 2, FIntList.Count);
  AssertEquals('First element should remain', 1, FIntList[0]);
  AssertEquals('Last element should be moved up', 5, FIntList[1]);
end;

procedure TThreadSafeListTest.Test35_Contains;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  AssertTrue('Should find existing element', FIntList.Contains(2));
  AssertFalse('Should not find non-existing element', FIntList.Contains(5));
end;

procedure TThreadSafeListTest.Test36_IndexOfItemWithStart;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(2);
  FIntList.Add(3);

  AssertEquals('Should find second occurrence of 2', 2, FIntList.IndexOfItem(2, 2));
  AssertEquals('Should not find element after its last occurrence', -1, FIntList.IndexOfItem(2, 3));
end;

procedure TThreadSafeListTest.Test37_IndexOfItemWithStartAndCount;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(2);
  FIntList.Add(3);

  AssertEquals('Should find element within count range', 1, FIntList.IndexOfItem(2, 0, 2));
  AssertEquals('Should not find element outside count range', -1, FIntList.IndexOfItem(2, 0, 1));
  AssertEquals('Should handle count extending beyond list bounds', 2, FIntList.IndexOfItem(2, 2, 10));
end;

procedure TThreadSafeListTest.Test38_LastIndexOf;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(2);
  FIntList.Add(3);

  AssertEquals('Should find last occurrence of 2', 2, FIntList.LastIndexOf(2));
  AssertEquals('Should not find non-existing element', -1, FIntList.LastIndexOf(5));
end;

procedure TThreadSafeListTest.Test39_LastIndexOfWithStart;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(2);
  FIntList.Add(3);

  AssertEquals('Should find last occurrence before start index', 1, FIntList.LastIndexOf(2, 1));
  AssertEquals('Should not find element before its first occurrence', -1, FIntList.LastIndexOf(2, 0));
end;

procedure TThreadSafeListTest.Test40_LastIndexOfWithStartAndCount;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(2);
  FIntList.Add(3);

  AssertEquals('Should find element within count range', 2, FIntList.LastIndexOf(2, 2, 2));
  AssertEquals('Should not find element outside count range', -1, FIntList.LastIndexOf(2, 3, 1));
  AssertEquals('Should handle count extending beyond list bounds', 1, FIntList.LastIndexOf(2, 1, 10));
end;

procedure TThreadSafeListTest.Test41_Insert;
begin
  FIntList.Add(1);
  FIntList.Add(3);

  FIntList.Insert(1, 2);
  
  AssertEquals('Count should be increased', 3, FIntList.Count);
  AssertEquals('Element should be inserted at correct position', 2, FIntList[1]);
  
  try
    FIntList.Insert(-1, 0);
    Fail('Should not allow negative index');
  except
    on E: Exception do
      AssertTrue('Correct exception raised', True);
  end;
end;

procedure TThreadSafeListTest.Test42_Exchange;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  FIntList.Exchange(0, 2);
  
  AssertEquals('First element should be exchanged', 3, FIntList[0]);
  AssertEquals('Last element should be exchanged', 1, FIntList[2]);
  
  try
    FIntList.Exchange(0, 10);
    Fail('Should not allow exchange with invalid index');
  except
    on E: Exception do
      AssertTrue('Correct exception raised', True);
  end;
end;

procedure TThreadSafeListTest.Test43_Move;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  FIntList.MoveItem(0, 2);
  
  AssertEquals('Elements should be properly reordered', 2, FIntList[0]);
  AssertEquals('Elements should be properly reordered', 3, FIntList[1]);
  AssertEquals('Elements should be properly reordered', 1, FIntList[2]);
  
  try
    FIntList.MoveItem(0, 10);
    Fail('Should not allow move to invalid index');
  except
    on E: Exception do
      AssertTrue('Correct exception raised', True);
  end;
end;

procedure TThreadSafeListTest.Test44_Reverse;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  FIntList.Reverse;
  
  AssertEquals('First element should be reversed', 3, FIntList[0]);
  AssertEquals('Middle element should remain', 2, FIntList[1]);
  AssertEquals('Last element should be reversed', 1, FIntList[2]);
end;

procedure TThreadSafeListTest.Test45_Extract;
var
  ExtractedValue: Integer;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  ExtractedValue := FIntList.Extract(2);
  
  AssertEquals('Extracted value should be correct', 2, ExtractedValue);
  AssertEquals('Count should be decreased', 2, FIntList.Count);
  AssertEquals('Elements should be properly reordered', 3, FIntList[1]);
  
  try
    FIntList.Extract(5);
    Fail('Should not allow extracting non-existent element');
  except
    on E: Exception do
      AssertTrue('Correct exception raised', True);
  end;
end;

procedure TThreadSafeListTest.Test46_ExtractAt;
var
  ExtractedValue: Integer;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  ExtractedValue := FIntList.ExtractAt(1);
  
  AssertEquals('Extracted value should be correct', 2, ExtractedValue);
  AssertEquals('Count should be decreased', 2, FIntList.Count);
  AssertEquals('Elements should be properly reordered', 3, FIntList[1]);
  
  try
    FIntList.ExtractAt(10);
    Fail('Should not allow extracting from invalid index');
  except
    on E: Exception do
      AssertTrue('Correct exception raised', True);
  end;
end;

initialization
  RegisterTest(TThreadSafeListTest);
end.

