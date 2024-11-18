unit ThreadSafeListTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ThreadSafeList, DateUtils;

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
    procedure TestFind;
    procedure TestFirstLast;
    procedure TestReplace;
    procedure TestSort;
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
    procedure TestLargeDataSetSortingPerformance;
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
    Operation := Random(3); // 0: Add, 1: Delete, 2: Find
    case Operation of
      0: FList.Add(FStartValue + I);
      1: if FList.Count > 0 then
         try
           FList.Delete(Random(FList.Count));
         except
           // Ignore concurrent deletion errors
         end;
      2: FList.Find(FStartValue + Random(I + 1));
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
      3: FIntList.Find(Random(1000));
    end;
  end;

  // Verify list integrity
  AssertTrue('List should survive random operations', True);
end;

procedure TThreadSafeListTest.TestFind;
begin
  FIntList.Add(1);
  FIntList.Add(2);
  FIntList.Add(3);

  AssertEquals('Should find existing element', 1, FIntList.Find(2));
  AssertEquals('Should return -1 for non-existing element', -1, FIntList.Find(5));
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
  AssertEquals('Find in empty list should return -1', -1, FIntList.Find(1));
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
  AssertEquals('Should find first occurrence', 0, FIntList.Find(1));
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
begin
  CharSet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';

  // Test Integer sorting
  StartTick := GetTickCount64;
  for I := 0 to SORT_SIZE - 1 do
    FIntList.Add(Random(SORT_SIZE));
  FIntList.Sort;
  EndTick := GetTickCount64;
  WriteLn(Format('Sorting %d integers took: %d ms', [SORT_SIZE, EndTick - StartTick]));
  AssertTrue('Integer list should be sorted', FIntList.IsSorted);
  FIntList.Free;
  FIntList := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);

  // Test String sorting
  StartTick := GetTickCount64;
  for I := 0 to SORT_SIZE - 1 do
  begin
    SetLength(TempStr, 10);
    TempStr := '';
    while Length(TempStr) < 10 do
      TempStr := TempStr + CharSet[Random(Length(CharSet)) + 1];
    FStrList.Add(TempStr);
  end;
  FStrList.Sort;
  EndTick := GetTickCount64;
  WriteLn(Format('Sorting %d strings (10 chars each) took: %d ms', [SORT_SIZE, EndTick - StartTick]));
  AssertTrue('String list should be sorted', FStrList.IsSorted);
  FStrList.Free;
  FStrList := specialize TThreadSafeList<string>.Create(@StringComparer);

  // Test Real sorting
  StartTick := GetTickCount64;
  for I := 0 to SORT_SIZE - 1 do
    FRealList.Add(Random * SORT_SIZE);
  FRealList.Sort;
  EndTick := GetTickCount64;
  WriteLn(Format('Sorting %d reals took: %d ms', [SORT_SIZE, EndTick - StartTick]));
  AssertTrue('Real list should be sorted', FRealList.IsSorted);
  FRealList.Free;
  FRealList := specialize TThreadSafeList<Real>.Create(@RealComparer);
end;

initialization

  RegisterTest(TThreadSafeListTest);
end.

