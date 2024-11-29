unit ThreadSafeDictionaryTests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils,
  ThreadSafeCollections.Dictionary, ThreadSafeCollections.Interfaces;

type
  TIntStringDictionary = specialize TThreadSafeDictionary<integer, string>;
  TStringIntDictionary = specialize TThreadSafeDictionary<string, integer>;
  TStringObjectDictionary = specialize TThreadSafeDictionary<string, TObject>;

  TAdderThread = class(TThread)
  private
    FDict: TStringIntDictionary;
    FIterations: integer;
  public
    constructor Create(ADict: TStringIntDictionary; AIterations: integer);
    procedure Execute; override;
  end;

  TRemoverThread = class(TThread)
  private
    FDict: TStringIntDictionary;
    FIterations: integer;
  public
    constructor Create(ADict: TStringIntDictionary; AIterations: integer);
    procedure Execute; override;
  end;

  // Test thread for stress testing
  TDictTestThread = class(TThread)
  private
    FDict: TStringIntDictionary;
    FStartValue: integer;
    FOperations: integer;
  public
    constructor Create(ADict: TStringIntDictionary;
      AStartValue, AOperations: integer);
    procedure Execute; override;
  end;

  TStringIntPair = specialize TPair<string, Integer>;

  TThreadSafeDictionaryTest = class(TTestCase)
  private
    FIntDict: TIntStringDictionary;
    FStrDict: TStringIntDictionary;
    FMixedDict: TStringObjectDictionary;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Basic functionality tests
    procedure TestCreation;
    procedure TestAdd;
    procedure TestAddDuplicate;
    procedure TestFind;
    procedure TestRemove;
    procedure TestReplace;
    procedure TestClear;
    procedure TestCount;
    procedure TestFirstLast;

    // Edge cases
    procedure TestEmptyDictionary;
    procedure TestLargeDataSet;
    procedure TestNilValues;
    procedure TestBoundaries;

    // Stress tests
    procedure TestMultiThreadAccess;
    procedure TestConcurrentOperations;
    procedure TestHashCollisions;

    // Performance tests
    procedure TestLargeDataSetPerformance;
    procedure TestHashingPerformance;

    // New tests for initial capacity and resize
    procedure TestInitialCapacity;
    procedure TestManualResize;
    procedure TestResizeWithData;
    procedure TestResizeUnderflow;
    procedure TestBucketCount;

    // New iterator tests
    procedure TestIteratorBasic;
    procedure TestIteratorEmpty;
    procedure TestIteratorModification;
    procedure TestMultipleIterators;
    procedure TestIteratorReset;

    // RAII locking mechanism tests
    procedure TestLockingMechanism;
  end;

  // Add this type after other test thread types
  TLockTestThread = class(TThread)
  private
    FDict: TStringIntDictionary;
    FLockCount: Integer;
    FIterations: Integer;
  public
    constructor Create(ADict: TStringIntDictionary; AIterations: Integer);
    procedure Execute; override;
    property LockCount: Integer read FLockCount;
  end;

implementation

const
  STRESS_THREAD_COUNT = 10;
  OPERATIONS_PER_THREAD = 1000;
  LARGE_DATASET_SIZE = 100000;

var
  TestCounter: integer = 0;

procedure IncrementTestCounter;
begin
  Inc(TestCounter);
  WriteLn(Format('Running test #%d', [TestCounter]));
end;

{ TAdderThread }

constructor TAdderThread.Create(ADict: TStringIntDictionary; AIterations: integer);
begin
  inherited Create(True);
  FDict := ADict;
  FIterations := AIterations;
  FreeOnTerminate := False;
end;

procedure TAdderThread.Execute;
var
  I: integer;
begin
  for I := 0 to FIterations - 1 do
  try
    FDict.Add('Key' + IntToStr(I), I);
  except
    // Ignore duplicate key errors
  end;
end;

{ TRemoverThread }

constructor TRemoverThread.Create(ADict: TStringIntDictionary; AIterations: integer);
begin
  inherited Create(True);
  FDict := ADict;
  FIterations := AIterations;
  FreeOnTerminate := False;
end;

procedure TRemoverThread.Execute;
var
  I: integer;
begin
  for I := 0 to FIterations - 1 do
    FDict.Remove('Key' + IntToStr(I));
end;

{ TDictTestThread }

constructor TDictTestThread.Create(ADict: TStringIntDictionary;
  AStartValue, AOperations: integer);
begin
  inherited Create(True);
  FDict := ADict;
  FStartValue := AStartValue;
  FOperations := AOperations;
  FreeOnTerminate := False;
end;

procedure TDictTestThread.Execute;
var
  I: integer;
  Operation: integer;
  Key: string;
begin
  for I := 0 to FOperations - 1 do
  begin
    Operation := Random(3); // 0: Add, 1: Remove, 2: Find
    Key := 'Key' + IntToStr(FStartValue + I);

    case Operation of
      0:
      try
        FDict.Add(Key, FStartValue + I);
      except
        // Ignore duplicate key errors
      end;
      1: FDict.Remove(Key);
      2:
      try
        FDict.Find(Key);
      except
        // Ignore not found errors
      end;
    end;
  end;
end;

{ TThreadSafeDictionaryTest }

procedure TThreadSafeDictionaryTest.SetUp;
begin
  try
    WriteLn('Setting up test...');
    FIntDict := TIntStringDictionary.Create;
    FStrDict := TStringIntDictionary.Create;
    FMixedDict := TStringObjectDictionary.Create;
    WriteLn('Setup complete');
  except
    on E: Exception do
      WriteLn('Setup failed: ', E.Message);
  end;
end;

procedure TThreadSafeDictionaryTest.TearDown;
begin
  FIntDict.Free;
  FStrDict.Free;
  FMixedDict.Free;
end;

procedure TThreadSafeDictionaryTest.TestCreation;
begin
  WriteLn('Starting TestCreation');
  IncrementTestCounter;
  AssertNotNull('Integer dictionary should be created', FIntDict);
  AssertEquals('New dictionary should be empty', 0, FIntDict.Count);
  WriteLn('Completed TestCreation');
end;

procedure TThreadSafeDictionaryTest.TestAdd;
begin
  WriteLn('Starting TestAdd');
  IncrementTestCounter;
  try
    FStrDict.Add('test', 1);
    AssertEquals('Count should be 1', 1, FStrDict.Count);
    AssertEquals('Value should be retrievable', 1, FStrDict.Find('test'));
    WriteLn('Completed TestAdd');
  except
    on E: Exception do
    begin
      WriteLn('TestAdd failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestAddDuplicate;
begin
  WriteLn('Starting TestAddDuplicate');
  IncrementTestCounter;
  FStrDict.Add('test', 1);
  try
    FStrDict.Add('test', 2);
    Fail('Should raise exception for duplicate key');
  except
    on E: Exception do
      AssertTrue('Correct exception raised', True);
  end;
  WriteLn('Completed TestAddDuplicate');
end;

procedure TThreadSafeDictionaryTest.TestLargeDataSetPerformance;
const
  TEST_SIZE = 100000;
var
  I: integer;
  StartTick, EndTick: QWord;
  Key: string;
begin
  StartTick := GetTickCount64;

  // Test adding large number of items
  for I := 0 to TEST_SIZE - 1 do
  begin
    Key := 'Key' + IntToStr(I);
    FStrDict.Add(Key, I);
  end;

  EndTick := GetTickCount64;
  WriteLn(Format('Adding %d items took: %d ms', [TEST_SIZE, EndTick - StartTick]));

  // Test finding items
  StartTick := GetTickCount64;
  for I := 0 to TEST_SIZE - 1 do
  begin
    Key := 'Key' + IntToStr(I);
    AssertEquals('Value should match', I, FStrDict.Find(Key));
  end;
  EndTick := GetTickCount64;
  WriteLn(Format('Finding %d items took: %d ms', [TEST_SIZE, EndTick - StartTick]));
end;

procedure TThreadSafeDictionaryTest.TestHashCollisions;
var
  I: integer;
  CollisionKeys: array[0..9] of string;
begin
  // Create keys that will likely cause collisions
  for I := 0 to 9 do
    CollisionKeys[I] := 'Key' + IntToStr(I * 16);
  // Using multiples of 16 to force collisions

  // Add values
  for I := 0 to 9 do
    FStrDict.Add(CollisionKeys[I], I);

  // Verify all values are still accessible
  for I := 0 to 9 do
    AssertEquals('Value should be retrievable after collision',
      I, FStrDict.Find(CollisionKeys[I]));
end;

procedure TThreadSafeDictionaryTest.TestFind;
begin
  WriteLn('Starting TestFind');
  IncrementTestCounter;
  try
    FStrDict.Add('test1', 1);
    FStrDict.Add('test2', 2);

    AssertEquals('Should find correct value', 1, FStrDict.Find('test1'));
    AssertEquals('Should find correct value', 2, FStrDict.Find('test2'));

    try
      FStrDict.Find('nonexistent');
      Fail('Should raise exception for nonexistent key');
    except
      on E: Exception do
        AssertTrue('Correct exception raised', True);
    end;
    WriteLn('Completed TestFind');
  except
    on E: Exception do
    begin
      WriteLn('TestFind failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestRemove;
begin
  WriteLn('Starting TestRemove');
  IncrementTestCounter;
  try
    FStrDict.Add('test1', 1);
    FStrDict.Add('test2', 2);

    AssertTrue('Should remove existing key', FStrDict.Remove('test1'));
    AssertEquals('Count should decrease', 1, FStrDict.Count);
    AssertFalse('Should return false for nonexistent key', FStrDict.Remove('test1'));
    WriteLn('Completed TestRemove');
  except
    on E: Exception do
    begin
      WriteLn('TestRemove failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestReplace;
begin
  WriteLn('Starting TestReplace');
  IncrementTestCounter;
  try
    FStrDict.Add('test', 1);
    FStrDict.Replace('test', 2);

    AssertEquals('Value should be replaced', 2, FStrDict.Find('test'));

    try
      FStrDict.Replace('nonexistent', 1);
      Fail('Should raise exception for nonexistent key');
    except
      on E: Exception do
        AssertTrue('Correct exception raised', True);
    end;
    WriteLn('Completed TestReplace');
  except
    on E: Exception do
    begin
      WriteLn('TestReplace failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestClear;
begin
  WriteLn('Starting TestClear');
  IncrementTestCounter;
  try
    FStrDict.Add('test1', 1);
    FStrDict.Add('test2', 2);

    FStrDict.Clear;
    AssertEquals('Count should be 0 after clear', 0, FStrDict.Count);
    WriteLn('Completed TestClear');
  except
    on E: Exception do
    begin
      WriteLn('TestClear failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestCount;
begin
  WriteLn('Starting TestCount');
  IncrementTestCounter;
  try
    AssertEquals('Empty dictionary count', 0, FStrDict.Count);

    FStrDict.Add('test1', 1);
    AssertEquals('Count after one add', 1, FStrDict.Count);

    FStrDict.Add('test2', 2);
    AssertEquals('Count after second add', 2, FStrDict.Count);

    FStrDict.Remove('test1');
    AssertEquals('Count after remove', 1, FStrDict.Count);
    WriteLn('Completed TestCount');
  except
    on E: Exception do
    begin
      WriteLn('TestCount failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestFirstLast;
var
  Key: string;
  Value: integer;
begin
  WriteLn('Starting TestFirstLast');
  IncrementTestCounter;
  try

    AssertFalse('First should return false when empty', FStrDict.First(Key, Value));
    AssertFalse('Last should return false when empty', FStrDict.Last(Key, Value));

    FStrDict.Add('test1', 1);
    FStrDict.Add('test2', 2);

    AssertTrue('First should return true', FStrDict.First(Key, Value));
    AssertTrue('Last should return true', FStrDict.Last(Key, Value));
    WriteLn('Completed TestFirstLast');
  except
    on E: Exception do
    begin
      WriteLn('TestFirstLast failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestEmptyDictionary;
var
  Key: string;
  Value: integer;
begin
  WriteLn('Starting TestEmptyDictionary');
  IncrementTestCounter;
  try


    AssertFalse('Remove should return false on empty dictionary',
      FStrDict.Remove('nonexistent'));
    AssertFalse('First should return false on empty dictionary',
      FStrDict.First(Key, Value));  // This line was incomplete in the original
    AssertFalse('Last should return false on empty dictionary',
      FStrDict.Last(Key, Value));
    AssertFalse('TryGetValue should return false on empty dictionary',
      FStrDict.TryGetValue('test', Value));

    try
      FStrDict.Find('test');
      Fail('Find should raise exception on empty dictionary');
    except
      on E: Exception do
        AssertTrue('Correct exception raised', True);
    end;
    WriteLn('Completed TestEmptyDictionary');
  except
    on E: Exception do
    begin
      WriteLn('TestEmptyDictionary failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestLargeDataSet;
const
  TEST_SIZE = 20;
  LOGGING_ENABLED = False;
var
  I: integer;
  Key: string;
  Hash: cardinal;
begin
  if LOGGING_ENABLED then WriteLn('Starting TestLargeDataSet');
  IncrementTestCounter;
  try
    if LOGGING_ENABLED then WriteLn('Adding items...');
    for I := 0 to TEST_SIZE - 1 do
    begin
      if LOGGING_ENABLED then WriteLn(Format('About to add item %d', [I]));
      Key := 'Key' + IntToStr(I);
      if LOGGING_ENABLED then WriteLn(Format('Created key: %s', [Key]));

      // Get hash value before adding - use UInt32 to ensure positive display
      Hash := FStrDict.GetHashValue(Key);
      if LOGGING_ENABLED then WriteLn(Format('Hash for key %s: %u', [Key, Hash]));
      // Changed %d to %u

      if LOGGING_ENABLED then WriteLn('About to call Add method');
      try
        FStrDict.Add(Key, I);
        if LOGGING_ENABLED then
          WriteLn(Format('Successfully added item %d (Hash: %u)', [I, Hash]));
        // Changed %d to %u
        if LOGGING_ENABLED then
          WriteLn(Format('Current dictionary count: %d', [FStrDict.Count]));
      except
        on E: Exception do
        begin
          if LOGGING_ENABLED then
            WriteLn(Format('Exception during Add: %s', [E.Message]));
          if LOGGING_ENABLED then
            WriteLn(Format('Failed to add item %d: %s (Hash: %u)', [I, E.Message, Hash]));
          // Changed %d to %u
          raise;
        end;
      end;
    end;

    if LOGGING_ENABLED then WriteLn('Completed TestLargeDataSet');
  except
    on E: Exception do
    begin
      WriteLn(Format('TestLargeDataSet failed with exception: %s', [E.Message]));
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestNilValues;
var
  Obj: TObject;
begin
  // Test with object dictionary
  FMixedDict.Add('test', nil);
  AssertTrue('Should store nil value',
    FMixedDict.TryGetValue('test', Obj));
  AssertNull('Should retrieve nil value', Obj);

  FMixedDict.Replace('test', TObject.Create);
  try
    AssertNotNull('Should replace nil with object',
      FMixedDict.Find('test'));
  finally
    TObject(FMixedDict.Find('test')).Free;
  end;
end;

procedure TThreadSafeDictionaryTest.TestBoundaries;
var
  I: integer;
  Key: string;
begin
  // Test with very long keys
  Key := StringOfChar('A', 1000);
  FStrDict.Add(Key, 1);
  AssertEquals('Should handle long keys', 1, FStrDict.Find(Key));

  // Test with many collisions
  for I := 0 to 100 do
  begin
    Key := 'Key' + IntToStr(I * 16); // Force collisions
    FStrDict.Add(Key, I);
  end;

  for I := 0 to 100 do
  begin
    Key := 'Key' + IntToStr(I * 16);
    AssertEquals('Should handle collisions', I, FStrDict.Find(Key));
  end;
end;

procedure TThreadSafeDictionaryTest.TestMultiThreadAccess;
const
  THREAD_COUNT = 10;
  TIMEOUT_MS = 5000; // 5 second timeout
var
  Threads: array[0..THREAD_COUNT - 1] of TDictTestThread;
  I: integer;
  StartTime: QWord;
begin
  WriteLn('Starting TestMultiThreadAccess');
  StartTime := GetTickCount64;

  // Create and start threads
  for I := 0 to THREAD_COUNT - 1 do
  begin
    WriteLn(Format('Creating thread %d', [I]));
    Threads[I] := TDictTestThread.Create(FStrDict, I * 1000, 1000);
    Threads[I].Start;
  end;

  // Wait for all threads to complete
  for I := 0 to THREAD_COUNT - 1 do
  begin
    if GetTickCount64 - StartTime > TIMEOUT_MS then
    begin
      WriteLn('Thread timeout detected');
      Break;
    end;
    WriteLn(Format('Waiting for thread %d', [I]));
    Threads[I].WaitFor;
    Threads[I].Free;
  end;

  WriteLn('TestMultiThreadAccess completed');
end;

procedure TThreadSafeDictionaryTest.TestConcurrentOperations;
const
  ITERATIONS = 1000;
  TIMEOUT_MS = 5000;
var
  AdderThread: TAdderThread;
  RemoverThread: TRemoverThread;
  StartTime: QWord;
begin
  WriteLn('Starting TestConcurrentOperations');
  StartTime := GetTickCount64;

  AdderThread := TAdderThread.Create(FStrDict, ITERATIONS);
  RemoverThread := TRemoverThread.Create(FStrDict, ITERATIONS);

  AdderThread.Start;
  RemoverThread.Start;

  while (not AdderThread.Finished or not RemoverThread.Finished) and
    (GetTickCount64 - StartTime <= TIMEOUT_MS) do
    Sleep(10);

  if GetTickCount64 - StartTime > TIMEOUT_MS then
    WriteLn('Thread timeout detected');

  AdderThread.Free;
  RemoverThread.Free;

  WriteLn('TestConcurrentOperations completed');
end;

procedure TThreadSafeDictionaryTest.TestHashingPerformance;
const
  TEST_SIZE = 100000;
var
  StartTick, EndTick: QWord;
  I: integer;
  Keys: array of string;
begin
  SetLength(Keys, TEST_SIZE);

  // Generate test keys
  for I := 0 to TEST_SIZE - 1 do
    Keys[I] := 'Key' + IntToStr(I);

  // Test add performance
  StartTick := GetTickCount64;
  for I := 0 to TEST_SIZE - 1 do
    FStrDict.Add(Keys[I], I);
  EndTick := GetTickCount64;

  WriteLn(Format('Adding %d items took: %d ms', [TEST_SIZE, EndTick - StartTick]));

  // Test find performance
  StartTick := GetTickCount64;
  for I := 0 to TEST_SIZE - 1 do
    FStrDict.Find(Keys[I]);
  EndTick := GetTickCount64;

  WriteLn(Format('Finding %d items took: %d ms', [TEST_SIZE, EndTick - StartTick]));
end;

procedure TThreadSafeDictionaryTest.TestInitialCapacity;
var
  CustomDict: TStringIntDictionary;
begin
  WriteLn('Starting TestInitialCapacity');
  IncrementTestCounter;
  try
    // Test with custom initial capacity
    CustomDict := TStringIntDictionary.Create(100);
    try
      // Should be adjusted to next power of 2 (128)
      AssertEquals('Initial bucket count should be 128', 128, CustomDict.BucketCount);
      
      // Verify functionality with custom capacity
      CustomDict.Add('test1', 1);
      CustomDict.Add('test2', 2);
      AssertEquals('Should store values correctly', 1, CustomDict.Find('test1'));
    finally
      CustomDict.Free;
    end;
    
    // Test minimum capacity enforcement
    CustomDict := TStringIntDictionary.Create(2);
    try
      AssertEquals('Should enforce minimum bucket count', 4, CustomDict.BucketCount);
    finally
      CustomDict.Free;
    end;
    
    WriteLn('Completed TestInitialCapacity');
  except
    on E: Exception do
    begin
      WriteLn('TestInitialCapacity failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestManualResize;
begin
  WriteLn('Starting TestManualResize');
  IncrementTestCounter;
  try
    // Start with default size
    AssertEquals('Initial bucket count should be 16', 16, FStrDict.BucketCount);
    
    // Manual resize
    FStrDict.ResizeBuckets(32);
    AssertEquals('Bucket count should be updated', 32, FStrDict.BucketCount);
    
    // Verify functionality after resize
    FStrDict.Add('test1', 1);
    AssertEquals('Should work after resize', 1, FStrDict.Find('test1'));
    
    WriteLn('Completed TestManualResize');
  except
    on E: Exception do
    begin
      WriteLn('TestManualResize failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestResizeWithData;
var
  I: Integer;
  Key: string;
begin
  WriteLn('Starting TestResizeWithData');
  IncrementTestCounter;
  try
    // Add some data
    for I := 1 to 10 do
    begin
      Key := 'Key' + IntToStr(I);
      FStrDict.Add(Key, I);
    end;
    
    // Resize larger
    FStrDict.ResizeBuckets(64);
    AssertEquals('Bucket count should be updated', 64, FStrDict.BucketCount);
    
    // Verify all data still accessible
    for I := 1 to 10 do
    begin
      Key := 'Key' + IntToStr(I);
      AssertEquals('Data should be preserved after resize', I, FStrDict.Find(Key));
    end;
    
    WriteLn('Completed TestResizeWithData');
  except
    on E: Exception do
    begin
      WriteLn('TestResizeWithData failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestResizeUnderflow;
var
  I: Integer;
begin
  WriteLn('Starting TestResizeUnderflow');
  IncrementTestCounter;
  try
    // Add enough items to prevent small resize
    for I := 1 to 100 do
      FStrDict.Add('Key' + IntToStr(I), I);
      
    // Attempt resize too small for current items
    try
      FStrDict.ResizeBuckets(4);
      Fail('Should raise exception when resize too small for current items');
    except
      on E: Exception do
        AssertTrue('Correct exception raised', True);
    end;
    
    WriteLn('Completed TestResizeUnderflow');
  except
    on E: Exception do
    begin
      WriteLn('TestResizeUnderflow failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestBucketCount;
var
  I: Integer;
begin
  WriteLn('Starting TestBucketCount');
  IncrementTestCounter;
  try
    AssertEquals('Initial bucket count should be 16', 16, FStrDict.BucketCount);
    
    // Add items to trigger automatic resize
    for I := 1 to 20 do
      FStrDict.Add('Key' + IntToStr(I), I);
      
    AssertTrue('Bucket count should increase automatically', 
      FStrDict.BucketCount > 16);
    
    WriteLn('Completed TestBucketCount');
  except
    on E: Exception do
    begin
      WriteLn('TestBucketCount failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestIteratorBasic;
var
  Iterator: TStringIntDictionary.TDictionaryEnumerator;
  ExpectedCount: Integer;
  Pair: specialize TPair<string, Integer>;
begin
  WriteLn('Starting TestIteratorBasic');
  IncrementTestCounter;
  try
    // Add test data
    FStrDict.Add('one', 1);
    FStrDict.Add('two', 2);
    FStrDict.Add('three', 3);
    
    ExpectedCount := 0;
    Iterator := FStrDict.GetEnumerator;
    try
      while Iterator.MoveNext do
      begin
        Inc(ExpectedCount);
        Pair := Iterator.Current;
        AssertTrue('Key should not be empty', Pair.Key <> '');
        AssertTrue('Value should be between 1 and 3', (Pair.Value >= 1) and (Pair.Value <= 3));
      end;
      
      AssertEquals('Iterator should visit all items', 3, ExpectedCount);
    finally
      Iterator.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('TestIteratorBasic failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestIteratorEmpty;
var
  Iterator: TStringIntDictionary.TDictionaryEnumerator;
begin
  WriteLn('Starting TestIteratorEmpty');
  IncrementTestCounter;
  try
    Iterator := FStrDict.GetEnumerator;
    try
      AssertFalse('Empty dictionary should return false on MoveNext',
        Iterator.MoveNext);
    finally
      Iterator.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('TestIteratorEmpty failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestIteratorModification;
var
  Iterator: TStringIntDictionary.TDictionaryEnumerator;
  Pair: specialize TPair<string, Integer>;
begin
  WriteLn('Starting TestIteratorModification');
  IncrementTestCounter;
  try
    FStrDict.Add('one', 1);
    FStrDict.Add('two', 2);
    
    Iterator := FStrDict.GetEnumerator;
    try
      AssertTrue('Should get first item', Iterator.MoveNext);
      
      // Modify dictionary during iteration
      FStrDict.Add('three', 3);
      FStrDict.Remove('two');
      
      while Iterator.MoveNext do
      begin
        Pair := Iterator.Current;
        AssertTrue('Key should not be empty', Pair.Key <> '');
        AssertTrue('Value should be valid', Pair.Value > 0);
      end;
    finally
      Iterator.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('TestIteratorModification failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestMultipleIterators;
var
  Iterator1, Iterator2: TStringIntDictionary.TDictionaryEnumerator;
  Pair1, Pair2: specialize TPair<string, Integer>;
  Count1, Count2: Integer;
begin
  WriteLn('Starting TestMultipleIterators');
  IncrementTestCounter;
  try
    FStrDict.Add('one', 1);
    FStrDict.Add('two', 2);
    FStrDict.Add('three', 3);
    
    Iterator1 := FStrDict.GetEnumerator;
    Iterator2 := FStrDict.GetEnumerator;
    try
      Count1 := 0;
      Count2 := 0;
      
      while Iterator1.MoveNext do
      begin
        Inc(Count1);
        Pair1 := Iterator1.Current;
        if Iterator2.MoveNext then
        begin
          Inc(Count2);
          Pair2 := Iterator2.Current;
        end;
      end;
      
      AssertEquals('Both iterators should visit same number of items', Count1, Count2);
      AssertEquals('Iterators should visit all items', 3, Count1);
    finally
      Iterator1.Free;
      Iterator2.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('TestMultipleIterators failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestIteratorReset;
var
  Iterator: TStringIntDictionary.TDictionaryEnumerator;
  FirstPair, CurrentPair: specialize TPair<string, Integer>;
begin
  WriteLn('Starting TestIteratorReset');
  IncrementTestCounter;
  try
    FStrDict.Add('one', 1);
    FStrDict.Add('two', 2);
    
    // Get first item with first iterator
    Iterator := FStrDict.GetEnumerator;
    try
      AssertTrue('Should get first item', Iterator.MoveNext);
      FirstPair := Iterator.Current;
      
      // Move to next item
      AssertTrue('Should get second item', Iterator.MoveNext);
      
      // Get new iterator to start over
      Iterator.Free;
      Iterator := FStrDict.GetEnumerator;
      
      // First item with new iterator should match original first item
      AssertTrue('Should get first item with new iterator', Iterator.MoveNext);
      CurrentPair := Iterator.Current;
      AssertEquals('Key should match after reset', FirstPair.Key, CurrentPair.Key);
      AssertEquals('Value should match after reset', FirstPair.Value, CurrentPair.Value);
    finally
      Iterator.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('TestIteratorReset failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.TestLockingMechanism;
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
    Threads[I] := TLockTestThread.Create(FStrDict, ITERATIONS);
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

{ TLockTestThread }

constructor TLockTestThread.Create(ADict: TStringIntDictionary; AIterations: Integer);
begin
  inherited Create(True);
  FDict := ADict;
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
      LockToken := FDict.Lock;
      
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
  RegisterTest(TThreadSafeDictionaryTest);
end.
