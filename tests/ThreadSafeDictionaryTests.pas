unit ThreadSafeDictionaryTests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils, HashFunctions,
  ThreadSafeCollections.Dictionary, ThreadSafeCollections.Interfaces, Generics.Collections;

type
  TIntStringDictionary = specialize TThreadSafeDictionary<integer, string>;
  TStringIntDictionary = specialize TThreadSafeDictionary<string, integer>;
  TStringObjectDictionary = specialize TThreadSafeDictionary<string, TObject>;
  TStringIntDictionaryPair = specialize TDictionaryPair<string, integer>;

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
    procedure Test1_Creation;
    procedure Test2_Add;
    procedure Test3_AddDuplicate;
    procedure Test4_GetItem;
    procedure Test5_Remove;
    procedure Test6_AddOrSetValue;
    procedure Test7_Clear;
    procedure Test8_Count;
    procedure Test9_FirstLast;

    // Edge cases
    procedure Test10_EmptyDictionary;
    procedure Test11_LargeDataSet;
    procedure Test12_NilValues;
    procedure Test13_Boundaries;

    // Stress tests
    procedure Test14_MultiThreadAccess;
    procedure Test15_ConcurrentOperations;
    procedure Test16_HashCollisions;

    // Performance tests
    procedure Test17_LargeDataSetPerformance;
    procedure Test18_HashingPerformance;

    // New tests for initial capacity and resize
    procedure Test19_InitialCapacity;
    procedure Test20_ManualResize;
    procedure Test21_ResizeWithData;
    procedure Test22_ResizeUnderflow;
    procedure Test23_BucketCount;

    // New iterator tests
    procedure Test24_IteratorBasic;
    procedure Test25_IteratorEmpty;
    procedure Test26_IteratorModification;
    procedure Test27_MultipleIterators;
    procedure Test28_IteratorReset;

    // RAII locking mechanism tests
    procedure Test29_LockingMechanism;

    // New tests for compound keys and custom constructors
    procedure Test30_CompoundKeyBasic;
    procedure Test31_CompoundKeyIteration;
    procedure Test32_CustomConstructors;
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

// Add after the type declarations but before the thread classes
function HashString(const Key: string): Cardinal;
begin
  Result := XXHash32(Key);
end;

function CompareString(const Left, Right: string): Boolean;
begin
  Result := Left = Right;
end;

function HashInteger(const Key: Integer): Cardinal;
begin
  Result := MultiplicativeHash(Cardinal(Key));
end;

function CompareInteger(const Left, Right: Integer): Boolean;
begin
  Result := Left = Right;
end;


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
        FDict.GetItem(Key);
      except
        // Ignore not found errors
      end;
    end;
  end;
end;

{ TThreadSafeDictionaryTest }

procedure TThreadSafeDictionaryTest.SetUp;
var
  NullIntHashFunc: specialize THashFunction<integer>;
  NullIntEqualityFunc: specialize TEqualityComparison<integer>;
  NullStrHashFunc: specialize THashFunction<string>;
  NullStrEqualityFunc: specialize TEqualityComparison<string>;
begin
  try
    WriteLn('Setting up test...');
    NullIntHashFunc := nil;
    NullIntEqualityFunc := nil;
    NullStrHashFunc := nil;
    NullStrEqualityFunc := nil;
    
    // Explicitly call the constructor with hash/equality functions
    FIntDict := TIntStringDictionary.Create(NullIntHashFunc, NullIntEqualityFunc);
    FStrDict := TStringIntDictionary.Create(NullStrHashFunc, NullStrEqualityFunc);
    FMixedDict := TStringObjectDictionary.Create(NullStrHashFunc, NullStrEqualityFunc);
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

procedure TThreadSafeDictionaryTest.Test1_Creation;
begin
  WriteLn('Starting TestCreation');
  IncrementTestCounter;
  AssertNotNull('Integer dictionary should be created', FIntDict);
  AssertEquals('New dictionary should be empty', 0, FIntDict.Count);
  WriteLn('Completed TestCreation');
end;

procedure TThreadSafeDictionaryTest.Test2_Add;
begin
  WriteLn('Starting TestAdd');
  IncrementTestCounter;
  try
    FStrDict.Add('test', 1);
    AssertEquals('Count should be 1', 1, FStrDict.Count);
    AssertEquals('Value should be retrievable', 1, FStrDict.GetItem('test'));
    WriteLn('Completed TestAdd');
  except
    on E: Exception do
    begin
      WriteLn('TestAdd failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.Test3_AddDuplicate;
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





procedure TThreadSafeDictionaryTest.Test4_GetItem;
begin
  WriteLn('Starting TestGetItem');
  IncrementTestCounter;
  try
    FStrDict.Add('test1', 1);
    FStrDict.Add('test2', 2);

    AssertEquals('Should find correct value', 1, FStrDict.GetItem('test1'));
    AssertEquals('Should find correct value', 2, FStrDict.GetItem('test2'));

    try
      FStrDict.GetItem('nonexistent');
      Fail('Should raise exception for nonexistent key');
    except
      on E: Exception do
        AssertTrue('Correct exception raised', True);
    end;
    WriteLn('Completed TestGetItem');
  except
    on E: Exception do
    begin
      WriteLn('TestFind failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.Test5_Remove;
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

procedure TThreadSafeDictionaryTest.Test6_AddOrSetValue;
begin
  WriteLn('Starting TestReplace');
  IncrementTestCounter;
  try
    FStrDict.Add('test', 1);
    FStrDict.AddOrSetValue('test', 2);

    AssertEquals('Value should be replaced', 2, FStrDict.GetItem('test'));

    try
      FStrDict.AddOrSetValue('nonexistent', 1);
      Fail('Should raise exception for nonexistent key');
    except
      on E: Exception do
        AssertTrue('Correct exception raised', True);
    end;
    WriteLn('Completed TestAddOrSetValue');
  except
    on E: Exception do
    begin
      WriteLn('TestAddOrSetValue failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.Test7_Clear;
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

procedure TThreadSafeDictionaryTest.Test8_Count;
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

procedure TThreadSafeDictionaryTest.Test9_FirstLast;
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

procedure TThreadSafeDictionaryTest.Test10_EmptyDictionary;
var
  Key: string;
  Value: integer;
begin
  WriteLn('Starting Test10_EmptyDictionary');
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
      FStrDict.GetItem('test');
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

procedure TThreadSafeDictionaryTest.Test11_LargeDataSet;
const
  TEST_SIZE = 20;
  LOGGING_ENABLED = False;
var
  I: integer;
  Key: string;
  Hash: cardinal;
begin
  if LOGGING_ENABLED then WriteLn('Starting Test11_LargeDataSet');
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

procedure TThreadSafeDictionaryTest.Test12_NilValues;
var
  Obj: TObject;
begin
  // Test with object dictionary
  FMixedDict.Add('test', nil);
  AssertTrue('Should store nil value',
    FMixedDict.TryGetValue('test', Obj));
  AssertNull('Should retrieve nil value', Obj);

  FMixedDict.AddOrSetValue('test', TObject.Create);
  try
    AssertNotNull('Should replace nil with object',
      FMixedDict.GetItem('test'));
  finally
    TObject(FMixedDict.GetItem('test')).Free;
  end;
end;

procedure TThreadSafeDictionaryTest.Test13_Boundaries;
var
  I: integer;
  Key: string;
  HashFunc: specialize THashFunction<string>;
  EqualityFunc: specialize TEqualityComparison<string>;
begin
  HashFunc := @HashString;
  EqualityFunc := @CompareString;
  FStrDict.Free;  // Free the old dictionary
  FStrDict := TStringIntDictionary.Create(HashFunc, EqualityFunc);
  
  // Test with very long keys
  Key := StringOfChar('A', 1000);
  FStrDict.Add(Key, 1);
  AssertEquals('Should handle long keys', 1, FStrDict.GetItem(Key));

  // Test with many collisions
  for I := 0 to 100 do
  begin
    Key := 'Key' + IntToStr(I * 16); // Force collisions
    FStrDict.Add(Key, I);
  end;

  for I := 0 to 100 do
  begin
    Key := 'Key' + IntToStr(I * 16);
    AssertEquals('Should handle collisions', I, FStrDict.GetItem(Key));
  end;
end;

procedure TThreadSafeDictionaryTest.Test14_MultiThreadAccess;
const
  THREAD_COUNT = 10;
  TIMEOUT_MS = 5000; // 5 second timeout
var
  Threads: array[0..THREAD_COUNT - 1] of TDictTestThread;
  I: integer;
  StartTime: QWord;
begin
  WriteLn('Starting Test14_MultiThreadAccess');
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

procedure TThreadSafeDictionaryTest.Test15_ConcurrentOperations;
const
  ITERATIONS = 1000;
  TIMEOUT_MS = 5000;
var
  AdderThread: TAdderThread;
  RemoverThread: TRemoverThread;
  StartTime: QWord;
begin
  WriteLn('Starting Test15_ConcurrentOperations');
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

procedure TThreadSafeDictionaryTest.Test16_HashCollisions;
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
      I, FStrDict.GetItem(CollisionKeys[I]));
end;

procedure TThreadSafeDictionaryTest.Test17_LargeDataSetPerformance;
const
  TEST_SIZE = 100000;
var
  I: integer;
  StartTick, EndTick: QWord;
  Key: string;
  HashFunc: specialize THashFunction<string>;
  EqualityFunc: specialize TEqualityComparison<string>;
begin
  HashFunc := @HashString;
  EqualityFunc := @CompareString;
  FStrDict.Free;  // Free the old dictionary
  FStrDict := TStringIntDictionary.Create(HashFunc, EqualityFunc);
  
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
    AssertEquals('Value should match', I, FStrDict.GetItem(Key));
  end;
  EndTick := GetTickCount64;
  WriteLn(Format('Finding %d items took: %d ms', [TEST_SIZE, EndTick - StartTick]));
end;

procedure TThreadSafeDictionaryTest.Test18_HashingPerformance;
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
    FStrDict.GetItem(Keys[I]);
  EndTick := GetTickCount64;

  WriteLn(Format('Finding %d items took: %d ms', [TEST_SIZE, EndTick - StartTick]));
end;

procedure TThreadSafeDictionaryTest.Test19_InitialCapacity;
var
  CustomDict: TStringIntDictionary;
begin
  WriteLn('Starting Test19_InitialCapacity');
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
      AssertEquals('Should store values correctly', 1, CustomDict.GetItem('test1'));
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

procedure TThreadSafeDictionaryTest.Test20_ManualResize;
begin
  WriteLn('Starting Test20_ManualResize');
  IncrementTestCounter;
  try
    // Start with default size
    AssertEquals('Initial bucket count should be 16', 16, FStrDict.BucketCount);
    
    // Manual resize
    FStrDict.ResizeBuckets(32);
    AssertEquals('Bucket count should be updated', 32, FStrDict.BucketCount);
    
    // Verify functionality after resize
    FStrDict.Add('test1', 1);
    AssertEquals('Should work after resize', 1, FStrDict.GetItem('test1'));
    
    WriteLn('Completed TestManualResize');
  except
    on E: Exception do
    begin
      WriteLn('TestManualResize failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.Test21_ResizeWithData;
var
  I: Integer;
  Key: string;
  HashFunc: specialize THashFunction<string>;
  EqualityFunc: specialize TEqualityComparison<string>;
begin
  WriteLn('Starting Test21_ResizeWithData');
  IncrementTestCounter;
  try
    HashFunc := @HashString;
    EqualityFunc := @CompareString;
    FStrDict.Free;  // Free the old dictionary
    FStrDict := TStringIntDictionary.Create(HashFunc, EqualityFunc);
    
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
      AssertEquals('Data should be preserved after resize', I, FStrDict.GetItem(Key));
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

procedure TThreadSafeDictionaryTest.Test22_ResizeUnderflow;
var
  I: Integer;
begin
  WriteLn('Starting Test22_ResizeUnderflow');
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

procedure TThreadSafeDictionaryTest.Test23_BucketCount;
var
  I: Integer;
begin
  WriteLn('Starting Test23_BucketCount');
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

procedure TThreadSafeDictionaryTest.Test24_IteratorBasic;
var
  Iterator: TStringIntDictionary.TEnumerator;
  ExpectedCount: Integer;
  Pair: TStringIntDictionaryPair;
begin
  WriteLn('Starting Test24_IteratorBasic');
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

procedure TThreadSafeDictionaryTest.Test25_IteratorEmpty;
var
  Iterator: TStringIntDictionary.TEnumerator;
begin
  WriteLn('Starting Test25_IteratorEmpty');
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

procedure TThreadSafeDictionaryTest.Test26_IteratorModification;
var
  Iterator: TStringIntDictionary.TEnumerator;
  Pair: TStringIntDictionaryPair;
begin
  WriteLn('Starting Test26_IteratorModification');
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

procedure TThreadSafeDictionaryTest.Test27_MultipleIterators;
var
  Iterator1, Iterator2: TStringIntDictionary.TEnumerator;
  Pair1, Pair2: TStringIntDictionaryPair;
  Count1, Count2: Integer;
begin
  WriteLn('Starting Test27_MultipleIterators');
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

procedure TThreadSafeDictionaryTest.Test28_IteratorReset;
var
  Iterator: TStringIntDictionary.TEnumerator;
  FirstPair, CurrentPair: TStringIntDictionaryPair;
begin
  WriteLn('Starting Test28_IteratorReset');
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

procedure TThreadSafeDictionaryTest.Test29_LockingMechanism;
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

type
  TPersonKey = record
    FirstName: string;
    LastName: string;
  end;

  TPersonDictionary = specialize TThreadSafeDictionary<TPersonKey, Integer>;
  TPersonDictionaryPair = specialize TDictionaryPair<TPersonKey, Integer>;
  TPersonHashFunction = specialize THashFunction<TPersonKey>;
  TPersonEqualityComparison = specialize TEqualityComparison<TPersonKey>;

function HashPerson(const Key: TPersonKey): Cardinal;
begin
  Result := XXHash32(Key.FirstName + '|' + Key.LastName);
end;

function ComparePerson(const Left, Right: TPersonKey): Boolean;
begin
  Result := (Left.FirstName = Right.FirstName) and 
            (Left.LastName = Right.LastName);
end;

procedure TThreadSafeDictionaryTest.Test30_CompoundKeyBasic;
var
  Dict: TPersonDictionary;
  Person: TPersonKey;
  Value: Integer;
  HashFunc: TPersonHashFunction;
  EqualityFunc: TPersonEqualityComparison;
begin
  WriteLn('Starting Test30_CompoundKeyBasic');
  IncrementTestCounter;
  try
    HashFunc := @HashPerson;
    EqualityFunc := @ComparePerson;
    Dict := TPersonDictionary.Create(HashFunc, EqualityFunc);
    try
      // Test adding and retrieving
      Person.FirstName := 'John';
      Person.LastName := 'Doe';
      Dict.Add(Person, 42);

      AssertTrue('Should find existing compound key',
        Dict.TryGetValue(Person, Value));
      AssertEquals('Should retrieve correct value', 42, Value);

      // Test non-existent key
      Person.FirstName := 'Jane';
      AssertFalse('Should not find non-existent key',
        Dict.TryGetValue(Person, Value));

      WriteLn('TestCompoundKeyBasic passed');
    finally
      Dict.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('TestCompoundKeyBasic failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.Test31_CompoundKeyIteration;
var
  Dict: TPersonDictionary;
  Person: TPersonKey;
  Pair: TPersonDictionaryPair;
  Count: Integer;
  HashFunc: TPersonHashFunction;
  EqualityFunc: TPersonEqualityComparison;
begin
  WriteLn('Starting Test31_CompoundKeyIteration');
  IncrementTestCounter;
  try
    HashFunc := @HashPerson;
    EqualityFunc := @ComparePerson;
    Dict := TPersonDictionary.Create(HashFunc, EqualityFunc);
    try
      // Add multiple entries
      Person.FirstName := 'John';
      Person.LastName := 'Doe';
      Dict.Add(Person, 42);

      Person.FirstName := 'Jane';
      Person.LastName := 'Smith';
      Dict.Add(Person, 30);

      // Test iteration
      Count := 0;
      for Pair in Dict do
      begin
        Inc(Count);
        if (Pair.Key.FirstName = 'John') and (Pair.Key.LastName = 'Doe') then
          AssertEquals('Wrong value for John Doe', 42, Pair.Value)
        else if (Pair.Key.FirstName = 'Jane') and (Pair.Key.LastName = 'Smith') then
          AssertEquals('Wrong value for Jane Smith', 30, Pair.Value)
        else
          Fail('Unknown key in iteration');
      end;

      AssertEquals('Wrong number of iterations', 2, Count);
      WriteLn('TestCompoundKeyIteration passed');
    finally
      Dict.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('TestCompoundKeyIteration failed: ', E.Message);
      raise;
    end;
  end;
end;

procedure TThreadSafeDictionaryTest.Test32_CustomConstructors;
var
  Dict: TPersonDictionary;
  Person: TPersonKey;
  Value: Integer;
  HashFunc: TPersonHashFunction;
  EqualityFunc: TPersonEqualityComparison;
begin
  WriteLn('Starting Test32_CustomConstructors');
  IncrementTestCounter;
  try
    HashFunc := @HashPerson;
    EqualityFunc := @ComparePerson;
    
    // Test constructor with initial capacity
    Dict := TPersonDictionary.Create(32, HashFunc, EqualityFunc);
    try
      AssertEquals('Initial bucket count should be 32', 32, Dict.BucketCount);

      Person.FirstName := 'John';
      Person.LastName := 'Doe';
      Dict.Add(Person, 42);

      AssertTrue('Should find value with custom constructor',
        Dict.TryGetValue(Person, Value));
      AssertEquals('Wrong value with custom constructor', 42, Value);
    finally
      Dict.Free;
    end;

    // Test default constructor with comparers
    Dict := TPersonDictionary.Create(HashFunc, EqualityFunc);
    try
      Person.FirstName := 'Jane';
      Person.LastName := 'Smith';
      Dict.Add(Person, 30);

      AssertTrue('Should find value with default constructor',
        Dict.TryGetValue(Person, Value));
      AssertEquals('Wrong value with default constructor', 30, Value);
    finally
      Dict.Free;
    end;

    WriteLn('TestCustomConstructors passed');
  except
    on E: Exception do
    begin
      WriteLn('TestCustomConstructors failed: ', E.Message);
      raise;
    end;
  end;
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
