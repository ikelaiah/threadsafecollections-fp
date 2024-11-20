unit ThreadSafeHashSetTests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ThreadSafeCollections.HashSet, Math;

const
  INITIAL_BUCKET_COUNT = 16;

type
  TStudent = record
    Name: string;
    Age: Integer;
  end;

  TStressTestThread = class(TThread)
  private
    FSet: TThreadSafeHashSetInteger;
    FStartValue: Integer;
    FOperations: Integer;
  public
    constructor Create(ASet: TThreadSafeHashSetInteger; AStartValue, AOperations: Integer);
    procedure Execute; override;
  end;

  TCollisionThread = class(TThread)
  private
    const
      COLLISION_GROUPS = 2;  // Same as in the test
  private
    FSet: TThreadSafeHashSetString;
    FStartIndex: Integer;
    FCount: Integer;
  public
    constructor Create(ASet: TThreadSafeHashSetString; 
      AStartIndex, ACount: Integer);
    procedure Execute; override;
  end;

  TThreadSafeHashSetTest = class(TTestCase)
  private
    FIntSet: TThreadSafeHashSetInteger;
    FStrSet: TThreadSafeHashSetString;
    FBoolSet: TThreadSafeHashSetBoolean;
    FRealSet: TThreadSafeHashSetReal;
    FStudentSet: specialize TThreadSafeHashSet<TStudent>;
    procedure Log(const Msg: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test1_BasicOperations;
    procedure Test2_SimpleAdd;
    procedure Test3_SimpleRemove;
    procedure Test4_Duplicates;
    procedure Test5_Clear;
    procedure Test6_StudentBasic;
    procedure Test7_LargeDataSet;
    procedure Test8_ConcurrentAccess;
    procedure Test9_StressTest;
    procedure Test10_HashCollisions;
    procedure Test11_AggressiveCollisions;
  end;



// Student record comparers and hash functions
function StudentEquals(const A, B: TStudent): Boolean;
function StudentHash(const Value: TStudent): Cardinal;

type
  THashFunction = function(const Value: string): Cardinal;

type
  TCollisionHashSet = class(TThreadSafeHashSetString)
  public
    constructor Create(AInitialCapacity: Integer = INITIAL_BUCKET_COUNT); reintroduce;
  end;

  // Define a specific hash function type for strings
  // TStringHashFunction = function(const Value: string): Cardinal;

  // TTestHashSet = class(TThreadSafeHashSetString)
  // public
  //   procedure SetInternalHashFunction(AHashFunction: TStringHashFunction);
  // end;

implementation

uses HashFunctions;

const
  LOGGING_ENABLED = True;

function ForceCollisionHash(const Value: string): Cardinal;
begin
  // Always return the same hash value to force maximum collisions
  Result := $DEADBEEF;  // Any constant value will do
end;

constructor TCollisionHashSet.Create(AInitialCapacity: Integer);
var
  EqualityComparer: specialize TEqualityComparer<string>;
begin
  EqualityComparer := @StringEquals;
  inherited Create(@ForceCollisionHash, AInitialCapacity);
end;

{ TTestHashSet }

// procedure TTestHashSet.SetInternalHashFunction(AHashFunction: TStringHashFunction);
// begin
//   // Directly assign the hash function
//   FHashFunction := specialize THashFunction<string>(AHashFunction);
// end;

function StudentEquals(const A, B: TStudent): Boolean;
begin
  Result := (A.Name = B.Name) and (A.Age = B.Age);
end;

function StudentHash(const Value: TStudent): Cardinal;
var
  NameHash, AgeHash: Cardinal;
begin
  NameHash := XXHash32(Value.Name);
  AgeHash := MultiplicativeHash(Cardinal(Value.Age));
  Result := NameHash xor AgeHash;
end;

{ TStressTestThread }

constructor TStressTestThread.Create(ASet: TThreadSafeHashSetInteger; 
  AStartValue, AOperations: Integer);
begin
  inherited Create(True);
  FSet := ASet;
  FStartValue := AStartValue;
  FOperations := AOperations;
  FreeOnTerminate := False;
end;

procedure TStressTestThread.Execute;
var
  I: Integer;
  Operation: Integer;
begin
  for I := 0 to FOperations - 1 do
  begin
    Operation := Random(3); // 0=Add, 1=Remove, 2=Contains
    case Operation of
      0: FSet.Add(FStartValue + I);
      1: FSet.Remove(FStartValue + Random(I + 1));
      2: FSet.Contains(FStartValue + Random(I + 1));
    end;
  end;
end;

{ TCollisionThread }

constructor TCollisionThread.Create(ASet: TThreadSafeHashSetString;
  AStartIndex, ACount: Integer);
begin
  inherited Create(True);
  FSet := ASet;
  FStartIndex := AStartIndex;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TCollisionThread.Execute;
var
  I: Integer;
  AddedCount: Integer;
  Key: string;
begin
  try
    AddedCount := 0;
    WriteLn(Format('Thread %d starting, range %d to %d', 
      [ThreadID, FStartIndex, FStartIndex + FCount - 1]));
      
    for I := FStartIndex to FStartIndex + FCount - 1 do
    begin
      // Generate the key using the same pattern as the main test
      Key := Chr(65 + (I mod COLLISION_GROUPS)) + '_Item_' + IntToStr(I);
      
      if FSet.Add(Key) then
        Inc(AddedCount)
      else
        WriteLn(Format('Thread %d failed to add item %d: %s', 
          [ThreadID, I, Key]));
        
      if (I - FStartIndex + 1) mod 1000 = 0 then
        WriteLn(Format('Thread %d processed %d/%d items, successfully added: %d', 
          [ThreadID, I - FStartIndex + 1, FCount, AddedCount]));
    end;
    
    WriteLn(Format('Thread %d completed. Added %d/%d items successfully', 
      [ThreadID, AddedCount, FCount]));
  except
    on E: Exception do
      WriteLn(Format('Thread %d error: %s at item %d', [ThreadID, E.Message, I]));
  end;
end;

{ TThreadSafeHashSetTest }

procedure TThreadSafeHashSetTest.Log(const Msg: string);
begin
  WriteLn(FormatDateTime('hh:nn:ss.zzz', Now) + ' - ' + Msg);
  Flush(Output);
end;

procedure TThreadSafeHashSetTest.SetUp;
begin
  Log('SetUp starting...');
  FIntSet := TThreadSafeHashSetInteger.Create;
  FStrSet := TThreadSafeHashSetString.Create;
  FBoolSet := TThreadSafeHashSetBoolean.Create;
  FRealSet := TThreadSafeHashSetReal.Create;
  FStudentSet := specialize TThreadSafeHashSet<TStudent>.Create(@StudentEquals, @StudentHash);
  Log('SetUp completed');
end;

procedure TThreadSafeHashSetTest.TearDown;
begin
  Log('TearDown starting...');
  FIntSet.Free;
  FStrSet.Free;
  FBoolSet.Free;
  FRealSet.Free;
  FStudentSet.Free;
  Log('TearDown completed');
end;

procedure TThreadSafeHashSetTest.Test1_BasicOperations;
begin
  Log('Test1_BasicOperations starting...');
  AssertTrue('Should add new integer', FIntSet.Add(42));
  AssertTrue('Should contain added integer', FIntSet.Contains(42));
  Log('Test1_BasicOperations completed');
end;

procedure TThreadSafeHashSetTest.Test2_SimpleAdd;
begin
  Log('Test2_SimpleAdd starting...');
  AssertTrue('Should add new string', FStrSet.Add('test'));
  AssertTrue('Should contain added string', FStrSet.Contains('test'));
  Log('Test2_SimpleAdd completed');
end;

procedure TThreadSafeHashSetTest.Test3_SimpleRemove;
begin
  Log('Test3_SimpleRemove starting...');
  FIntSet.Add(42);
  AssertTrue('Should remove existing item', FIntSet.Remove(42));
  AssertFalse('Should not contain removed item', FIntSet.Contains(42));
  Log('Test3_SimpleRemove completed');
end;

procedure TThreadSafeHashSetTest.Test4_Duplicates;
begin
  Log('Test4_Duplicates starting...');
  
  // Test integer duplicates
  AssertTrue('First add should succeed', FIntSet.Add(42));
  AssertFalse('Duplicate add should fail', FIntSet.Add(42));
  AssertEquals('Count should be 1', 1, FIntSet.Count);
  
  // Test string duplicates
  AssertTrue('First string add should succeed', FStrSet.Add('test'));
  AssertFalse('Duplicate string add should fail', FStrSet.Add('test'));
  AssertEquals('String count should be 1', 1, FStrSet.Count);
  
  Log('Test4_Duplicates completed');
end;

procedure TThreadSafeHashSetTest.Test5_Clear;
var
  I: Integer;
begin
  Log('Test5_Clear starting...');
  
  // Add some items
  for I := 1 to 5 do
  begin
    FIntSet.Add(I);
    FStrSet.Add('String' + IntToStr(I));
  end;
  
  AssertEquals('Int set should have 5 items', 5, FIntSet.Count);
  AssertEquals('String set should have 5 items', 5, FStrSet.Count);
  
  // Clear sets
  FIntSet.Clear;
  FStrSet.Clear;
  
  AssertEquals('Int set should be empty', 0, FIntSet.Count);
  AssertEquals('String set should be empty', 0, FStrSet.Count);
  
  Log('Test5_Clear completed');
end;

procedure TThreadSafeHashSetTest.Test6_StudentBasic;
var
  Student1, Student2: TStudent;
begin
  Log('Test6_StudentBasic starting...');
  
  // Create test students
  Student1.Name := 'John';
  Student1.Age := 20; 
  
  Student2.Name := 'Jane';
  Student2.Age := 22;
  
  // Test basic operations
  AssertTrue('Should add first student', FStudentSet.Add(Student1));
  AssertTrue('Should add second student', FStudentSet.Add(Student2));
  AssertTrue('Should contain first student', FStudentSet.Contains(Student1));
  AssertTrue('Should contain second student', FStudentSet.Contains(Student2));
  AssertEquals('Should have 2 students', 2, FStudentSet.Count);
  
  // Test removal
  AssertTrue('Should remove first student', FStudentSet.Remove(Student1));
  AssertFalse('Should not contain removed student', FStudentSet.Contains(Student1));
  AssertEquals('Should have 1 student', 1, FStudentSet.Count);
  
  Log('Test6_StudentBasic completed');
end;

procedure TThreadSafeHashSetTest.Test7_LargeDataSet;
const
  LARGE_SET_SIZE = 10000;
  BATCH_SIZE = 100;
var
  I, Batch: Integer;
  StartTick, BatchStartTick, EndTick: QWord;
  ItemToCheck: Integer;
  TotalTime: QWord;
begin
  Log('Test7_LargeDataSet starting...');
  StartTick := GetTickCount64;
  TotalTime := 0;

  try
    // Add items in smaller batches
    for Batch := 0 to (LARGE_SET_SIZE div BATCH_SIZE) - 1 do
    begin
      BatchStartTick := GetTickCount64;
      
      for I := 1 to BATCH_SIZE do
      begin
        if not FIntSet.Add(Batch * BATCH_SIZE + I) then
          Log(Format('Failed to add item %d', [Batch * BATCH_SIZE + I]));
      end;
      
      EndTick := GetTickCount64;
      TotalTime := TotalTime + (EndTick - BatchStartTick);
      
      // Log progress every batch with timing
      Log(Format('Added batch %d/%d (%d items total) - Batch time: %d ms, Total time: %d ms', 
        [Batch + 1, LARGE_SET_SIZE div BATCH_SIZE, FIntSet.Count, 
         EndTick - BatchStartTick, TotalTime]));
        
      // Force a small delay between batches to see timing
      Sleep(1);
    end;

    EndTick := GetTickCount64;
    Log(Format('Total time for adding %d items: %d ms', [LARGE_SET_SIZE, EndTick - StartTick]));
    Log(Format('Actual processing time (excluding logging): %d ms', [TotalTime]));
    
    // Add verification step
    StartTick := GetTickCount64;
    for I := 1 to LARGE_SET_SIZE do
    begin
      if not FIntSet.Contains(I) then
        Log(Format('VERIFICATION FAILED: Missing item %d', [I]));
    end;
    EndTick := GetTickCount64;
    Log(Format('Verification of %d items took: %d ms', [LARGE_SET_SIZE, EndTick - StartTick]));
  except
    on E: Exception do
    begin
      Log(Format('Error in Test7_LargeDataSet: %s', [E.Message]));
      raise;
    end;
  end;
end;

procedure TThreadSafeHashSetTest.Test8_ConcurrentAccess;
const
  THREAD_COUNT = 4;
  OPS_PER_THREAD = 10000;  // Increased from 1000
var
  Threads: array[0..THREAD_COUNT-1] of TStressTestThread;
  I: Integer;
  StartTick, EndTick: QWord;
begin
  Log('Test8_ConcurrentAccess starting...');
  StartTick := GetTickCount64;

  // Create threads
  for I := 0 to THREAD_COUNT-1 do
  begin
    Threads[I] := TStressTestThread.Create(FIntSet, I * OPS_PER_THREAD, OPS_PER_THREAD);
    Log(Format('Created thread %d', [I]));
  end;

  Sleep(100); // Give a moment to ensure threads are ready

  try
    // Start all threads
    for I := 0 to THREAD_COUNT-1 do
    begin
      Threads[I].Start;
      Log(Format('Started thread %d at %d ms', [I, GetTickCount64 - StartTick]));
    end;

    // Wait for all threads
    for I := 0 to THREAD_COUNT-1 do
    begin
      Threads[I].WaitFor;
      EndTick := GetTickCount64;
      Log(Format('Thread %d completed at %d ms', [I, EndTick - StartTick]));
    end;

  finally
    // Cleanup
    for I := 0 to THREAD_COUNT-1 do
      Threads[I].Free;
  end;

  EndTick := GetTickCount64;
  Log(Format('Concurrent test total time: %d ms', [EndTick - StartTick]));
  Log(Format('Final item count: %d', [FIntSet.Count]));
end;

procedure TThreadSafeHashSetTest.Test9_StressTest;
const
  STRESS_ITERATIONS = 100000;
var
  I: Integer;
  StartTick, EndTick: QWord;
  Student: TStudent;
  Value: Integer;
begin
  Log('Test9_StressTest starting...');
  StartTick := GetTickCount64;

  // Mix of operations with forced work
  for I := 1 to STRESS_ITERATIONS do
  begin
    case I mod 4 of
      0: begin // Integer operations with verification
           Value := I * 17; // Use prime number to avoid patterns
           FIntSet.Add(Value);
           if not FIntSet.Contains(Value) then
             Log(Format('Failed to find added integer %d', [Value]));
         end;
      1: begin // String operations with concatenation
           FStrSet.Add('Prefix_' + IntToStr(I) + '_Suffix');
           if I > 1 then
             FStrSet.Remove('Prefix_' + IntToStr(I-1) + '_Suffix');
         end;
      2: begin // Boolean operations with alternating patterns
           FBoolSet.Add((I and 3) = 0);
           FBoolSet.Contains((I and 7) = 0);
           FBoolSet.Remove((I and 15) = 0);
         end;
      3: begin // Student operations with unique combinations
           Student.Name := 'Student_' + IntToStr(I div 100) + '_Group_' + IntToStr(I mod 100);
           Student.Age := 18 + (I mod 50);
           FStudentSet.Add(Student);
           if not FStudentSet.Contains(Student) then
             Log('Failed to find added student');
         end;
    end;

    if I mod 10000 = 0 then
    begin
      EndTick := GetTickCount64;
      Log(Format('Completed %d/%d iterations (%d%%) - Time: %d ms', 
        [I, STRESS_ITERATIONS, (I * 100) div STRESS_ITERATIONS, EndTick - StartTick]));
      Sleep(1); // Allow other processes some time
    end;
  end;

  EndTick := GetTickCount64;
  Log(Format('Stress test completed in %d ms', [EndTick - StartTick]));
  Log(Format('Final counts - Int: %d, Str: %d, Bool: %d, Student: %d',
    [FIntSet.Count, FStrSet.Count, FBoolSet.Count, FStudentSet.Count]));
end;



procedure TThreadSafeHashSetTest.Test10_HashCollisions;
const
  COLLISION_COUNT = 10000;
  COLLISION_GROUPS = 4;
  MAX_DETAILED_LOGS = 10;
var
  I, J, Group: Integer;
  CollisionKeys: array of string;
  StartTick, EndTick: QWord;
  TotalLostKeys, InitialCount: Integer;
  Hash1, Hash2: Cardinal;
  TestSet: TCollisionHashSet;
begin
  StartTick := GetTickCount64;
  TotalLostKeys := 0;
  SetLength(CollisionKeys, COLLISION_COUNT);
  
  WriteLn('=== Starting Hash Collision Test ===');
  
  // Create keys that will definitely collide
  for I := 0 to COLLISION_COUNT-1 do
  begin
    Group := I mod COLLISION_GROUPS;
    // Now we'll create more collisions by using just a few starting letters
    CollisionKeys[I] := Chr(65 + (Group mod 4)) + '_Item_' + IntToStr(I);
  end;
  
  // Verify collisions before adding items
  WriteLn('Verifying hash collisions in first group:');
  Hash1 := ForceCollisionHash(CollisionKeys[0]);
  WriteLn(Format('First item (Group 0): "%s" -> Hash: %.8x', 
    [CollisionKeys[0], Hash1]));
    
  Hash2 := ForceCollisionHash(CollisionKeys[COLLISION_GROUPS]);
  WriteLn(Format('Second item (Group 0): "%s" -> Hash: %.8x', 
    [CollisionKeys[COLLISION_GROUPS], Hash2]));
    
  if Hash1 = Hash2 then
    WriteLn('SUCCESS: Hash collision confirmed!')
  else
    WriteLn('WARNING: No hash collision detected!');
  
  // Create a new set with our collision-forcing hash function
  FStrSet.Free;
  TestSet := TCollisionHashSet.Create;
  FStrSet := TestSet;
  
  WriteLn('Adding items with forced collisions...');
  // Add all items
  for I := 0 to COLLISION_COUNT-1 do
  begin
    if not FStrSet.Add(CollisionKeys[I]) then
      WriteLn(Format('Failed to add item %d: %s', [I, CollisionKeys[I]]));
      
    if (I + 1) mod 1000 = 0 then
      WriteLn(Format('Added %d/%d items...', [I + 1, COLLISION_COUNT]));
  end;
  
  // Verify initial count
  InitialCount := FStrSet.Count;
  WriteLn(Format('Initial set count: %d', [InitialCount]));
  AssertEquals('Initial count mismatch', COLLISION_COUNT, InitialCount);
  
  WriteLn('Starting deep verification...');
  for J := 0 to COLLISION_COUNT-1 do
  begin
    if not FStrSet.Contains(CollisionKeys[J]) then
    begin
      if TotalLostKeys < MAX_DETAILED_LOGS then
        WriteLn(Format('Deep verify: Lost key from group %d: %s', 
          [J mod COLLISION_GROUPS, CollisionKeys[J]]))
      else if TotalLostKeys = MAX_DETAILED_LOGS then
        WriteLn('... additional lost keys omitted ... please wait ...');
        
      Inc(TotalLostKeys);
    end;
    
    if (J + 1) mod 1000 = 0 then
      WriteLn(Format('Verified %d/%d items...', [J + 1, COLLISION_COUNT]));
  end;

  WriteLn(Format('Deep verify summary: Total lost keys: %d', [TotalLostKeys]));
  WriteLn(Format('Final set count: %d', [FStrSet.Count]));
  
  EndTick := GetTickCount64;
  WriteLn(Format('Total test time: %d ms', [EndTick - StartTick]));
  WriteLn('=== Hash Collision Test Complete ===');
  
  AssertEquals('Lost keys detected', 0, TotalLostKeys);
  AssertEquals('Final count mismatch', COLLISION_COUNT, FStrSet.Count);
end;


{
  Test11_AggressiveCollisions is designed to test the absolute worst-case 
  scenario for a hash set using a large dataset. 
  1. Forces max Hash Collisions 

        ```
        function ForceCollisionHash(const Value: string): Cardinal;
        begin
          Result := $DEADBEEF;  // Every single item hashes to the same value
        end;
        ```

  2. Concurrent access under collisions

      ```
      // Creates 4 threads, each adding 25,000 items
      for I := 0 to THREAD_COUNT-1 do
      begin
          Threads[I] := TCollisionThread.Create(FStrSet,
            I * ItemsPerThread, ItemsPerThread);
      end;
      ```

  3. Verifies data integrity

      ```
      // Checks that all 100,000 items were actually added
      for J := 0 to COLLISION_COUNT-1 do
      begin
        if not FStrSet.Contains(CollisionKeys[J]) then
          Inc(TotalLostKeys);
      end;
      ``` 
}
procedure TThreadSafeHashSetTest.Test11_AggressiveCollisions;
const
  COLLISION_COUNT = 100000;
  COLLISION_GROUPS = 2;
  MAX_DETAILED_LOGS = 10;
  THREAD_COUNT = 4;
  BATCH_SIZE = 1000;
var
  I, J, Group: Integer;
  CollisionKeys: array of string;
  StartTick, EndTick, BatchStart: QWord;
  TotalLostKeys, InitialCount: Integer;
  Hash1, Hash2: Cardinal;
  TestSet: TCollisionHashSet;
  Threads: array[0..THREAD_COUNT-1] of TCollisionThread;
  ItemsPerThread: Integer;
begin
  StartTick := GetTickCount64;
  TotalLostKeys := 0;
  SetLength(CollisionKeys, COLLISION_COUNT);
  
  WriteLn('=== Starting Aggressive Collision Test ===');
  WriteLn(Format('Creating %d items with %d threads', [COLLISION_COUNT, THREAD_COUNT]));
  
  // Create keys that will definitely collide
  for I := 0 to COLLISION_COUNT-1 do
  begin
    Group := I mod COLLISION_GROUPS;
    CollisionKeys[I] := Chr(65 + Group) + '_Item_' + IntToStr(I);
  end;
  
  // Verify collisions
  WriteLn('Verifying hash collisions:');
  Hash1 := ForceCollisionHash(CollisionKeys[0]);
  Hash2 := ForceCollisionHash(CollisionKeys[1]);
  WriteLn(Format('Group A hash: %.8x, Group B hash: %.8x', [Hash1, Hash2]));
  AssertEquals('Hashes should be equal for collision test', Hash1, Hash2);
  
  // Create a new set with our collision-forcing hash function
  FStrSet.Free;
  TestSet := TCollisionHashSet.Create;
  FStrSet := TestSet;
  
  WriteLn('Adding items with forced collisions using multiple threads...');
  
  ItemsPerThread := COLLISION_COUNT div THREAD_COUNT;
  
  // Create and start threads
  try
    BatchStart := GetTickCount64;
    
    // Create threads
    for I := 0 to THREAD_COUNT-1 do
    begin
      WriteLn(Format('Creating thread %d for range %d to %d', 
        [I, I * ItemsPerThread, (I + 1) * ItemsPerThread - 1]));
        
      Threads[I] := TCollisionThread.Create(FStrSet,
        I * ItemsPerThread, ItemsPerThread);
    end;
    
    // Start all threads
    WriteLn('Starting all threads...');
    for I := 0 to THREAD_COUNT-1 do
      Threads[I].Start;
      
    // Wait for all threads
    WriteLn('Waiting for threads to complete...');
    for I := 0 to THREAD_COUNT-1 do
      Threads[I].WaitFor;
      
    EndTick := GetTickCount64;
    WriteLn(Format('Parallel insertion took: %d ms', [EndTick - BatchStart]));
  finally
    WriteLn('Cleaning up threads...');
    for I := 0 to THREAD_COUNT-1 do
      Threads[I].Free;
  end;
  
  // Verify results
  InitialCount := FStrSet.Count;
  WriteLn(Format('Initial set count: %d', [InitialCount]));
  AssertEquals('Initial count mismatch', COLLISION_COUNT, InitialCount);
  
  WriteLn('Starting deep verification...');
  for J := 0 to COLLISION_COUNT-1 do
  begin
    if not FStrSet.Contains(CollisionKeys[J]) then
    begin
      if TotalLostKeys < MAX_DETAILED_LOGS then
        WriteLn(Format('Deep verify: Lost key from group %d: %s', 
          [J mod COLLISION_GROUPS, CollisionKeys[J]]))
      else if TotalLostKeys = MAX_DETAILED_LOGS then
        WriteLn('... additional lost keys omitted ... please wait ...');
        
      Inc(TotalLostKeys);
    end;
    
    if (J + 1) mod BATCH_SIZE = 0 then
      WriteLn(Format('Verified %d/%d items...', [J + 1, COLLISION_COUNT]));
  end;

  WriteLn(Format('Deep verify summary: Total lost keys: %d', [TotalLostKeys]));
  WriteLn(Format('Final set count: %d', [FStrSet.Count]));
  
  EndTick := GetTickCount64;
  WriteLn(Format('Total test time: %d ms', [EndTick - StartTick]));
  WriteLn('=== Aggressive Collision Test Complete ===');
  
  AssertEquals('Lost keys detected', 0, TotalLostKeys);
  AssertEquals('Final count mismatch', COLLISION_COUNT, FStrSet.Count);
end;

initialization
  RegisterTest(TThreadSafeHashSetTest);
end.
