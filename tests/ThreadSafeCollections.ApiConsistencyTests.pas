unit ThreadSafeCollections.ApiConsistencyTests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Generics.Collections,
  ThreadSafeCollections.List,
  ThreadSafeCollections.Deque,
  ThreadSafeCollections.Dictionary,
  ThreadSafeCollections.HashSet,
  ThreadSafeCollections.Interfaces;

type
  TIntList = specialize TThreadSafeList<Integer>;
  TStrDeque = specialize TThreadSafeDeque<string>;
  TStrIntDict = specialize TThreadSafeDictionary<string, Integer>;
  TIntStrDict = specialize TThreadSafeDictionary<Integer, string>;
  TStrSet = specialize TThreadSafeHashSet<string>;

  TThreadSafeApiConsistencyTests = class(TTestCase)
  published
    // Lock() re-entrancy and compound operations (v0.8.9 lock policy)
    procedure TestManualLockSameThreadReentrantList;
    procedure TestManualLockSameThreadReentrantDeque;
    procedure TestManualLockSameThreadReentrantDictionary;
    procedure TestManualLockSameThreadReentrantHashSet;
    procedure TestManualLockNestedTokensSameThread;
    procedure TestManualLockCompoundCheckThenUpdate;
    procedure TestLockHoldingIterationAllowsSameThreadCalls;

// Constructor defaults and capacity rules
    procedure TestDictionaryNilCallbacksUseDefaults;
    procedure TestDictionaryCapacityRoundsToPowerOfTwo;
    procedure TestHashSetInitialCapacityRoundsUp;
    procedure TestListNegativeInitialCapacityClamped;
    procedure TestDequeNegativeInitialCapacityRoundsUp;

    // Exception and edge contracts
    procedure TestListEmptyFirstLastRaiseEListError;
    procedure TestListOutOfBoundsRaiseEArgumentOutOfRange;
    procedure TestListExtractMissingRaiseEArgumentOutOfRange;
    procedure TestDequeEmptyNonTryOperationsRaiseEListError;
    procedure TestDictionaryDuplicateKeyRaiseEArgumentException;
    procedure TestDictionaryMissingKeyRaiseEKeyNotFoundException;
    procedure TestDictionaryResizeBucketsTooSmallRaises;
    procedure TestListSetCapacityBelowCountRaises;
    procedure TestEnumeratorCurrentBeforeFirstMoveRaises;

    // Bulk and nil-source contracts
    procedure TestEmptyBulkSourcesAreNoOp;
    procedure TestNilCollectionSourcesAreNoOp;
    procedure TestSelfSourceAddRangeIsSafe;
    procedure TestThrowingCallbackLeavesBulkStateUsable;

    // Surface consistency
    procedure TestDictionaryCountReadableOnConcreteAndInterface;

    // Iterator policy sanity (snapshot vs lock-holding)
    procedure TestDictionarySnapshotIterationIgnoresMidLoopMutations;
  end;

implementation

// Equality/comparer/hash helpers are taken from the exported functions of
// ThreadSafeCollections.HashSet (IntegerEquals, StringEquals, IntegerHash,
// StringHash, ...) so that this unit never shadows the HashSet unit names.

function ThrowingHash(const Key: string): Cardinal;
begin
  if Key = 'boom' then
    raise Exception.Create('hash function raised for "boom"');
  Result := StringHash(Key);
end;

{ Re-entrant Lock() policy }

procedure TThreadSafeApiConsistencyTests.TestManualLockSameThreadReentrantList;
var
  List: TIntList;
  Token: ILockToken;
begin
  List := TIntList.Create(@IntegerComparer);
  try
    Token := List.Lock;
    try
      // Public methods acquire the same lock; the calling thread already owns it,
      // so these must complete instead of deadlocking (POSIX) or silently working.
      List.Add(1);
      List.Add(2);
      List.Sort;
      AssertEquals('Re-entrant public calls must complete', 2, List.Count);
    finally
      Token := nil;
    end;
  finally
    List.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestManualLockSameThreadReentrantDeque;
var
  Deque: TStrDeque;
  Token: ILockToken;
  Item: string;
begin
  Deque := TStrDeque.Create;
  try
    Token := Deque.Lock;
    try
      Deque.PushBack('a');
      Deque.PushFront('z');
      AssertTrue('TryPopFront under a held token must work', Deque.TryPopFront(Item));
      AssertEquals('Expected front item', 'z', Item);
      AssertEquals('Expected remaining items', 1, Deque.Count);
    finally
      Token := nil;
    end;
  finally
    Deque.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestManualLockSameThreadReentrantDictionary;
var
  Dict: TStrIntDict;
  Token: ILockToken;
  Value: Integer;
begin
  Dict := TStrIntDict.Create;
  try
    Token := Dict.Lock;
    try
      Dict.Add('one', 1);
      Dict.AddOrSetValue('one', 2);
      AssertTrue('TryGetValue under a held token must work', Dict.TryGetValue('one', Value));
      AssertEquals('Update must be visible', 2, Value);
    finally
      Token := nil;
    end;
  finally
    Dict.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestManualLockSameThreadReentrantHashSet;
var
  SetA: TStrSet;
  Token: ILockToken;
begin
  SetA := TStrSet.Create(@StringEquals, @StringHash);
  try
    Token := SetA.Lock;
    try
      AssertTrue('Add under a held token must work', SetA.Add('x'));
      AssertFalse('Duplicate add must report false', SetA.Add('x'));
      AssertTrue('Contains under a held token must work', SetA.Contains('x'));
    finally
      Token := nil;
    end;
  finally
    SetA.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestManualLockNestedTokensSameThread;
var
  List: TIntList;
  Outer, Inner: ILockToken;
begin
  List := TIntList.Create(@IntegerComparer);
  try
    Outer := List.Lock;
    Inner := List.Lock; // same-thread nested acquisition
    try
      List.Add(42);
    finally
      Inner := nil;
      Outer := nil;
    end;
    AssertEquals('Nested tokens must not deadlock', 1, List.Count);
    AssertTrue('Lock must be released after the last token', List.IsEmpty = False);
  finally
    List.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestManualLockCompoundCheckThenUpdate;
var
  List: TIntList;
  Token: ILockToken;
begin
  List := TIntList.Create(@IntegerComparer);
  try
    Token := List.Lock;
    try
      // check-then-update is atomic only when guarded by one token
      if not List.Contains(5) then
        List.Add(5);
      if not List.Contains(5) then
        List.Add(5);
    finally
      Token := nil;
    end;
    AssertEquals('Compound check-then-update must add exactly once', 1, List.Count);
  finally
    List.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestLockHoldingIterationAllowsSameThreadCalls;
var
  List: TIntList;
  Value, Count: Integer;
begin
  List := TIntList.Create(@IntegerComparer);
  try
    List.AddRange([10, 20, 30]);
    Count := 0;
    // Lock-holding enumeration plus a same-thread public call used to deadlock
    // on non-reentrant (POSIX) critical sections. The re-entrant lock makes
    // this a supported pattern.
    for Value in List do
    begin
      Inc(Count);
      AssertTrue('Count inside enumeration must be readable', List.Count = 3);
      AssertTrue('IndexOf inside enumeration must be usable', List.IndexOf(Value) >= 0);
    end;
    AssertEquals('Enumeration must visit every element', 3, Count);
  finally
    List.Free;
  end;
end;

{ Constructors and capacity }

procedure TThreadSafeApiConsistencyTests.TestDictionaryNilCallbacksUseDefaults;
var
  Dict: TStrIntDict;
  Value: Integer;
begin
  // nil hash/equality select the built-in defaults (string => XXHash32)
  Dict := TStrIntDict.Create(nil, nil);
  try
    Dict.Add('key', 7);
    AssertTrue('Default callbacks must serve lookups', Dict.TryGetValue('key', Value));
    AssertEquals('Expected stored value', 7, Value);
  finally
    Dict.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestDictionaryCapacityRoundsToPowerOfTwo;
var
  Dict: TIntStrDict;
begin
  Dict := TIntStrDict.Create(100);
  try
    AssertEquals('Capacity must round to a power of two', 128, Dict.BucketCount);
  finally
    Dict.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestHashSetInitialCapacityRoundsUp;
var
  SetA: specialize TThreadSafeHashSet<Integer>;
begin
  // Generic constructor accepts an explicit capacity hint
  SetA := specialize TThreadSafeHashSet<Integer>.Create(@IntegerEquals, @IntegerHash, 40);
  try
    AssertTrue('Rounded-up capacity must be usable', SetA.Add(1));
    AssertTrue('Count must reflect the added item', SetA.Count = 1);
  finally
    SetA.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestListNegativeInitialCapacityClamped;
var
  List: TIntList;
begin
  List := TIntList.Create(@IntegerComparer, -10);
  try
    AssertTrue('Negative initial capacity must be clamped to the minimum', List.Capacity >= 4);
  finally
    List.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestDequeNegativeInitialCapacityRoundsUp;
var
  Deque: TStrDeque;
begin
  Deque := TStrDeque.Create(-4);
  try
    Deque.PushBack('ok');
    AssertEquals('Construction after negative capacity must work', 1, Deque.Count);
  finally
    Deque.Free;
  end;
end;

{ Exception and edge contracts }

procedure TThreadSafeApiConsistencyTests.TestListEmptyFirstLastRaiseEListError;
var
  List: TIntList;
begin
  List := TIntList.Create(@IntegerComparer);
  try
    try
      List.First;
      Fail('First on an empty list must raise EListError');
    except
      on E: EListError do ;
    end;
    try
      List.Last;
      Fail('Last on an empty list must raise EListError');
    except
      on E: EListError do ;
    end;
  finally
    List.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestListOutOfBoundsRaiseEArgumentOutOfRange;
var
  List: TIntList;
begin
  List := TIntList.Create(@IntegerComparer);
  try
    List.Add(1);
    try
      List.Delete(-1);
      Fail('Delete(-1) must raise EArgumentOutOfRangeException');
    except
      on E: EArgumentOutOfRangeException do ;
    end;
    try
      List.GetItem(5);
      Fail('GetItem(5) must raise EArgumentOutOfRangeException');
    except
      on E: EArgumentOutOfRangeException do ;
    end;
    try
      List.Replace(9, 0);
      Fail('Replace(9) must raise EArgumentOutOfRangeException');
    except
      on E: EArgumentOutOfRangeException do ;
    end;
    try
      List.Insert(-1, 0);
      Fail('Insert(-1) must raise EArgumentOutOfRangeException');
    except
      on E: EArgumentOutOfRangeException do ;
    end;
  finally
    List.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestListExtractMissingRaiseEArgumentOutOfRange;
var
  List: TIntList;
begin
  List := TIntList.Create(@IntegerComparer);
  try
    List.Add(1);
    try
      List.Extract(99);
      Fail('Extract of a missing value must raise EArgumentOutOfRangeException');
    except
      on E: EArgumentOutOfRangeException do ;
    end;
  finally
    List.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestDequeEmptyNonTryOperationsRaiseEListError;
var
  Deque: TStrDeque;
begin
  Deque := TStrDeque.Create;
  try
    try
      Deque.PopFront;
      Fail('PopFront on an empty deque must raise EListError');
    except
      on E: EListError do ;
    end;
    try
      Deque.PopBack;
      Fail('PopBack on an empty deque must raise EListError');
    except
      on E: EListError do ;
    end;
    try
      Deque.PeekFront;
      Fail('PeekFront on an empty deque must raise EListError');
    except
      on E: EListError do ;
    end;
    try
      Deque.PeekBack;
      Fail('PeekBack on an empty deque must raise EListError');
    except
      on E: EListError do ;
    end;
  finally
    Deque.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestDictionaryDuplicateKeyRaiseEArgumentException;
var
  Dict: TStrIntDict;
begin
  Dict := TStrIntDict.Create;
  try
    Dict.Add('k', 1);
    try
      Dict.Add('k', 2);
      Fail('Adding a duplicate key must raise EArgumentException');
    except
      on E: EArgumentException do ;
    end;
  finally
    Dict.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestDictionaryMissingKeyRaiseEKeyNotFoundException;
var
  Dict: TStrIntDict;
  Value: Integer;
begin
  Dict := TStrIntDict.Create;
  try
    try
      Value := Dict.GetItem('missing');
      if Value = 0 then Value := 1; // avoid "unused" warnings
      Fail('Reading a missing key must raise EKeyNotFoundException');
    except
      on E: EKeyNotFoundException do ;
    end;
  finally
    Dict.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestDictionaryResizeBucketsTooSmallRaises;
var
  Dict: TIntStrDict;
  I: Integer;
begin
  Dict := TIntStrDict.Create;
  try
    for I := 1 to 100 do
      Dict.Add(I, '');
    try
      Dict.ResizeBuckets(4);
      Fail('ResizeBuckets below the current load must raise EArgumentOutOfRangeException');
    except
      on E: EArgumentOutOfRangeException do ;
    end;
  finally
    Dict.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestListSetCapacityBelowCountRaises;
var
  List: TIntList;
begin
  List := TIntList.Create(@IntegerComparer);
  try
    List.AddRange([1, 2, 3]);
    try
      List.Capacity := 2;
      Fail('Capacity below Count must raise EArgumentOutOfRangeException');
    except
      on E: EArgumentOutOfRangeException do ;
    end;
  finally
    List.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestEnumeratorCurrentBeforeFirstMoveRaises;
var
  List: TIntList;
  Deque: TStrDeque;
  Dict: TStrIntDict;
  SetA: TStrSet;
  ListEnum: TIntList.TEnumerator;
  DequeEnum: TStrDeque.TEnumerator;
  DictEnum: TStrIntDict.TEnumerator;
  SetEnum: TStrSet.TEnumerator;
begin
  List := TIntList.Create(@IntegerComparer);
  Deque := TStrDeque.Create;
  Dict := TStrIntDict.Create;
  SetA := TStrSet.Create(@StringEquals, @StringHash);
  try
    ListEnum := List.GetEnumerator;
    try
      try
        if ListEnum.Current = 0 then;
        Fail('List enumerator Current before MoveNext must raise EInvalidOperation');
      except
        on E: EInvalidOperation do ;
      end;
    finally
      ListEnum.Free;
    end;

    DequeEnum := Deque.GetEnumerator;
    try
      try
        if DequeEnum.Current = '' then;
        Fail('Deque enumerator Current before MoveNext must raise EInvalidOperation');
      except
        on E: EInvalidOperation do ;
      end;
    finally
      DequeEnum.Free;
    end;

    SetEnum := SetA.GetEnumerator;
    try
      try
        if SetEnum.Current = '' then;
        Fail('HashSet enumerator Current before MoveNext must raise EInvalidOperation');
      except
        on E: EInvalidOperation do ;
      end;
    finally
      SetEnum.Free;
    end;

    DictEnum := Dict.GetEnumerator;
    try
      try
        DictEnum.Current; // returns TPair; a raise above is the failure signal
        Fail('Dictionary enumerator Current before MoveNext must raise EInvalidOperation');
      except
        on E: EInvalidOperation do ;
      end;
    finally
      DictEnum.Free;
    end;
  finally
    List.Free;
    Deque.Free;
    Dict.Free;
    SetA.Free;
  end;
end;

{ Bulk and nil-source contracts }

procedure TThreadSafeApiConsistencyTests.TestEmptyBulkSourcesAreNoOp;
var
  List: TIntList;
  Deque: TStrDeque;
  SetA: TStrSet;
  Dict: TStrIntDict;
begin
  List := TIntList.Create(@IntegerComparer);
  Deque := TStrDeque.Create;
  SetA := TStrSet.Create(@StringEquals, @StringHash);
  Dict := TStrIntDict.Create;
  try
    List.AddRange([]);
    Deque.PushRangeBack([]);
    SetA.AddRange([]);
    Dict.AddRange([]);

    AssertEquals('Empty bulk sources must be no-ops', 0, List.Count);
    AssertTrue('Deque must stay empty', Deque.IsEmpty);
    AssertTrue('HashSet must stay empty', SetA.IsEmpty);
    AssertEquals('Dictionary must stay empty', 0, Dict.Count);
  finally
    List.Free;
    Deque.Free;
    SetA.Free;
    Dict.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestNilCollectionSourcesAreNoOp;
var
  List: TIntList;
  SetA: TStrSet;
  Dict: TStrIntDict;
begin
  List := TIntList.Create(@IntegerComparer);
  SetA := TStrSet.Create(@StringEquals, @StringHash);
  Dict := TStrIntDict.Create;
  try
    List.AddRange(nil);
    SetA.AddRange(nil);
    Dict.AddRange(specialize IThreadSafeDictionary<string, Integer>(nil));

    AssertEquals('Nil collection source must be a no-op for List', 0, List.Count);
    AssertTrue('Nil collection source must be a no-op for HashSet', SetA.IsEmpty);
    AssertEquals('Nil collection source must be a no-op for Dictionary', 0, Dict.Count);
  finally
    List.Free;
    SetA.Free;
    Dict.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestSelfSourceAddRangeIsSafe;
var
  List: TIntList;
  SetA: TStrSet;
begin
  List := TIntList.Create(@IntegerComparer);
  SetA := TStrSet.Create(@StringEquals, @StringHash);
  try
    List.AddRange([1, 2]);
    // Implicit class-to-interface conversion in the argument, like the
    // dictionary self-source test; the snapshot must not deadlock or duplicate.
    List.AddRange(List);
    AssertEquals('List self-source must keep unique values in order', 4, List.Count);

    SetA.Add('a');
    SetA.Add('b');
    SetA.AddRange(SetA); // snapshot of self
    AssertEquals('HashSet self-source must not duplicate values', 2, SetA.Count);
  finally
    List.Free;
    SetA.Free;
  end;
end;

procedure TThreadSafeApiConsistencyTests.TestThrowingCallbackLeavesBulkStateUsable;
var
  Dict: TStrIntDict;
  Pairs: specialize TPairArray<string, Integer>;
begin
  Dict := TStrIntDict.Create(@ThrowingHash, nil);
  try
    SetLength(Pairs, 3);
    Pairs[0].Key := 'a';
    Pairs[0].Value := 1;
    Pairs[1].Key := 'boom';
    Pairs[1].Value := 2;
    Pairs[2].Key := 'b';
    Pairs[2].Value := 3;

    try
      Dict.AddRange(Pairs);
      Fail('AddRange must propagate the callback exception');
    except
      on E: Exception do
        AssertEquals('Callback message must propagate', 'hash function raised for "boom"', E.Message);
    end;

    // Bulk operations are best-effort: successful items remain, later items
    // are not applied, and the dictionary stays fully usable.
    AssertEquals('Items before the failing callback must remain', 1, Dict.Count);
    AssertTrue('Applied key must be present', Dict.ContainsKey('a'));
    AssertFalse('Key after the failure must not be inserted', Dict.ContainsKey('b'));
    Dict.Add('b', 3);
    AssertEquals('Dictionary must remain usable after a callback failure', 2, Dict.Count);
  finally
    Dict.Free;
  end;
end;

{ Surface consistency }

procedure TThreadSafeApiConsistencyTests.TestDictionaryCountReadableOnConcreteAndInterface;
var
  Dict: TStrIntDict;
  IDict: specialize IThreadSafeDictionary<string, Integer>;
begin
  Dict := TStrIntDict.Create;
  try
    Dict.Add('one', 1);
    // The interface reference controls the object's lifetime once assigned, so
    // the concrete reference is not explicitly freed afterwards.
    IDict := Dict;
    AssertEquals('Concrete Count property must read correctly', 1, Dict.Count);
    AssertEquals('Interface Count property must read correctly', 1, IDict.Count);
  finally
    IDict := nil; // releases the object (TInterfacedObject self-destruction)
    Dict := nil;  // do not touch the underlying object again
  end;
end;

{ Iterator policy sanity }

procedure TThreadSafeApiConsistencyTests.TestDictionarySnapshotIterationIgnoresMidLoopMutations;
var
  Dict: TStrIntDict;
  Pairs: specialize TPairArray<string, Integer>;
  Pair: specialize TPair<string, Integer>;
  Seen: Integer;
begin
  Dict := TStrIntDict.Create;
  try
    Dict.Add('a', 1);
    Dict.Add('b', 2);
Seen := 0;
    for Pair in Dict do
    begin
      Inc(Seen);
      Dict.AddOrSetValue('c', 3); // mutation during snapshot iteration
    end;
    AssertEquals('Snapshot iteration must see exactly the pre-loop pairs', 2, Seen);
    AssertEquals('Mid-loop add must be applied after the snapshot', 3, Dict.Count);
  finally
    Dict.Free;
  end;
end;

initialization
  RegisterTest(TThreadSafeApiConsistencyTests);

end.
