program PackageSmokeConsumer;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  ThreadSafeCollections.List,
  ThreadSafeCollections.Deque,
  ThreadSafeCollections.Dictionary,
  ThreadSafeCollections.HashSet;

function IntegerComparer(const A, B: Integer): Integer;
begin
  if A < B then
    Result := -1
  else if A > B then
    Result := 1
  else
    Result := 0;
end;

var
  Numbers: specialize TThreadSafeList<Integer>;
  Queue: specialize TThreadSafeDeque<Integer>;
  Lookup: specialize TThreadSafeDictionary<string, Integer>;
  Members: TThreadSafeHashSetInteger;

procedure Check(Condition: Boolean; const What: string);
begin
  if not Condition then
  begin
    WriteLn('FAIL: ' + What);
    Halt(1);
  end;
  WriteLn('OK: ' + What);
end;

begin
  Numbers := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    Numbers.AddRange([30, 10, 20]);
    Numbers.Sort;
    Check(Numbers.Count = 3, 'List count');
    Check((Numbers[0] = 10) and (Numbers[2] = 30), 'List sorted');
  finally
    Numbers.Free;
  end;

  Queue := specialize TThreadSafeDeque<Integer>.Create;
  try
    Queue.PushFront(1);
    Queue.PushBack(2);
    Check(Queue.PopFront = 1, 'Deque front');
    Check(Queue.PopBack = 2, 'Deque back');
  finally
    Queue.Free;
  end;

  Lookup := specialize TThreadSafeDictionary<string, Integer>.Create;
  try
    Lookup.Add('one', 1);
    Lookup.AddOrSetValue('two', 2);
    Check(Lookup.Count = 2, 'Dictionary count');
    Check(Lookup.ContainsKey('two'), 'Dictionary lookup');
  finally
    Lookup.Free;
  end;

  Members := TThreadSafeHashSetInteger.Create;
  try
    Members.Add(1);
    Members.Add(2);
    Members.Add(1);
    Check(Members.Count = 2, 'HashSet unique count');
    Check(Members.Contains(1), 'HashSet contains');
  finally
    Members.Free;
  end;

  WriteLn('Package smoke consumer passed.');
end.
