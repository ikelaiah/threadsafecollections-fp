program InterfaceTest;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils,
  ThreadSafeCollections.Interfaces,
  ThreadSafeCollections.List,
  ThreadSafeCollections.Deque,
  ThreadSafeCollections.HashSet,
  ThreadSafeCollections.Dictionary;

type
  // Declare specialized types
  TIntegerList = specialize TThreadSafeList<Integer>;
  IIntegerList = specialize IThreadSafeList<Integer>;

  TStringDeque = specialize TThreadSafeDeque<string>;
  IStringDeque = specialize IThreadSafeDeque<string>;

procedure TestList;
var
  List: IIntegerList;
begin
  WriteLn('Testing List...');
  List := TIntegerList.Create(@IntegerComparer);

  List.Add(1);
  List.Add(2);
  List.Add(3);

  WriteLn('Count: ', List.Count);
  WriteLn('First: ', List.First);
  WriteLn('Last: ', List.Last);
end;

procedure TestDeque;
var
  Deque: IStringDeque;
begin
  WriteLn('Testing Deque...');
  Deque := TStringDeque.Create;

  Deque.PushBack('First');
  Deque.PushBack('Second');
  Deque.PushFront('Zero');

  WriteLn('Count: ', Deque.Count);
  WriteLn('Front: ', Deque.PeekFront);
  WriteLn('Back: ', Deque.PeekBack);
end;

begin
  try
    TestList;
    WriteLn;
    TestDeque;

    WriteLn('All tests completed successfully');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ReadLn;
    end;
  end;

  WriteLn('Press enter to quit ...');
  ReadLn;
end.
