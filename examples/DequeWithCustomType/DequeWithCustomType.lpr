program DequeWithCustomType;

{$mode objfpc}{$H+}{$J-}
{$modeSwitch advancedrecords}

uses
  ThreadSafeCollections.Deque;

type
  TPerson = record
    Name: string;
    Age: Integer;
    public
    constructor Create(NewName: string; NewAge: Integer);
  end;

constructor TPerson.Create(NewName: string; NewAge: Integer);
begin
  Name := NewName;
  Age := NewAge;
end;

var
  Deque: specialize TThreadSafeDeque<TPerson>;
  Person: TPerson;
begin
  Deque := specialize TThreadSafeDeque<TPerson>.Create;
  try
    // Add items to the front and back
    Deque.PushFront(TPerson.Create('Alice', 30));
    Deque.PushBack(TPerson.Create('Bob', 25));

    // Remove items from the front and back
    if Deque.TryPopFront(Person) then
      WriteLn('Popped from front: ', Person.Name);

    if Deque.TryPopBack(Person) then
      WriteLn('Popped from back: ', Person.Name);
  finally
    Deque.Free;
  end;

  // Pause console
  WriteLn('Press enter to quit');
  ReadLn;
end.
