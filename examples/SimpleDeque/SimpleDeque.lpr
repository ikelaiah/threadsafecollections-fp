program SimpleDeque;

{$mode objfpc}{$H+}{$J-}
{$modeSwitch advancedrecords}
uses
  ThreadSafeCollections.Deque;

var
  Deque: specialize TThreadSafeDeque<string>;
  Name: string;
begin
  Deque := specialize TThreadSafeDeque<string>.Create;
  try
    // Add items to the front and back
    Deque.PushFront('Obed');
    Deque.PushFront('Jesse');
    Deque.PushBack('David');

    // Remove items from the front and back
    if Deque.TryPopFront(Name) then
      WriteLn('Popped from front: ', Name);

    if Deque.TryPopBack(Name) then
      WriteLn('Popped from back: ', Name);
  finally
    Deque.Free;
  end;

  // Pause console
  WriteLn('Press enter to quit');
  ReadLn;
end.
