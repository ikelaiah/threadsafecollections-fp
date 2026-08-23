program DequeWorkQueue;

{$mode objfpc}{$H+}

uses
  SysUtils,
  ThreadSafeCollections.Deque;

var
  Tasks: specialize TThreadSafeDeque<string>;
  Task: string;
begin
  Tasks := specialize TThreadSafeDeque<string>.Create;
  try
    Tasks.PushBack('build');
    Tasks.PushBack('test');
    Tasks.PushBack('ship');

    if Tasks.TryPopFront(Task) then
      Writeln(Task);
    if Tasks.TryPeekFront(Task) then
      Writeln('next: ' + Task);
    Writeln('remaining: ' + IntToStr(Tasks.Count));
  finally
    Tasks.Free;
  end;
end.
