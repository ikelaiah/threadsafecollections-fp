program SimpleTodoList;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, ThreadSafeCollections.List;

var
  TodoList: specialize TThreadSafeList<string>;
  Command, Task: string;
  I:Integer;
begin
  TodoList := specialize TThreadSafeList<string>.Create(@StringComparer);
  try
    WriteLn('Simple Todo List (enter ''quit'' to exit)');
    WriteLn('Commands: add <task>, list, sort, remove <number>');

    repeat
      Write('> ');
      ReadLn(Command);

      if Copy(Command, 1, 4) = 'add ' then
      begin
        Task := Copy(Command, 5, Length(Command));
        TodoList.Add(Task);
        WriteLn('Task added!');
      end
      else if Command = 'list' then
      begin
        if TodoList.Count = 0 then
          WriteLn('No tasks.')
        else
          for I := 0 to TodoList.Count - 1 do
            WriteLn(Format('%d. %s', [I + 1, TodoList[I]]));
      end
      else if Command = 'sort' then
      begin
        TodoList.Sort;
        WriteLn('Tasks sorted alphabetically!');
      end;
    until Command = 'quit';
  finally
    TodoList.Free;
  end;

  WriteLn('Press enter to quit ...');
  ReadLn;
end.
