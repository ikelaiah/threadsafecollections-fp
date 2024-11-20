program TestRunner;

{$mode objfpc}{$H+}{$J-}

uses
  Classes
  , consoletestrunner, testutils, DateUtils, SysUtils
  //, ThreadSafeListTests
  //, ThreadSafeListStudentTests
  //, ThreadSafeDictionaryTests
  , ThreadSafeHashSetTests, testregistry;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  procedure WriteCustomHelp; override;
  end;

procedure TMyTestRunner.WriteCustomHelp;
begin
  writeln('Usage: ');
  writeln('-l or --list to show all available tests');
  writeln('-a or --all to run all tests');
  writeln('-t TestName to run a specific test');
end;


var
  Application: TMyTestRunner;
  i:integer;
begin
  try
    WriteLn('Starting test runner...');
        WriteLn('Time: ' + FormatDateTime('hh:nn:ss.zzz', Now));

        Application := TMyTestRunner.Create(nil);
        Application.Initialize;

        WriteLn('Running tests...');
        Application.Run;

        WriteLn('Tests completed.');
        WriteLn('Time: ' + FormatDateTime('hh:nn:ss.zzz', Now));

  finally
    Application.Free;
  end;
end.
