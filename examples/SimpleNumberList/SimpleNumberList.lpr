program SimpleNumberList;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, ThreadSafeCollections.List;

var
  Numbers: specialize TThreadSafeList<Integer>;
  I: Integer;
begin
  Numbers := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    // Add some random numbers
    for I := 1 to 10 do
      Numbers.Add(Random(100));

    // Show unsorted numbers
    Write('Original numbers: ');
    for I := 0 to Numbers.Count - 1 do
      Write(Numbers[I], ' ');
    WriteLn;

    // Sort and show
    Numbers.Sort;
    Write('Sorted ascending: ');
    for I := 0 to Numbers.Count - 1 do
      Write(Numbers[I], ' ');
    WriteLn;

    // Sort descending and show
    Numbers.Sort(False);
    Write('Sorted descending: ');
    for I := 0 to Numbers.Count - 1 do
      Write(Numbers[I], ' ');
    WriteLn;
  finally
    Numbers.Free;
  end;

  WriteLn('Press enter to quit ...');
  ReadLn;
end.
