program IterateList;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, ThreadSafeCollections.List;

var
  List: specialize TThreadSafeList<Integer>;
  Number: Integer;
begin
  List := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    // Add some numbers
    List.Add(10);
    List.Add(20);
    List.Add(30);
    List.Add(40);
    List.Add(50);

    // Iterate using for-in
    WriteLn('Iterating over TThreadSafeList:');
    for Number in List do
      Write(Number, ' ');
    WriteLn;

  finally
    List.Free;
  end;

  // Pause console
  WriteLn('Press enter to quit ...');
  ReadLn;
end.
