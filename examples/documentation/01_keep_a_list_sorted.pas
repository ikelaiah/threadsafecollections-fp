program KeepListSorted;

{$mode objfpc}{$H+}

uses
  ThreadSafeCollections.List;

var
  Numbers: specialize TThreadSafeList<Integer>;
  Number: Integer;
begin
  Numbers := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    Numbers.AddRange([30, 10, 20]);
    Numbers.Sort;

    for Number in Numbers do
      Writeln(Number);

    if Numbers.IndexOf(20) >= 0 then
      Writeln('found');
  finally
    Numbers.Free;
  end;
end.
