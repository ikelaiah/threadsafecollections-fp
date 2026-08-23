program HashSetAlgebra;

{$mode objfpc}{$H+}

uses
  ThreadSafeCollections.HashSet;

var
  A, B: TThreadSafeHashSetInteger;
begin
  A := TThreadSafeHashSetInteger.Create;
  B := TThreadSafeHashSetInteger.Create;
  try
    A.AddRange([1, 2, 3, 4]);
    B.AddRange([3, 4, 5]);
    A.IntersectWith(B);

    Writeln('count ', A.Count);
    if A.Contains(3) then
      Writeln('3 yes');
    if A.Contains(4) then
      Writeln('4 yes');
    if A.Contains(1) then
      Writeln('1 yes');
  finally
    A.Free;
    B.Free;
  end;
end.
