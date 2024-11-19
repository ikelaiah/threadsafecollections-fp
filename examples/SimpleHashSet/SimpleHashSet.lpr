program SimpleHashSet;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, ThreadSafeCollections.HashSet;

var
  Numbers: specialize TThreadSafeHashSet<Integer>;
  Strings: specialize TThreadSafeHashSet<string>;
  I: Integer;
begin
  // Create a set of integers
  Numbers := specialize TThreadSafeHashSet<Integer>.Create(@IntegerEquals, @IntegerHash);
  try
    // Add some numbers
    WriteLn('Adding numbers to set...');
    for I := 1 to 5 do
    begin
      if Numbers.Add(I) then
        WriteLn('Added: ', I)
      else
        WriteLn('Already exists: ', I);
    end;

    // Try to add duplicate
    if Numbers.Add(1) then
      WriteLn('Added: 1')
    else
      WriteLn('Failed to add duplicate: 1');

    // Check if numbers exist
    WriteLn('Contains 3? ', Numbers.Contains(3));
    WriteLn('Contains 10? ', Numbers.Contains(10));

    // Remove a number
    if Numbers.Remove(3) then
      WriteLn('Removed: 3')
    else
      WriteLn('Failed to remove: 3');

    WriteLn('Final count: ', Numbers.Count);
  finally
    Numbers.Free;
  end;

  // Create a set of strings
  Strings := specialize TThreadSafeHashSet<string>.Create(@StringEquals, @StringHash);
  try
    Strings.Add('apple');
    Strings.Add('banana');
    Strings.Add('apple');  // Won't be added (duplicate)

    WriteLn('String set count: ', Strings.Count);
    WriteLn('Contains banana? ', Strings.Contains('banana'));
  finally
    Strings.Free;
  end;

  WriteLn('Press Enter to exit...');
  ReadLn;
end.
