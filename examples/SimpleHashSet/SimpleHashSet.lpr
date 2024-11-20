program SimpleHashSet;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  ThreadSafeCollections.HashSet;

var
  Numbers: TThreadSafeHashSetInteger;
  MoreNumbers: specialize TThreadSafeHashSet<integer>;
  Strings: TThreadSafeHashSetString;
  MoreStrings: specialize TThreadSafeHashSet<string>;
  I: integer;

begin

  // Create a set of integers #1 - Simple Constructor
  WriteLn('Create a set of integers #1 - Simple Constructor');
  Numbers := TThreadSafeHashSetInteger.Create;
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

  // Create a set of integers #2 - Full Constructor
  WriteLn;
  WriteLn('Create a set of integers #2 - Full Constructor');
  MoreNumbers := specialize TThreadSafeHashSet<integer>.Create(
    @IntegerEquals, @IntegerHash);
  try
    // Add some numbers
    WriteLn('Adding numbers to set...');
    for I := 1 to 5 do
    begin
      if MoreNumbers.Add(I) then
        WriteLn('Added: ', I)
      else
        WriteLn('Already exists: ', I);
    end;

    // Try to add duplicate
    if MoreNumbers.Add(1) then
      WriteLn('Added: 1')
    else
      WriteLn('Failed to add duplicate: 1');

    // Check if numbers exist
    WriteLn('Contains 3? ', MoreNumbers.Contains(3));
    WriteLn('Contains 10? ', MoreNumbers.Contains(10));

    // Remove a number
    if MoreNumbers.Remove(3) then
      WriteLn('Removed: 3')
    else
      WriteLn('Failed to remove: 3');

    WriteLn('Final count: ', MoreNumbers.Count);
  finally
    MoreNumbers.Free;
  end;

  // Create a set of strings #1 - Simple constructor
  WriteLn;
  WriteLn('Create a set of strings #1 - Simple Constructor');
  Strings := TThreadSafeHashSetString.Create;
  try
    Strings.Add('apple');
    Strings.Add('banana');
    Strings.Add('apple');  // Won't be added (duplicate)

    WriteLn('String set count: ', Strings.Count);
    WriteLn('Contains banana? ', Strings.Contains('banana'));
  finally
    Strings.Free;
  end;


  // Create a set of strings #2 - Full Constructor
  WriteLn;
  WriteLn('Create a set of strings #2 - Full Constructor');
  MoreStrings := specialize TThreadSafeHashSet<string>.Create(@StringEquals,
    @StringHash);
  try
    MoreStrings.Add('apple');
    MoreStrings.Add('banana');
    MoreStrings.Add('apple');  // Won't be added (duplicate)

    WriteLn('String set count: ', MoreStrings.Count);
    WriteLn('Contains banana? ', MoreStrings.Contains('banana'));
  finally
    MoreStrings.Free;
  end;

  WriteLn('Press Enter to exit...');
  ReadLn;
end.
