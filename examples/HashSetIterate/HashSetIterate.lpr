program HashSetIterate;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, ThreadSafeCollections.HashSet;

var
  HashSet: TThreadSafeHashSetInteger;
  Value: Integer;
begin
  try
    // Create a new integer hash set
    HashSet := TThreadSafeHashSetInteger.Create;
    try
      // Add some numbers
      HashSet.Add(10);
      HashSet.Add(20);
      HashSet.Add(30);
      HashSet.Add(40);
      HashSet.Add(50);

      WriteLn('Iterating through the hash set:');
      // Use the iterator to go through all items
      for Value in HashSet do
      begin
        WriteLn('Value: ', Value);
      end;

      WriteLn('Total items in set: ', HashSet.Count);
    finally
      HashSet.Free;
    end;

  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;

  WriteLn('Press Enter to exit...');
  ReadLn;
end.
