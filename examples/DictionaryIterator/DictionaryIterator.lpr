program DictionaryIterator;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  ThreadSafeCollections.Dictionary;

var
  Dict: specialize TThreadSafeDictionary<string, integer>;
  Pair: specialize TPair<string, integer>;
begin
  Dict := specialize TThreadSafeDictionary<string, integer>.Create;
  try
    Dict.Add('one', 1);
    Dict.Add('two', 2);
    Dict.Add('three', 3);

    // Iterate over both keys and values
    for Pair in Dict do
      WriteLn(Format('Key: %s, Value: %d', [Pair.Key, Pair.Value]));
  finally
    Dict.Free;
  end;

  WriteLn('Press enter to quit ...');
  ReadLn;
end.
