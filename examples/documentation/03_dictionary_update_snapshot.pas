program DictionaryUpdateSnapshot;

{$mode objfpc}{$H+}

uses
  Generics.Collections,
  ThreadSafeCollections.Dictionary;

var
  Stock: specialize TThreadSafeDictionary<string, integer>;
  Pair: specialize TPair<string, integer>;
begin
  Stock := specialize TThreadSafeDictionary<string, integer>.Create;
  try
    Stock.Add('apples', 3);
    Stock.Add('pears', 5);
    Stock.AddOrSetValue('apples', 4);

    for Pair in Stock do
      if Pair.Key = 'apples' then
        Writeln(Pair.Key, '=', Pair.Value);
  finally
    Stock.Free;
  end;
end.
