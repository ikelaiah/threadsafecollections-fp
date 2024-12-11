program DictionaryWithCustomType;

{$mode objfpc}{$H+}{$J-}
{$modeSwitch advancedrecords}

uses
  ThreadSafeCollections.Dictionary,
  HashFunctions,
  SysUtils;

type
  TPersonKey = record
    FirstName: string;
    LastName: string;
  public
    constructor Create(NewFirstName, NewLastName: string);
  end;

  constructor TPersonKey.Create(NewFirstName, NewLastName: string);
  begin
    FirstName := NewFirstName;
    LastName := NewLastName;
  end;

  // Custom hash function for TPersonKey
  function HashPerson(const Key: TPersonKey): cardinal;
  begin
    Result := XXHash32(Key.FirstName + '|' + Key.LastName);
  end;

  // Custom equality comparison for TPersonKey
  function ComparePerson(const Left, Right: TPersonKey): boolean;
  begin
    Result := (Left.FirstName = Right.FirstName) and
      (Left.LastName = Right.LastName);
  end;

var
  Dict: specialize TThreadSafeDictionary<TPersonKey, integer>;
  Pair: specialize TDictionaryPair<TPersonKey, integer>;
begin
  Dict := specialize TThreadSafeDictionary<TPersonKey, integer>.Create(
    @HashPerson, @ComparePerson);
  try
    // Create and add some people
    Dict.Add(TPersonKey.create('John','Doe'), 25);
    Dict.Add(TPersonKey.Create('Jane', 'Smith'), 30);

    // Iterate using for..in loop
    WriteLn('All people in dictionary:');
    for Pair in Dict do
    begin
      WriteLn(Format('%s %s: %d years old', [Pair.Key.FirstName,
        Pair.Key.LastName, Pair.Value]));
    end;

  finally
    Dict.Free;
  end;

  WriteLn('Press enter to quit ...');
  ReadLn;
end.
