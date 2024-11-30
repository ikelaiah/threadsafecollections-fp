program DictionaryWithCustomType;

{$mode objfpc}{$H+}{$J-}
{$modeSwitch advancedrecords}

uses
  ThreadSafeCollections.Dictionary, HashFunctions, SysUtils;

type
  TPersonKey = record
    FirstName: string;
    LastName: string;
  end;

  // Custom hash function for TPersonKey
  function HashPerson(const Key: TPersonKey): Cardinal;
  begin
    Result := XXHash32(Key.FirstName + '|' + Key.LastName);
  end;

  // Custom equality comparison for TPersonKey
  function ComparePerson(const Left, Right: TPersonKey): Boolean;
  begin
    Result := (Left.FirstName = Right.FirstName) and
              (Left.LastName = Right.LastName);
  end;

var
  Dict: specialize TThreadSafeDictionary<TPersonKey, Integer>;
  Person: TPersonKey;
  Pair: specialize TDictionaryPair<TPersonKey, Integer>;
begin
  Dict := specialize TThreadSafeDictionary<TPersonKey, Integer>.Create(
    @HashPerson,
    @ComparePerson
  );
  try
    // Create and add some people
    Person.FirstName := 'John';
    Person.LastName := 'Doe';
    Dict.Add(Person, 25);

    Person.FirstName := 'Jane';
    Person.LastName := 'Smith';
    Dict.Add(Person, 30);

    // Iterate using for..in loop
    WriteLn('All people in dictionary:');
    for Pair in Dict do
    begin
      WriteLn(Format('%s %s: %d years old',
        [Pair.Key.FirstName,
         Pair.Key.LastName,
         Pair.Value]));
    end;

  finally
    Dict.Free;
  end;

  ReadLn;
end.
