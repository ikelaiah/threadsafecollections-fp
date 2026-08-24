unit ThreadSafeCollections.GenericsMixedTests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Generics.Collections,
  Generics.Defaults,
  ThreadSafeCollections.HashSet,
  ThreadSafeCollections.Dictionary;

type
  TStringSet = specialize TThreadSafeHashSet<string>;
  TStringDict = specialize TThreadSafeDictionary<string, Integer>;
  TStringList = specialize TList<string>;
  TStringComparer = specialize TComparer<string>;
  TStringIComparer = specialize IComparer<string>;

  TThreadSafeGenericsMixedTests = class(TTestCase)
  published
    procedure TestMixedGenericsBindToTheCorrectUnits;
  end;

implementation

function StringEquals(const A, B: string): Boolean;
begin
  Result := A = B;
end;

function StringHash(const Value: string): Cardinal;
begin
  Result := Length(Value) * 2654435761 + Byte(Value[1]);
end;

procedure TThreadSafeGenericsMixedTests.TestMixedGenericsBindToTheCorrectUnits;
var
  SetA: TStringSet;
  Dict: TStringDict;
  StdList: TStringList;
  Comparer: TStringIComparer;
  FoundIndex: Int64;
begin
  // This unit intentionally imports Generics.Defaults (and its
  // TEqualityComparer<T> class) together with ThreadSafeCollections.HashSet.
  // The HashSet's own comparer name, THashSetEqualityComparer<T>, must keep
  // binding to the function type and not collide with the RTL class.
  SetA := TStringSet.Create(@StringEquals, @StringHash);
  Dict := TStringDict.Create;
  Comparer := TStringComparer.Default;
  StdList := TStringList.Create(Comparer);
  try
    SetA.Add('a');
    SetA.Add('b');
    Dict.Add('a', 1);
    StdList.Add('x');
    StdList.Add('y');

    AssertEquals('HashSet must work beside the Dictionary', 2, SetA.Count);
    AssertTrue('Dictionary must work beside the HashSet', Dict.ContainsKey('a'));
    AssertTrue('RTL list must still bind to Generics.Collections',
      StdList.BinarySearch('x', FoundIndex));
    AssertEquals('Binary search must find the matching position', 0, FoundIndex);

    AssertTrue('HashSet membership must use its own equality',
      SetA.Contains('a'));
    AssertFalse('HashSet membership must reject absent values',
      SetA.Contains('z'));
  finally
    SetA.Free;
    Dict.Free;
    StdList.Free;
  end;
end;

initialization
  RegisterTest(TThreadSafeGenericsMixedTests);

end.