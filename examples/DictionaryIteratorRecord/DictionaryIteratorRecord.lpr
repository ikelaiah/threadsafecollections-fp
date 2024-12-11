program DictionaryIteratorRecord;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

uses
  SysUtils,
  ThreadSafeCollections.Dictionary;

type
  TClient = record
  private
    FName: string;
    FLocation: string;
  public
    constructor Create(const AName, ALocation: string);
    function ToString: string;

    property Name: string read FName write FName;
    property Location: string read FLocation write FLocation;
  end;

  constructor TClient.Create(const AName, ALocation: string);
  begin
    FName := AName;
    FLocation := ALocation;
  end;

  function TClient.ToString: string;
  begin
    Result := Format('%s in %s', [FName, FLocation]);
  end;

var
  Clients: specialize TThreadSafeDictionary<integer, TClient>;
  Pair: specialize TDictionaryPair<integer, TClient>;
  Client: TClient;

begin
  Clients := specialize TThreadSafeDictionary<integer, TClient>.Create;
  try
    // Add some sample clients using constructor
    Clients.Add(1, TClient.Create('John Doe', 'New York'));
    Clients.Add(2, TClient.Create('Jane Smith', 'London'));
    Clients.Add(3, TClient.Create('Bob Johnson', 'Paris'));

    WriteLn('All Clients:');
    WriteLn('-------------------------');

    // Iterate using TPair
    for Pair in Clients do
      WriteLn(Format('ID: %d - %s', [Pair.Key, Pair.Value.ToString]));
    WriteLn;

    // Demonstrate update using properties
    Client := Clients[1];
    Client.Location := 'Los Angeles';  // Using property
    Clients.AddOrSetValue(1, Client);

    WriteLn('After Update:');
    WriteLn('-------------------------');
    for Pair in Clients do
      WriteLn(Format('ID: %d - %s', [Pair.Key, Pair.Value.ToString]));

  finally
    Clients.Free;
  end;

  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
