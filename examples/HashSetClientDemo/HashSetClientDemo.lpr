program HashSetClientDemo;

{$mode objfpc}{$H+}{$J-}
{$modeSwitch advancedrecords}

uses
  SysUtils, HashFunctions,
  ThreadSafeCollections.HashSet;

type
  TClient = record
    Name: string;
    Phone: string;
    PostCode: string;
    constructor Create(const AName, APhone, APostCode: string);
  end;

{ Custom comparer and hash function for TClient }
function ClientEquals(const A, B: TClient): Boolean;
begin
  Result := (A.Name = B.Name) and 
            (A.Phone = B.Phone) and 
            (A.PostCode = B.PostCode);
end;

function ClientHash(const Value: TClient): Cardinal;
begin
  // Combine hashes of all fields
  Result := XXHash32(Value.Name) xor 
            XXHash32(Value.Phone) xor 
            XXHash32(Value.PostCode);
end;

{ TClient }
constructor TClient.Create(const AName, APhone, APostCode: string);
begin
  Name := AName;
  Phone := APhone;
  PostCode := APostCode;
end;

procedure DemonstrateSingleOperations(Clients: specialize TThreadSafeHashSet<TClient>);
var
  Client: TClient;
  Found: TClient;
begin
  WriteLn('=== Single Operations Demo ===');
  
  // Add operations
  Client := TClient.Create('John Doe', '1234567890', '12345');
  WriteLn('Adding John Doe... Success: ', Clients.Add(Client));
  WriteLn('Adding John Doe again... Success: ', Clients.Add(Client), ' (should be False)');
  
  // Contains operation
  WriteLn('Contains John Doe? ', Clients.Contains(Client));
  
  // TryGetValue operation
  if Clients.TryGetValue(Client, Found) then
    WriteLn('Found client: ', Found.Name);
    
  // Remove operation
  WriteLn('Removing John Doe... Success: ', Clients.Remove(Client));
  WriteLn('Contains John Doe after removal? ', Clients.Contains(Client));
  WriteLn;
end;

procedure DemonstrateBulkOperations(Clients: specialize TThreadSafeHashSet<TClient>);
var
  ClientArray: array of TClient;
  OtherClients: specialize TThreadSafeHashSet<TClient>;
  Client: TClient;
begin
  WriteLn('=== Bulk Operations Demo ===');
  
  // Setup array of clients
  SetLength(ClientArray, 3);
  ClientArray[0] := TClient.Create('Alice Smith', '1111111111', '11111');
  ClientArray[1] := TClient.Create('Bob Jones', '2222222222', '22222');
  ClientArray[2] := TClient.Create('Carol White', '3333333333', '33333');
  
  // AddRange from array
  WriteLn('Adding clients from array...');
  Clients.AddRange(ClientArray);
  WriteLn('Current client count: ', Clients.Count);
  
  // Create another set for bulk operations
  OtherClients := specialize TThreadSafeHashSet<TClient>.Create(@ClientEquals, @ClientHash);
  try
    OtherClients.Add(TClient.Create('Bob Jones', '2222222222', '22222'));
    OtherClients.Add(TClient.Create('David Brown', '4444444444', '44444'));
    
    // AddRange from another set
    WriteLn('Adding clients from another set...');
    Clients.AddRange(OtherClients);
    WriteLn('Current client count: ', Clients.Count);
    
    // RemoveRange
    WriteLn('Removing clients that exist in other set...');
    WriteLn('Removed ', Clients.RemoveRange(OtherClients), ' clients');
    WriteLn('Current client count: ', Clients.Count);
  finally
    OtherClients.Free;
  end;
  WriteLn;
end;

procedure DemonstrateSetOperations(Clients: specialize TThreadSafeHashSet<TClient>);
var
  SetA, SetB: specialize TThreadSafeHashSet<TClient>;
  Client: TClient;
begin
  WriteLn('=== Set Operations Demo ===');
  
  SetA := specialize TThreadSafeHashSet<TClient>.Create(@ClientEquals, @ClientHash);
  SetB := specialize TThreadSafeHashSet<TClient>.Create(@ClientEquals, @ClientHash);
  try
    // Setup SetA
    SetA.Add(TClient.Create('Client1', '1111111111', '11111'));
    SetA.Add(TClient.Create('Client2', '2222222222', '22222'));
    SetA.Add(TClient.Create('Client3', '3333333333', '33333'));
    
    // Setup SetB
    SetB.Add(TClient.Create('Client2', '2222222222', '22222'));
    SetB.Add(TClient.Create('Client3', '3333333333', '33333'));
    SetB.Add(TClient.Create('Client4', '4444444444', '44444'));
    
    // Demonstrate Overlaps
    WriteLn('Sets overlap? ', SetA.Overlaps(SetB));
    
    // Demonstrate SetEquals
    WriteLn('Sets equal? ', SetA.SetEquals(SetB));
    
    // Demonstrate IntersectWith
    WriteLn('Intersecting SetA with SetB...');
    SetA.IntersectWith(SetB);
    WriteLn('SetA count after intersection: ', SetA.Count);
    
    // Demonstrate UnionWith
    WriteLn('Creating union of SetA and SetB...');
    SetA.UnionWith(SetB);
    WriteLn('SetA count after union: ', SetA.Count);
    
    // Demonstrate ExceptWith
    WriteLn('Removing SetB items from SetA...');
    SetA.ExceptWith(SetB);
    WriteLn('SetA count after except: ', SetA.Count);
  finally
    SetA.Free;
    SetB.Free;
  end;
  WriteLn;
end;

procedure DemonstrateIteration(Clients: specialize TThreadSafeHashSet<TClient>);
var
  Client: TClient;
  ClientArray: specialize TArray<TClient>;
begin
  WriteLn('=== Iteration Demo ===');
  
  // Clear and add some test data
  Clients.Clear;
  Clients.Add(TClient.Create('Joe Toro', '1111111111', '11111'));
  Clients.Add(TClient.Create('Jill Toro', '2222222222', '22222'));
  
  // Demonstrate for-in loop
  WriteLn('Using for-in loop:');
  for Client in Clients do
    WriteLn('- ', Client.Name);
    
  // Demonstrate ToArray
  WriteLn('Using ToArray:');
  ClientArray := Clients.ToArray;
  for Client in ClientArray do
    WriteLn('- ', Client.Name);
  WriteLn;
end;

var
  Clients: specialize TThreadSafeHashSet<TClient>;
begin
  try
    // Create the main set
    Clients := specialize TThreadSafeHashSet<TClient>.Create(@ClientEquals, @ClientHash);
    try
      DemonstrateSingleOperations(Clients);
      DemonstrateBulkOperations(Clients);
      DemonstrateSetOperations(Clients);
      DemonstrateIteration(Clients);
      
      WriteLn('All demonstrations completed successfully!');
    finally
      Clients.Free;
    end;
    
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
  
  WriteLn('Press Enter to exit...');
  ReadLn;
end. 
