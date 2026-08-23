program LockTokenDemo;

{$mode objfpc}{$H+}

uses
  ThreadSafeCollections.Interfaces,
  ThreadSafeCollections.List;

var
  Pending: specialize TThreadSafeList<string>;
  Token: ILockToken;
begin
  Pending := specialize TThreadSafeList<string>.Create(@StringComparer);
  try
    Token := Pending.Lock;
    try
      Pending.Add('one');
      Pending.Add('two');
      Pending.Replace(1, 'two!');
      Pending.Sort;
    finally
      Token.Release;
    end;
    Writeln(Pending.Count);
  finally
    Pending.Free;
  end;
end.
