unit ThreadSafeCollections.Interfaces;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  { ILockToken - Interface for RAII-style locking }
  ILockToken = interface
    ['{77DD964B-11D5-4C42-9DD7-76B66A47DF7E}']
  end;

  { TLockToken - Implementation of RAII lock pattern }
  TLockToken = class(TInterfacedObject, ILockToken)
  private
    FLock: TCriticalSection;
  public
    constructor Create(ALock: TCriticalSection);
    destructor Destroy; override;
  end;

implementation

{ TLockToken }

constructor TLockToken.Create(ALock: TCriticalSection);
begin
  inherited Create;
  FLock := ALock;
  FLock.Acquire;
end;

destructor TLockToken.Destroy;
begin
  FLock.Release;
  inherited;
end;

end. 