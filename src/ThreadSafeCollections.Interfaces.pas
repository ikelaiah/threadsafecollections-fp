unit ThreadSafeCollections.Interfaces;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  { ILockToken - Interface for managing thread-safe access }
  ILockToken = interface
    ['{A8F9D6B3-C2E4-4F5D-9E8A-1B7D3F2C8A0D}']
    procedure Release;
  end;

  { IThreadSafeCollection - Base interface for all collections }
  generic IThreadSafeCollection<T> = interface
    ['{D7E5C4B2-A1F3-4E2D-8B9C-0D6E5F4A3B2C}']
    function GetCount: Integer;
    function IsEmpty: Boolean;
    procedure Clear;
    function Lock: ILockToken;
    
    property Count: Integer read GetCount;
  end;

  { IThreadSafeList - Interface for list operations }
  generic IThreadSafeList<T> = interface(specialize IThreadSafeCollection<T>)
    ['{B1A2C3D4-E5F6-4A3B-8C7D-9E0F1A2B3C4D}']
    function Add(const Item: T): Integer;
    procedure Delete(Index: Integer);
    function Find(const Item: T): Integer;
    function First: T;
    function Last: T;
    procedure Sort(Ascending: Boolean = True);
    function IsSorted: Boolean;
    procedure Replace(Index: Integer; const Item: T);
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
    
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;

  { IThreadSafeDeque - Interface for double-ended queue operations }
  generic IThreadSafeDeque<T> = interface(specialize IThreadSafeCollection<T>)
    ['{E4F5D6C7-B8A9-4321-9876-5432DCBA0987}']
    procedure PushFront(const Item: T);
    procedure PushBack(const Item: T);
    function PopFront: T;
    function PopBack: T;
    function TryPopFront(out Value: T): Boolean;
    function TryPopBack(out Value: T): Boolean;
    function PeekFront: T;
    function PeekBack: T;
    function TryPeekFront(out Value: T): Boolean;
    function TryPeekBack(out Value: T): Boolean;
    procedure PushRangeFront(const Items: array of T);
    procedure PushRangeBack(const Items: array of T);
  end;

  { IThreadSafeHashSet - Interface for set operations }
  generic IThreadSafeHashSet<T> = interface(specialize IThreadSafeCollection<T>)
    ['{C7B6A5D4-E3F2-4189-9876-543210FEDCBA}']
    function Add(const Item: T): Boolean;
    function Remove(const Item: T): Boolean;
    function Contains(const Item: T): Boolean;
    function ToArray: specialize TArray<T>;
  end;

  { IThreadSafeDictionary - Interface for dictionary operations }
  generic IThreadSafeDictionary<TKey, TValue> = interface
    ['{F1E2D3C4-B5A6-4789-8901-23456789ABCD}']
    procedure Add(const Key: TKey; const Value: TValue);
    function Remove(const Key: TKey): Boolean;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure Replace(const Key: TKey; const Value: TValue);
    function ContainsKey(const Key: TKey): Boolean;
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);
    function GetCount: Integer;
    procedure Clear;
    function Lock: ILockToken;
    
    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Integer read GetCount;
  end;

  { TLockToken - Basic implementation of ILockToken }
  TLockToken = class(TInterfacedObject, ILockToken)
  private
    FLock: TCriticalSection;
  public
    constructor Create(ALock: TCriticalSection);
    destructor Destroy; override;
    procedure Release;
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
  if Assigned(FLock) then
    FLock.Release;
  inherited;
end;

procedure TLockToken.Release;
begin
  if Assigned(FLock) then
  begin
    FLock.Release;
    FLock := nil;
  end;
end;

end. 
