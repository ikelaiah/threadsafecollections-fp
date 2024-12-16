unit ThreadSafeCollections.Interfaces;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections;

type
  // Define our array types
  generic TKeyArray<T> = array of T;
  generic TValueArray<T> = array of T;
  generic TPairArray<TKey, TValue> = array of specialize TPair<TKey, TValue>;

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
    function IndexOf(const Item: T): Integer;
    function First: T;
    function Last: T;
    procedure Sort(Ascending: Boolean = True);
    function IsSorted: Boolean;
    procedure Replace(Index: Integer; const Item: T);
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
    
    // Capacity management
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
    
    // Array operations
    function ToArray: specialize TArray<T>;
    procedure FromArray(const Values: array of T);
    
    // Range operations
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Collection: specialize IThreadSafeList<T>); overload;
    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    procedure InsertRange(Index: Integer; const Collection: specialize IThreadSafeList<T>); overload;
    procedure DeleteRange(AIndex, ACount: Integer);
    
    // Search operations
    function Contains(const Value: T): Boolean;
    function IndexOfItem(const Item: T; StartIndex: Integer): Integer; overload;
    function IndexOfItem(const Item: T; StartIndex, ACount: Integer): Integer; overload;
    function LastIndexOf(const Item: T): Integer; overload;
    function LastIndexOf(const Item: T; StartIndex: Integer): Integer; overload;
    function LastIndexOf(const Item: T; StartIndex, ACount: Integer): Integer; overload;
    
    // Additional utility methods
    procedure Insert(Index: Integer; const Item: T);
    procedure Exchange(Index1, Index2: Integer);
    procedure MoveItem(CurIndex, NewIndex: Integer);
    procedure Reverse;
    function Extract(const Item: T): T;
    function ExtractAt(Index: Integer): T;
    procedure TrimExcess;
    
    // Properties
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property Capacity: Integer read GetCapacity write SetCapacity;
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
    procedure AddOrSetValue(const Key: TKey; const Value: TValue);
    function ContainsKey(const Key: TKey): Boolean;
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);
    function GetCount: Integer;
    procedure Clear;
    function Lock: ILockToken;
    
    function GetKeys: specialize TKeyArray<TKey>;
    function GetValues: specialize TValueArray<TValue>;
    procedure TrimExcess;
    function TryAdd(const Key: TKey; const Value: TValue): Boolean;
    procedure AddRange(const ADictionary: specialize IThreadSafeDictionary<TKey, TValue>); overload;
    procedure AddRange(const AArray: specialize TPairArray<TKey, TValue>); overload;
    function ToArray: specialize TPairArray<TKey, TValue>;
    function ContainsValue(const Value: TValue): Boolean;
    
    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Integer read GetCount;
  end;

  { TLockToken - Basic implementation of ILockToken
    This class provides a simple mechanism to manage the lifecycle of a lock (critical section).
    When an instance is created, it acquires the provided critical section.
    Upon destruction or when Release is called, it ensures the critical section is properly released.
    This ensures that locks are released even if an exception occurs, preventing potential deadlocks.
  }
  TLockToken = class(TInterfacedObject, ILockToken)
  private
    FLock: TCriticalSection; // The critical section being managed
  public
    { 
      constructor Create(ALock: TCriticalSection);
        // Initializes the TLockToken with a given critical section.
        // Acquires the critical section to ensure exclusive access.
        // Parameters:
        //   ALock - The critical section to manage.
    }
    constructor Create(ALock: TCriticalSection);
    
    { 
      destructor Destroy; override;
        // Destructor ensures that the critical section is released when the TLockToken is destroyed.
        // If Release has not been called manually, the lock is still released to prevent deadlocks.
    }
    destructor Destroy; override;
    
    { 
      procedure Release;
        // Manually releases the critical section.
        // After calling Release, the TLockToken no longer holds the lock and cannot release it again.
        // This is useful when you want to release the lock before the TLockToken is destroyed.
    }
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
