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
    /// <summary>Returns the number of elements in the collection</summary>
    /// <complexity>O(1)</complexity>
    function GetCount: Integer;

    /// <summary>Checks if the collection is empty</summary>
    /// <complexity>O(1)</complexity>
    function IsEmpty: Boolean;

    /// <summary>Removes all elements from the collection</summary>
    /// <complexity>O(n) where n is the number of elements</complexity>
    procedure Clear;

    /// <summary>Acquires a lock token for thread-safe multi-operation sequences</summary>
    /// <complexity>O(1)</complexity>
    function Lock: ILockToken;

    property Count: Integer read GetCount;
  end;

  { IThreadSafeList - Interface for list operations }
  generic IThreadSafeList<T> = interface(specialize IThreadSafeCollection<T>)
    ['{B1A2C3D4-E5F6-4A3B-8C7D-9E0F1A2B3C4D}']
    /// <summary>Adds an item to the end of the list</summary>
    /// <complexity>O(1) amortized, O(n) worst case during resize</complexity>
    function Add(const Item: T): Integer;

    /// <summary>Removes the element at the specified index</summary>
    /// <complexity>O(n) due to element shifting</complexity>
    procedure Delete(Index: Integer);

    /// <summary>Finds the index of the first occurrence of an item</summary>
    /// <complexity>O(n) linear search</complexity>
    function IndexOf(const Item: T): Integer;

    /// <summary>Returns the first element in the list</summary>
    /// <complexity>O(1)</complexity>
    function First: T;

    /// <summary>Returns the last element in the list</summary>
    /// <complexity>O(1)</complexity>
    function Last: T;

    /// <summary>Sorts the list using QuickSort algorithm</summary>
    /// <complexity>O(n log n) average case, O(n²) worst case</complexity>
    procedure Sort(Ascending: Boolean = True);

    /// <summary>Checks if the list is currently sorted</summary>
    /// <complexity>O(1)</complexity>
    function IsSorted: Boolean;

    /// <summary>Replaces the element at the specified index</summary>
    /// <complexity>O(1)</complexity>
    procedure Replace(Index: Integer; const Item: T);

    /// <summary>Gets the element at the specified index</summary>
    /// <complexity>O(1)</complexity>
    function GetItem(Index: Integer): T;

    /// <summary>Sets the element at the specified index</summary>
    /// <complexity>O(1)</complexity>
    procedure SetItem(Index: Integer; const Value: T);
    
    // Capacity management
    /// <complexity>O(1)</complexity>
    function GetCapacity: Integer;
    /// <complexity>O(n) when resizing</complexity>
    procedure SetCapacity(const Value: Integer);

    // Array operations
    /// <summary>Converts the list to an array</summary>
    /// <complexity>O(n)</complexity>
    function ToArray: specialize TArray<T>;
    /// <summary>Replaces list contents with array values</summary>
    /// <complexity>O(n)</complexity>
    procedure FromArray(const Values: array of T);

    // Range operations
    /// <summary>Adds multiple items from an array</summary>
    /// <complexity>O(m) where m is array length, O(n+m) if resize needed</complexity>
    procedure AddRange(const Values: array of T); overload;
    /// <complexity>O(m) where m is collection size, O(n+m) if resize needed</complexity>
    procedure AddRange(const Collection: specialize IThreadSafeList<T>); overload;
    /// <complexity>O(n+m) due to element shifting</complexity>
    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    /// <complexity>O(n+m) due to element shifting</complexity>
    procedure InsertRange(Index: Integer; const Collection: specialize IThreadSafeList<T>); overload;
    /// <complexity>O(n) due to element shifting</complexity>
    procedure DeleteRange(AIndex, ACount: Integer);

    // Search operations
    /// <complexity>O(n)</complexity>
    function Contains(const Value: T): Boolean;
    /// <complexity>O(n)</complexity>
    function IndexOfItem(const Item: T; StartIndex: Integer): Integer; overload;
    /// <complexity>O(n)</complexity>
    function IndexOfItem(const Item: T; StartIndex, ACount: Integer): Integer; overload;
    /// <complexity>O(n)</complexity>
    function LastIndexOf(const Item: T): Integer; overload;
    /// <complexity>O(n)</complexity>
    function LastIndexOf(const Item: T; StartIndex: Integer): Integer; overload;
    /// <complexity>O(n)</complexity>
    function LastIndexOf(const Item: T; StartIndex, ACount: Integer): Integer; overload;

    // Additional utility methods
    /// <complexity>O(n) due to element shifting</complexity>
    procedure Insert(Index: Integer; const Item: T);
    /// <complexity>O(1)</complexity>
    procedure Exchange(Index1, Index2: Integer);
    /// <complexity>O(n) due to element shifting</complexity>
    procedure MoveItem(CurIndex, NewIndex: Integer);
    /// <complexity>O(n)</complexity>
    procedure Reverse;
    /// <complexity>O(n) search + O(n) deletion</complexity>
    function Extract(const Item: T): T;
    /// <complexity>O(n) due to element shifting</complexity>
    function ExtractAt(Index: Integer): T;
    /// <complexity>O(n) when resizing</complexity>
    procedure TrimExcess;
    
    // Properties
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  { IThreadSafeDeque - Interface for double-ended queue operations }
  generic IThreadSafeDeque<T> = interface(specialize IThreadSafeCollection<T>)
    ['{E4F5D6C7-B8A9-4321-9876-5432DCBA0987}']
    /// <summary>Adds item to the front</summary>
    /// <complexity>O(1) amortized, O(n) during resize</complexity>
    procedure PushFront(const Item: T);
    /// <summary>Adds item to the back</summary>
    /// <complexity>O(1) amortized, O(n) during resize</complexity>
    procedure PushBack(const Item: T);
    /// <summary>Removes and returns item from the front</summary>
    /// <complexity>O(1)</complexity>
    function PopFront: T;
    /// <summary>Removes and returns item from the back</summary>
    /// <complexity>O(1)</complexity>
    function PopBack: T;
    /// <complexity>O(1)</complexity>
    function TryPopFront(out Value: T): Boolean;
    /// <complexity>O(1)</complexity>
    function TryPopBack(out Value: T): Boolean;
    /// <summary>Returns item at front without removing</summary>
    /// <complexity>O(1)</complexity>
    function PeekFront: T;
    /// <summary>Returns item at back without removing</summary>
    /// <complexity>O(1)</complexity>
    function PeekBack: T;
    /// <complexity>O(1)</complexity>
    function TryPeekFront(out Value: T): Boolean;
    /// <complexity>O(1)</complexity>
    function TryPeekBack(out Value: T): Boolean;
    /// <summary>Adds multiple items to the front</summary>
    /// <complexity>O(m) where m is array length, O(n+m) if resize needed</complexity>
    procedure PushRangeFront(const Items: array of T);
    /// <summary>Adds multiple items to the back</summary>
    /// <complexity>O(m) where m is array length, O(n+m) if resize needed</complexity>
    procedure PushRangeBack(const Items: array of T);
  end;

  { IThreadSafeHashSet - Interface for set operations }
  generic IThreadSafeHashSet<T> = interface(specialize IThreadSafeCollection<T>)
    ['{C7B6A5D4-E3F2-4189-9876-543210FEDCBA}']
    /// <summary>Adds an item to the set</summary>
    /// <complexity>O(1) average case, O(n) during resize</complexity>
    function Add(const Item: T): Boolean;
    /// <summary>Removes an item from the set</summary>
    /// <complexity>O(1) average case</complexity>
    function Remove(const Item: T): Boolean;
    /// <summary>Checks if set contains an item</summary>
    /// <complexity>O(1) average case</complexity>
    function Contains(const Item: T): Boolean;
    /// <complexity>O(n)</complexity>
    function ToArray: specialize TArray<T>;

    // New bulk operations
    /// <complexity>O(m) where m is array length, O(n+m) if resize needed</complexity>
    procedure AddRange(const Items: array of T); overload;
    /// <complexity>O(m) where m is collection size, O(n+m) if resize needed</complexity>
    procedure AddRange(const Collection: specialize IThreadSafeHashSet<T>); overload;
    /// <complexity>O(m) average case</complexity>
    function RemoveRange(const Items: array of T): Integer; overload;
    /// <complexity>O(m) average case</complexity>
    function RemoveRange(const Collection: specialize IThreadSafeHashSet<T>): Integer; overload;

    // Additional utility methods
    /// <complexity>O(1) average case</complexity>
    function TryGetValue(const Item: T; out Value: T): Boolean;
    /// <summary>Keeps only items present in both sets</summary>
    /// <complexity>O(n+m) where n and m are set sizes</complexity>
    procedure IntersectWith(const Collection: specialize IThreadSafeHashSet<T>);
    /// <summary>Adds all items from another set</summary>
    /// <complexity>O(m) where m is other set size</complexity>
    procedure UnionWith(const Collection: specialize IThreadSafeHashSet<T>);
    /// <summary>Removes all items in another set</summary>
    /// <complexity>O(m) where m is other set size</complexity>
    procedure ExceptWith(const Collection: specialize IThreadSafeHashSet<T>);
    /// <summary>Checks if sets share any elements</summary>
    /// <complexity>O(min(n,m)) where n and m are set sizes</complexity>
    function Overlaps(const Collection: specialize IThreadSafeHashSet<T>): Boolean;
    /// <summary>Checks if sets contain exactly same elements</summary>
    /// <complexity>O(n) where n is set size</complexity>
    function SetEquals(const Collection: specialize IThreadSafeHashSet<T>): Boolean;
  end;

  { IThreadSafeDictionary - Interface for dictionary operations }
  generic IThreadSafeDictionary<TKey, TValue> = interface
    ['{F1E2D3C4-B5A6-4789-8901-23456789ABCD}']
    /// <summary>Adds a key-value pair</summary>
    /// <complexity>O(1) average case, O(n) during resize</complexity>
    procedure Add(const Key: TKey; const Value: TValue);
    /// <summary>Removes a key-value pair</summary>
    /// <complexity>O(1) average case</complexity>
    function Remove(const Key: TKey): Boolean;
    /// <summary>Gets value for a key if it exists</summary>
    /// <complexity>O(1) average case</complexity>
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    /// <summary>Adds or updates a key-value pair</summary>
    /// <complexity>O(1) average case, O(n) during resize</complexity>
    procedure AddOrSetValue(const Key: TKey; const Value: TValue);
    /// <summary>Checks if dictionary contains a key</summary>
    /// <complexity>O(1) average case</complexity>
    function ContainsKey(const Key: TKey): Boolean;
    /// <complexity>O(1) average case</complexity>
    function GetItem(const Key: TKey): TValue;
    /// <complexity>O(1) average case</complexity>
    procedure SetItem(const Key: TKey; const Value: TValue);
    /// <complexity>O(1)</complexity>
    function GetCount: Integer;
    /// <complexity>O(n)</complexity>
    procedure Clear;
    /// <complexity>O(1)</complexity>
    function Lock: ILockToken;

    /// <summary>Returns all keys as an array</summary>
    /// <complexity>O(n)</complexity>
    function GetKeys: specialize TKeyArray<TKey>;
    /// <summary>Returns all values as an array</summary>
    /// <complexity>O(n)</complexity>
    function GetValues: specialize TValueArray<TValue>;
    /// <complexity>O(n) when resizing</complexity>
    procedure TrimExcess;
    /// <complexity>O(1) average case</complexity>
    function TryAdd(const Key: TKey; const Value: TValue): Boolean;
    /// <complexity>O(m) where m is dictionary size</complexity>
    procedure AddRange(const ADictionary: specialize IThreadSafeDictionary<TKey, TValue>); overload;
    /// <complexity>O(m) where m is array length</complexity>
    procedure AddRange(const AArray: specialize TPairArray<TKey, TValue>); overload;
    /// <complexity>O(n)</complexity>
    function ToArray: specialize TPairArray<TKey, TValue>;
    /// <summary>Checks if dictionary contains a value (scans all entries)</summary>
    /// <complexity>O(n)</complexity>
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
