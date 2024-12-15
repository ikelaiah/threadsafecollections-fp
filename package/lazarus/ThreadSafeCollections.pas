{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ThreadSafeCollections;

{$warn 5023 off : no warning about unused units}
interface

uses
  HashFunctions, ThreadSafeCollections.Deque, 
  ThreadSafeCollections.Dictionary, ThreadSafeCollections.HashSet, 
  ThreadSafeCollections.Interfaces, ThreadSafeCollections.List, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('ThreadSafeCollections', @Register);
end.
