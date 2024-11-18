unit ThreadSafeListStudentTests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ThreadSafeList;

type
  TStudent = record
    Name: string;
    StudentId: Integer;
  end;

type
  TThreadSafeListStudentTest = class(TTestCase)
  private
    FStudentList: specialize TThreadSafeList<TStudent>;
    const
      STUDENT_SORT_SIZE = 100000;
      NAME_LENGTH = 10;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLargeStudentSortByName;
    procedure TestLargeStudentSortById;
  end;

implementation

function StudentNameComparer(const A, B: TStudent): Integer;
begin
  Result := CompareStr(A.Name, B.Name);
end;

function StudentIdComparer(const A, B: TStudent): Integer;
begin
  Result := A.StudentId - B.StudentId;
end;

procedure TThreadSafeListStudentTest.SetUp;
begin
  FStudentList := specialize TThreadSafeList<TStudent>.Create(@StudentNameComparer);
end;

procedure TThreadSafeListStudentTest.TearDown;
begin
  FStudentList.Free;
end;

procedure TThreadSafeListStudentTest.TestLargeStudentSortByName;
var
  Student: TStudent;
  I: Integer;
  StartTick, EndTick: QWord;
  TempName: string;
  CharSet: string;
  IsSortedCorrectly: Boolean;
begin
  CharSet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  WriteLn('Student Sort By Name Test:');

  // Add students with random names
  for I := 0 to STUDENT_SORT_SIZE - 1 do
  begin
    TempName := '';
    while Length(TempName) < NAME_LENGTH do
      TempName := TempName + CharSet[Random(Length(CharSet)) + 1];
    Student.Name := TempName;
    Student.StudentId := STUDENT_SORT_SIZE - I;  // Reverse order IDs
    FStudentList.Add(Student);
  end;

  // Print first few records before sort
  WriteLn('Before sort (first 3):');
  for I := 0 to 2 do
    WriteLn(Format('Name: %s, ID: %d', [FStudentList[I].Name, FStudentList[I].StudentId]));
  WriteLn;

  StartTick := GetTickCount64;
  FStudentList.Sort;
  EndTick := GetTickCount64;
  WriteLn(Format('Sorting %d students by name took: %d ms', 
    [STUDENT_SORT_SIZE, EndTick - StartTick]));

  // Print first few records after sort
  WriteLn('After sort (first 3):');
  for I := 0 to 2 do
    WriteLn(Format('Name: %s, ID: %d', [FStudentList[I].Name, FStudentList[I].StudentId]));
  WriteLn;

  // Verify sorting
  IsSortedCorrectly := True;
  for I := 1 to FStudentList.Count - 1 do
    if CompareStr(FStudentList[I-1].Name, FStudentList[I].Name) > 0 then
    begin
      WriteLn(Format('Sort error at position %d: %s > %s',
        [I, FStudentList[I-1].Name, FStudentList[I].Name]));
      IsSortedCorrectly := False;
      Break;
    end;

  AssertTrue('Student list should be properly sorted by name', IsSortedCorrectly);
  WriteLn('Name sort verification: ', IsSortedCorrectly);
  WriteLn;
end;

procedure TThreadSafeListStudentTest.TestLargeStudentSortById;
var
  Student: TStudent;
  I: Integer;
  StartTick, EndTick: QWord;
  TempList: specialize TThreadSafeList<TStudent>;
  IsSortedCorrectly: Boolean;
begin
  TempList := specialize TThreadSafeList<TStudent>.Create(@StudentIdComparer);
  WriteLn('Student Sort By ID Test:');

  // Add students with random IDs
  for I := 0 to STUDENT_SORT_SIZE - 1 do
  begin
    Student.Name := Format('Student%d', [I]);
    Student.StudentId := Random(STUDENT_SORT_SIZE * 2);  // Random IDs
    TempList.Add(Student);
  end;

  // Print first few records before sort
  WriteLn('Before sort (first 3):');
  for I := 0 to 2 do
    WriteLn(Format('ID: %d, Name: %s', [TempList[I].StudentId, TempList[I].Name]));
  WriteLn;

  StartTick := GetTickCount64;
  TempList.Sort;
  EndTick := GetTickCount64;
  WriteLn(Format('Sorting %d students by ID took: %d ms', 
    [STUDENT_SORT_SIZE, EndTick - StartTick]));

  // Print first few records after sort
  WriteLn('After sort (first 3):');
  for I := 0 to 2 do
    WriteLn(Format('ID: %d, Name: %s', [TempList[I].StudentId, TempList[I].Name]));
  WriteLn;

  // Verify sorting
  IsSortedCorrectly := True;
  for I := 1 to TempList.Count - 1 do
    if TempList[I-1].StudentId > TempList[I].StudentId then
    begin
      WriteLn(Format('Sort error at position %d: %d > %d',
        [I, TempList[I-1].StudentId, TempList[I].StudentId]));
      IsSortedCorrectly := False;
      Break;
    end;

  AssertTrue('Student list should be properly sorted by ID', IsSortedCorrectly);
  WriteLn('ID sort verification: ', IsSortedCorrectly);
  WriteLn;

  TempList.Free;
end;

initialization
  RegisterTest(TThreadSafeListStudentTest);
end.
