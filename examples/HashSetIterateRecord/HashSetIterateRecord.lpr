program HashSetIterateRecord;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, ThreadSafeCollections.HashSet;

type
  TStudent = record
    name: string;
    age: integer;
  end;

  TThreadSafeStudentSet = specialize TThreadSafeHashSet<TStudent>;

  // Set equality comparator for the type
  function StudentEquals(const Left, Right: TStudent): Boolean;
  begin
    Result := (Left.name = Right.name) and (Left.age = Right.age);
  end;

  // Set a hash function to use fo the type
  function StudentHash(const Student: TStudent): DWord;
  var
    strHash: DWord;
  begin
    strHash := Hash(Student.name);  // Hash the string
    Result := strHash xor DWord(Student.age);  // Combine with age
  end;

  var
    Students: TThreadSafeStudentSet;
    Student: TStudent;
  begin
    Students := TThreadSafeStudentSet.Create(@StudentEquals, @StudentHash);
    try
      // Add some students
      Student.name := 'John';
      Student.age := 20;
      Students.Add(Student);

      Student.name := 'Alice';
      Student.age := 22;
      Students.Add(Student);

      Student.name := 'Bob';
      Student.age := 21;
      Students.Add(Student);

      // Thread-safe iteration using for..in
      for Student in Students do
      begin
        WriteLn('Student Name: ', Student.name, ', Age: ', Student.age);
      end;

    finally
      Students.Free;
    end;

  WriteLn('Press enter to quit ...');
  ReadLn;
end.
