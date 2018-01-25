{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2018 by Maciej Izak (hnb),
    member of the Free Pascal development team

    It contains tests for the Free Pascal generics library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Acknowledgment

    Thanks to Sphere 10 Software (http://sphere10.com) for sponsoring
    many new types, tests and major refactoring of entire library

    Thanks to Castle Game Engine (https://castle-engine.sourceforge.io)
    Part of tests for this module was copied from Castle Game Engine tests

 **********************************************************************}

unit tests.generics.stdcollections;

{$mode delphi}

interface

uses
  fpcunit, testutils, testregistry,
  Classes, SysUtils, Generics.Collections, Generics.Defaults;

type

  { TTestStdCollections }

  TTestStdCollections = class(TTestCase)
  published
    // Tests from Castle Game Engine
    procedure Test_List;
    procedure Test_FreeingManually;
    procedure Test_AddingLists;
    procedure Test_Sort;
    procedure Test_Pack;
    procedure Test_RecordsList;
    procedure Test_VectorsList;
    procedure Test_MethodsList;

    // My (c) tests
    procedure Test_SortedList;
    procedure Test_Queue;
  end;

implementation

type
  TApple = class
    Name: string;
  end;

type
  TAppleList = class(TObjectList<TApple>)
    procedure Pack;
  end;

procedure TAppleList.Pack;
begin
  while Remove(nil) <> -1 do ;
end;

procedure TTestStdCollections.Test_List;
var
  A: TApple;
  Apples: TAppleList;
begin
  Apples := TAppleList.Create(true);
  try
    A := TApple.Create;
    Apples.Add(A);
    Apples.Add(TApple.Create);
    A := TApple.Create;
    Apples.Add(A);

    AssertEquals(3, Apples.Count);
    AssertEquals(2, Apples.IndexOf(A));

    Apples.Delete(0);

    AssertEquals(2, Apples.Count);
    AssertEquals(1, Apples.IndexOf(A));

    Apples.Remove(A);

    AssertEquals(1, Apples.Count);

    Apples.Delete(0);

    AssertEquals(0, Apples.Count);
  finally FreeAndNil(Apples) end;
end;

procedure TTestStdCollections.Test_FreeingManually;
var
  A: TApple;
  Apples: TAppleList;
begin
  Apples := TAppleList.Create(false);
  try
    A := TApple.Create;
    Apples.Add(A);
    Apples.Add(A);
    Apples.Add(TApple.Create);

    { This freeing would be invalid on a list that owns children,
      as we free something twice, and we leave some invalid references
      (to already freed items) in the list at various stages.
      But it should be OK with list that has OwnsChildren = false. }

    Apples[0].Free;
    Apples[0] := nil;
    Apples[1] := nil;
    Apples[2].Free;
  finally FreeAndNil(Apples) end;
end;

procedure TTestStdCollections.Test_AddingLists;
var
  A: TApple;
  Apples, Apples2: TAppleList;
begin
  Apples := TAppleList.Create(true);
  try
    A := TApple.Create;
    A.Name := 'One';
    Apples.Add(A);

    A := TApple.Create;
    A.Name := 'Two';
    Apples.Add(A);

    Apples2 := TAppleList.Create(false);
    try
      Apples2.AddRange(Apples);
      Apples2.AddRange(Apples);
      Apples2.AddRange(Apples);
      AssertEquals(6, Apples2.Count);
      AssertEquals('One', Apples2[0].Name);
      AssertEquals('Two', Apples2[1].Name);
      AssertEquals('One', Apples2[2].Name);
      AssertEquals('Two', Apples2[3].Name);
      AssertEquals('One', Apples2[4].Name);
      AssertEquals('Two', Apples2[5].Name);
    finally FreeAndNil(Apples2) end;
  finally FreeAndNil(Apples) end;
end;

function CompareApples(constref Left, Right: TApple): Integer;
begin
  Result := AnsiCompareStr(Left.Name, Right.Name);
end;

procedure TTestStdCollections.Test_Sort;
type
  TAppleComparer = TComparer<TApple>;
var
  A: TApple;
  L: TAppleList;
begin
  L := TAppleList.Create(true);
  try
    A := TApple.Create;
    A.Name := '11';
    L.Add(A);

    A := TApple.Create;
    A.Name := '33';
    L.Add(A);

    A := TApple.Create;
    A.Name := '22';
    L.Add(A);

    L.Sort(TAppleComparer.Construct(@CompareApples));

    AssertEquals(3, L.Count);
    AssertEquals('11', L[0].Name);
    AssertEquals('22', L[1].Name);
    AssertEquals('33', L[2].Name);
  finally FreeAndNil(L) end;
end;

procedure TTestStdCollections.Test_Pack;
var
  A: TApple;
  L: TAppleList;
begin
  L := TAppleList.Create(true);
  try
    L.Add(nil);

    A := TApple.Create;
    A.Name := '11';
    L.Add(A);

    L.Add(nil);

    A := TApple.Create;
    A.Name := '33';
    L.Add(A);

    A := TApple.Create;
    A.Name := '22';
    L.Add(A);

    L.Add(nil);
    L.Add(nil);

    L.Pack;

    AssertEquals(3, L.Count);
    AssertEquals('11', L[0].Name);
    AssertEquals('33', L[1].Name);
    AssertEquals('22', L[2].Name);
  finally FreeAndNil(L) end;
end;

procedure TTestStdCollections.Test_RecordsList;
type
  TMyRecord = packed record
    A, B: Integer;
  end;
  TMyRecordList = TList<TMyRecord>;
var
  List: TMyRecordList;
  R1, R2, R: TMyRecord;
begin
  List := TMyRecordList.Create;
  try
    R1.A := 11;
    R1.B := 22;
    List.Add(R1);

    R2.A := 33;
    R2.B := 44;
    List.Add(R2);

    R2.A := 33;
    R2.B := 44;
    List.Add(R2);

    AssertEquals(3, List.Count);
    AssertEquals(11, List[0].A);
    AssertEquals(22, List[0].B);
    AssertEquals(33, List[1].A);
    AssertEquals(44, List[1].B);
    AssertEquals(33, List[2].A);
    AssertEquals(44, List[2].B);

    List.Delete(2);

    AssertEquals(2, List.Count);
    AssertEquals(11, List[0].A);
    AssertEquals(22, List[0].B);
    AssertEquals(33, List[1].A);
    AssertEquals(44, List[1].B);

    AssertEquals(0, List.IndexOf(R1));
    AssertEquals(1, List.IndexOf(R2));

    // change R1 and R2, to make sure it doesn't matter for tests
    R1.A := 111111;
    R1.B := 222222;
    R2.A := 333333;
    R2.B := 444444;
    AssertEquals(-1, List.IndexOf(R1));
    AssertEquals(-1, List.IndexOf(R2));

    R.A := 11;
    R.B := 22;
    AssertEquals(0, List.IndexOf(R));

    R.A := 33;
    R.B := 44;
    AssertEquals(1, List.IndexOf(R));

    R.A := 11;
    R.B := 22;
    List.Remove(R);
    AssertEquals(1, List.Count);
    AssertEquals(33, List[0].A);
    AssertEquals(44, List[0].B);

    R.A := 666;
    R.B := 22;
    List.Remove(R); // does nothing, no such record
    AssertEquals(1, List.Count);
    AssertEquals(33, List[0].A);
    AssertEquals(44, List[0].B);
  finally FreeAndNil(List) end;
end;

procedure TTestStdCollections.Test_VectorsList;
type
  TMyVector = packed array [0..1] of Single;
  TMyVectorList = TList<TMyVector>;
var
  List: TMyVectorList;
  R1, R2, R: TMyVector;
begin
  List := TMyVectorList.Create;
  try
    R1[0] := 11;
    R1[1] := 22;
    List.Add(R1);

    R2[0] := 33;
    R2[1] := 44;
    List.Add(R2);

    R2[0] := 33;
    R2[1] := 44;
    List.Add(R2);

    AssertEquals(3, List.Count);
    AssertEquals(11, List[0][0]);
    AssertEquals(22, List[0][1]);
    AssertEquals(33, List[1][0]);
    AssertEquals(44, List[1][1]);
    AssertEquals(33, List[2][0]);
    AssertEquals(44, List[2][1]);

    List.Delete(2);

    AssertEquals(2, List.Count);
    AssertEquals(11, List[0][0]);
    AssertEquals(22, List[0][1]);
    AssertEquals(33, List[1][0]);
    AssertEquals(44, List[1][1]);

    AssertEquals(0, List.IndexOf(R1));
    AssertEquals(1, List.IndexOf(R2));

    // change R1 and R2, to make sure it doesn't matter for tests
    R1[0] := 111111;
    R1[1] := 222222;
    R2[0] := 333333;
    R2[1] := 444444;
    AssertEquals(-1, List.IndexOf(R1));
    AssertEquals(-1, List.IndexOf(R2));

    R[0] := 11;
    R[1] := 22;
    AssertEquals(0, List.IndexOf(R));

    R[0] := 33;
    R[1] := 44;
    AssertEquals(1, List.IndexOf(R));

    R[0] := 11;
    R[1] := 22;
    List.Remove(R);
    AssertEquals(1, List.Count);
    AssertEquals(33, List[0][0]);
    AssertEquals(44, List[0][1]);

    R[0] := 666;
    R[1] := 22;
    List.Remove(R); // does nothing, no such item
    AssertEquals(1, List.Count);
    AssertEquals(33, List[0][0]);
    AssertEquals(44, List[0][1]);
  finally FreeAndNil(List) end;
end;

type
  TSomeClass = class
    procedure Foo(A: Integer);
  end;

procedure TSomeClass.Foo(A: Integer);
begin
end;

procedure TTestStdCollections.Test_MethodsList;
type
  TMyMethod = procedure (A: Integer) of object;
  TMyMethodList = TList<TMyMethod>;

  procedure AssertMethodsEqual(const M1, M2: TMyMethod);
  begin
    AssertTrue(TMethod(M1).Code = TMethod(M2).Code);
    AssertTrue(TMethod(M1).Data = TMethod(M2).Data);
  end;

var
  List: TMyMethodList;
  C1, C2, C3: TSomeClass;
  M: TMyMethod;
begin
  C1 := TSomeClass.Create;
  C2 := TSomeClass.Create;
  C3 := TSomeClass.Create;

  List := TMyMethodList.Create;
  try
    List.Add(C1.Foo);
    List.Add(C2.Foo);
    List.Add(C2.Foo);

    AssertEquals(3, List.Count);
    M := C1.Foo;
    AssertMethodsEqual(List[0], M);
    M := C2.Foo;
    AssertMethodsEqual(List[1], M);
    AssertMethodsEqual(List[2], M);

    List.Delete(2);

    AssertEquals(2, List.Count);
    M := C1.Foo;
    AssertMethodsEqual(List[0], M);
    M := C2.Foo;
    AssertMethodsEqual(List[1], M);

    AssertEquals(0, List.IndexOf(C1.Foo));
    AssertEquals(1, List.IndexOf(C2.Foo));

    AssertEquals(-1, List.IndexOf(C3.Foo));

    List.Remove(C1.Foo);
    AssertEquals(1, List.Count);
    M := C2.Foo;
    AssertMethodsEqual(List[0], M);

    List.Remove(C3.Foo); // does nothing, no such item
    AssertEquals(1, List.Count);
    M := C2.Foo;
    AssertMethodsEqual(List[0], M);
  finally FreeAndNil(List) end;

  C1.Free;
  C2.Free;
  C3.Free;
end;

procedure TTestStdCollections.Test_SortedList;
var
  LSortedList: TSortedList<Integer>;
  i: integer;
  LRandomOrder: TArray<Integer>;
begin
  LRandomOrder := TArray<Integer>.Create(
    10, 8, 17, 19, 2, 0, 13, 15, 5, 7, 12, 14, 4, 6, 11, 9, 16, 18, 3, 1);

  LSortedList := TSortedList<Integer>.Create;
  for i in LRandomOrder do
    LSortedList.Add(i);

  AssertEquals('Wrong Count value for TSortedList', Length(LRandomOrder), LSortedList.Count);

  for i := 0 to 19 do
    AssertEquals(Format('Wrong item (%d) index (%d) in TSortedList',[LSortedList[i], i]), i, LSortedList[i]);

  LSortedList.Free;
end;

procedure TTestStdCollections.Test_Queue;
const
  NUMBERS: array[0..2] of Integer = (3,4,5);
var
  LQueue: TQueue<Integer>;
  i: Integer;
  j: Integer;
  pi: Pinteger;
begin
  LQueue := TQueue<Integer>.Create;

  for i := 1 to 5 do
  begin
    LQueue.Enqueue(i);
    AssertEquals(LQueue.Peek, 1);
  end;

  AssertEquals(LQueue.Dequeue, 1);
  AssertEquals(LQueue.Extract, 2);

  j := 0;
  for i in LQueue do
  begin
    AssertEquals(i, NUMBERS[j]);
    Inc(j);
  end;

  j := 0;
  for pi in LQueue.Ptr^ do
  begin
    AssertEquals(pi^, NUMBERS[j]);
    Inc(j);
  end;

  LQueue.Free;
end;

begin
  RegisterTest(TTestStdCollections);
end.
