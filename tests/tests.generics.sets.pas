unit tests.generics.sets;

{$mode delphi}

interface

uses
  fpcunit, testregistry, testutils,
  Classes, SysUtils, Generics.Collections;

type
  THashSet_Integer = THashSet<Integer>;
  TSortedSet_Integer = TSortedSet<Integer>;
  TSortedHashSet_Integer = TSortedHashSet<Integer>;

  { TTestSets }

  TTestSets = class(TTestCase)
  public
    constructor Create; override;
  published
    procedure Test_HashSet_General;
    procedure Test_SortedSet_General;
    procedure Test_SortedHashSet_General;
  end;

  { TGenericTestSets }

  TGenericTestSets<T> = record
    class procedure ValidateHashSet(ASet: T; const ANumbers: array of Integer); static;
    class procedure Test_Set_General; static;
  end;

var
  GTest: TTestSets;

implementation

{ TGenericTestSets }

class procedure TGenericTestSets<T>.ValidateHashSet(ASet: T;
  const ANumbers: array of Integer);
var
  i: Integer;
begin with GTest do begin
  for i in ANumbers do
    AssertTrue('Can''t find number ' + i.ToString, ASet.Contains(i));
  AssertEquals(ASet.Count, Length(ANumbers));
end end;

class procedure TGenericTestSets<T>.Test_Set_General;
var
  NumbersA: T;
  NumbersB: T;
  NumbersC: T;
  i: Integer;
begin with GTest do begin
  NumbersA := T.Create;
  NumbersB := T.Create;

  for i := 0 to 4 do
  begin
    AssertTrue(NumbersA.Add(i * 2));
    AssertTrue(NumbersB.Add((i * 2) + 1));
  end;

  ValidateHashSet(NumbersA, [0, 2, 4, 6, 8]);
  ValidateHashSet(NumbersB, [1, 3, 5, 7, 9]);

  { UnionWith }
  NumbersC := T.Create(NumbersA);
  NumbersC.UnionWith(NumbersB);
  ValidateHashSet(NumbersC, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
  AssertFalse(NumbersC.Add(5));
  AssertFalse(NumbersC.AddRange([6, 7]));
  AssertEquals(NumbersC.Count, 10);

  { ExceptWith }
  NumbersC.ExceptWith(NumbersB);
  AssertEquals(NumbersC.Count, 5);
  ValidateHashSet(NumbersC, [0, 2, 4, 6, 8]);
  AssertTrue(NumbersC.AddRange(NumbersB));
  ValidateHashSet(NumbersC, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);

  { SymmetricExceptWith }
  NumbersA.Clear;
  AssertEquals(NumbersA.Count, 0);
  NumbersB.Clear;
  AssertEquals(NumbersB.Count, 0);
  NumbersC.Clear;
  AssertEquals(NumbersC.Count, 0);
  AssertTrue(NumbersA.AddRange([0, 1, 2, 3, 4, 5]));
  ValidateHashSet(NumbersA, [0, 1, 2, 3, 4, 5]);
  AssertTrue(NumbersB.AddRange([3, 4, 5, 6, 7, 8, 9]));
  ValidateHashSet(NumbersB, [3, 4, 5, 6, 7, 8, 9]);
  NumbersC.Free;
  NumbersC := T.Create(NumbersA);
  ValidateHashSet(NumbersC, [0, 1, 2, 3, 4, 5]);
  NumbersC.SymmetricExceptWith(NumbersB);
  ValidateHashSet(NumbersC, [0, 1, 2, 8, 7, 6, 9]);

  { IntersectWith }
  NumbersA.Clear;
  AssertEquals(NumbersA.Count, 0);
  NumbersB.Clear;
  AssertEquals(NumbersB.Count, 0);
  NumbersC.Clear;
  AssertEquals(NumbersC.Count, 0);
  AssertTrue(NumbersA.AddRange([0, 1, 2, 3, 4, 5]));
  AssertTrue(NumbersB.AddRange([3, 4, 5, 6, 7, 8, 9]));
  AssertTrue(NumbersC.AddRange(NumbersA));
  NumbersC.IntersectWith(NumbersB);
  ValidateHashSet(NumbersC, [3, 4, 5]);

  NumbersC.Free;
  NumbersB.Free;
  NumbersA.Free;
end end;

{ TTestSets }

constructor TTestSets.Create;
begin
  inherited Create;
  GTest := Self;
end;

procedure TTestSets.Test_HashSet_General;
begin
  TGenericTestSets<THashSet_Integer>.Test_Set_General;
end;

procedure TTestSets.Test_SortedSet_General;
begin
  TGenericTestSets<TSortedSet_Integer>.Test_Set_General;
end;

procedure TTestSets.Test_SortedHashSet_General;
begin
  TGenericTestSets<TSortedHashSet_Integer>.Test_Set_General;
end;

begin
  RegisterTest(TTestSets);
end.

