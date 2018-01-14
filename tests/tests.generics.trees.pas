unit tests.generics.trees;

{$mode delphi}

interface

uses
  fpcunit, testregistry, testutils,
  Classes, SysUtils, Generics.Collections;

type

  { TTestArrayHelper }

  { TTestTrees }

  TTestTrees = class(TTestCase)
  published
    procedure Test_IndexedAVLTree_Add;
    procedure Test_IndexedAVLTree_Delete;
  end;

implementation

type
  TStringsTree = TIndexedAVLTree<string>;

{ TTestTrees }

procedure TTestTrees.Test_IndexedAVLTree_Add;
var
  LNumbers: THashSet<Integer>;
  i, j: Integer;
  LTree: TStringsTree;
  LNodes: TList<TStringsTree.PNode>;
  n: TStringsTree.PNode;
begin
  LNumbers := THashSet<Integer>.Create;
  LTree := TStringsTree.Create;
  LNodes := TList<TStringsTree.PNode>.Create;

  // check consistency of adding new nodes to Indexed AVL
  for i := 0 to 999 do
  begin
    LNodes.Add(LTree.Add('0'+i.ToString));
    LNumbers.Clear;
    for n in LTree.Nodes do
      Check(LNumbers.Add(LTree.NodeToIndex(n)), 'Wrong index (duplicate) of '+ i.ToString + ' for node ' + n.Key);
    for j := 0 to LNodes.Count - 1 do
      Check(LNumbers.Contains(j), 'Missing index ' + j.ToString + ' for i = ' + i.ToString);
    LTree.ConsistencyCheck;
    CheckEquals(i+1, LTree.Count, 'Wrong tree count');
  end;

  LNodes.Free;
  LTree.Free;
  LNumbers.Free;
end;

procedure TTestTrees.Test_IndexedAVLTree_Delete;
var
  LNumbers: THashSet<Integer>;
  i, j: Integer;
  LTree: TStringsTree;
  LNodes: TList<TStringsTree.PNode>;
  n: TStringsTree.PNode;
begin
  LNumbers := THashSet<Integer>.Create;
  LTree := TStringsTree.Create;
  LNodes := TList<TStringsTree.PNode>.Create;

  for i := 0 to 999 do
    LNodes.Add(LTree.Add('0'+i.ToString));

  // check consistency of deleting nodes from Indexed AVL
  for i := 0 to 999 do
  begin
    LTree.Delete(LNodes.ExtractIndex(Random(LNodes.count)));
    LNumbers.Clear;
    for n in LTree.Nodes do
      Check(LNumbers.Add(LTree.NodeToIndex(n)), 'Wrong index (duplicate) of '+ i.ToString + ' for node ' + n.Key);
    for j := 0 to LNodes.Count - 1 do
      Check(LNumbers.Contains(j), 'Missing index ' + j.ToString + ' for i = ' + i.ToString);
    LTree.ConsistencyCheck;
    CheckEquals(999-i, LTree.Count, 'Wrong tree count');
  end;

  LNodes.Free;
  LTree.Free;
  LNumbers.Free;
end;

begin
  RegisterTest(TTestTrees);
end.

