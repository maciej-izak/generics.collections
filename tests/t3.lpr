program t3;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

uses
  SysUtils, Generics.Collections;

type
  TStringsTree = TIndexedAVLTree<string>;

var
  Numbers: THashSet<Integer>;
  i, j: Integer;
  Tree: TStringsTree;
  Nodes: TList<TStringsTree.PNode>;
  n: TStringsTree.PNode;
begin
  Numbers := THashSet<Integer>.Create;
  Tree := TStringsTree.Create;
  Nodes := TList<TStringsTree.PNode>.Create;

  // check consistency of adding new nodes to Indexed AVL
  for i := 0 to 999 do
  begin
    Nodes.Add(Tree.Add('0'+i.ToString));
    Numbers.Clear;
    for n in Tree.Nodes do
      if not Numbers.Add(Tree.NodeToIndex(n)) then
      begin
        WriteLn('Wrong index (duplicate) of ', i, ' for node ', n.Key);
        Halt(1);
      end;
    for j := 0 to Nodes.Count - 1 do
      if not Numbers.Contains(j) then
      begin
        WriteLn(i,'Missing index ', j);
        Halt(2);
      end;
    try
      Tree.ConsistencyCheck;
    except
      on E: EIndexedAVLTree do
      begin
        WriteLn(E.Message);
        Halt(3);
      end;
      on E: EAVLTree do
      begin
        WriteLn(E.Message);
        Halt(4);
      end;
    end;
    if Tree.Count <> i+1 then
      Halt(5);
  end;

  // check consistency of deleting nodes from Indexed AVL
  for i := 0 to 999 do
  begin
    Tree.Delete(Nodes.ExtractIndex(Random(Nodes.count)));
    Numbers.Clear;
    for n in Tree.Nodes do
      if not Numbers.Add(Tree.NodeToIndex(n)) then
      begin
        WriteLn('Wrong index (duplicate) of ', i, ' for node ', n.Key);
        Halt(6);
      end;
    for j := 0 to Nodes.Count - 1 do
      if not Numbers.Contains(j) then
      begin
        WriteLn(i,'Missing index ', j);
        Halt(7);
      end;
    try
      Tree.ConsistencyCheck;
    except
      on E: EIndexedAVLTree do
      begin
        WriteLn(E.Message);
        Halt(8);
      end;
      on E: EAVLTree do
      begin
        WriteLn(E.Message);
        Halt(9);
      end;
    end;
    if Tree.Count <> 1000-(i+1) then
      Halt(10);
  end;

  Nodes.Free;
  Tree.Free;
  Numbers.Free;

  WriteLn('ok');
end.
