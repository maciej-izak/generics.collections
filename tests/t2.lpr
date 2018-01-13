program t2;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

uses
  SysUtils, Generics.Collections, Generics.Defaults;

var
  a: TArray<Integer>;
  SearchResult: TBinarySearchResult;
begin
  a := TArray<Integer>.Create(1,3,5,7,9,11,13,15,20);

  if TArrayHelper<Integer>.BinarySearch(a,10,SearchResult) then
    Halt(1);

  with SearchResult do
  if (CandidateIndex <> 5) or (FoundIndex<>-1) or (CompareResult<=0) then
    Halt(2);

  if not (TArrayHelper<Integer>).BinarySearch(a,20,SearchResult) then
    Halt(3);

  with SearchResult do
  if (CandidateIndex <> 8) or (FoundIndex<>8) or (CompareResult<>0) then
    Halt(4);


  WriteLn('ok');
end.
