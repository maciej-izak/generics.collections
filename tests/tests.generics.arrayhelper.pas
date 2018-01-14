unit tests.generics.arrayhelper;

{$mode delphi}

interface

uses
  fpcunit, testregistry, testutils,
  Classes, SysUtils, Generics.Collections;

type

  { TTestArrayHelper }

  TTestArrayHelper = class(TTestCase)
  protected
    procedure CheckBinarySearch(constref AArray: TArray<Integer>;
      AValue: Integer; AExpectedResult: boolean; out ASearchResult: TBinarySearchResult);
    procedure CheckSearchResult(constref ASearchResult: TBinarySearchResult;
      AValue: Integer; ACandidateIndex, AFoundIndex: SizeInt; ACompareResult: Boolean);
  published
    procedure Test_BinarySearch_Integers;
    procedure Test_BinarySearch_EmptyArray;
  end;

implementation

{ TTestArrayHelper }

procedure TTestArrayHelper.CheckBinarySearch(constref AArray: TArray<Integer>;
  AValue: Integer; AExpectedResult: boolean; out
  ASearchResult: TBinarySearchResult);
begin
  CheckEquals(AExpectedResult,
    TArrayHelper<Integer>.BinarySearch(AArray,AValue,ASearchResult),
    'Wrong BinarySearch result for ' + AValue.ToString);
end;

procedure TTestArrayHelper.CheckSearchResult(constref
  ASearchResult: TBinarySearchResult; AValue: Integer; ACandidateIndex,
  AFoundIndex: SizeInt; ACompareResult: Boolean);
begin
  with ASearchResult do
  begin
    CheckEquals(ACandidateIndex, CandidateIndex, 'Wrong binary search result (CandidateIndex) for ' + AValue.ToString);
    CheckEquals(AFoundIndex, FoundIndex, 'Wrong binary search result (FoundIndex) for ' + AValue.ToString);
    Check(ACompareResult, 'Wrong binary search result (CompareResult) for ' + AValue.ToString);
  end;
end;

procedure TTestArrayHelper.Test_BinarySearch_Integers;
var
  a: TArray<Integer>;
  LSearchResult: TBinarySearchResult;
begin
  a := TArray<Integer>.Create(1,3,5,7,9,11,13,15,20);

  CheckBinarySearch(a, 10, False, LSearchResult);
  CheckSearchResult(LSearchResult, 10, 5, -1, LSearchResult.CompareResult>0);

  CheckBinarySearch(a, 20, True, LSearchResult);
  CheckSearchResult(LSearchResult, 20, 8, 8, LSearchResult.CompareResult=0);
end;

procedure TTestArrayHelper.Test_BinarySearch_EmptyArray;
var
  LSearchResult: TBinarySearchResult;
begin
  CheckBinarySearch(nil, 1, False, LSearchResult);
  CheckSearchResult(LSearchResult, 1, -1, -1, LSearchResult.CompareResult=0);
end;

begin
  RegisterTest(TTestArrayHelper);
end.
