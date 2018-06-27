{
    This file is part of the Free Pascal/NewPascal run time library.
    Copyright (c) 2018 by Maciej Izak (hnb),
    member of the NewPascal development team (http://newpascal.org)

    Copyright(c) 2004-2018 DaThoX

    It contains tests for the Free Pascal generics library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Acknowledgment

    Thanks to Sphere 10 Software (http://sphere10.com) for sponsoring
    many new types, tests and major refactoring of entire library

 **********************************************************************

 !!! IMPORTANT NOTE about usage of Generics.Collections and bug reports !!!

 author of this library has no access to FPC trunk anymore, so every problem
 related to this library should be reported here :

 https://github.com/maciej-izak/generics.collections/issues

 The library is compatible with NewPascal, FPC 3.0.4 and FPC trunk, every problem
 (if possible) will be re-reported to FPC bugtracker with proper patch by main author.
 Compatibility with FPC 3.0.4 and trunk will be provided as long as possible.

 The NewPascal has special support for this library, more recent version (more 
 bug fixes), more optimizations and better support from compiler side 
 (NewPascal contains modified/extended FPC compiler version).

 **********************************************************************}

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

