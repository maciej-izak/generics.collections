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

 **********************************************************************}

unit tests.generics.hashmaps;

{$mode delphi}

interface

uses
  fpcunit, testregistry, testutils, typinfo,
  Classes, SysUtils, StrUtils, Generics.Collections, Generics.Defaults;

type
  PCollectionNotification = ^TCollectionNotification;

  { TTestHashMaps }

  TTestHashMaps= class(TTestCase)
  private
    procedure CountAsKey_Check(const AWhat: string; AValue, AExpectedValue: Integer;
      AAction: PCollectionNotification);
    procedure CountAsKey_Notify(const AKind: string; ASender: TObject; constref AItem: Integer; AAction: TCollectionNotification);
    procedure CountAsKey_NotifyValue(ASender: TObject; constref AItem: Integer; AAction: TCollectionNotification);
    procedure CountAsKey_NotifyKey(ASender: TObject; constref AItem: Integer; AAction: TCollectionNotification);
  published
    procedure Test_CountAsKey_OpenAddressingLP;
    procedure Test_CountAsKey_OpenAddressingLPT;
    procedure Test_CountAsKey_OpenAddressingQP;
    procedure Test_CountAsKey_OpenAddressingDH;
    procedure Test_CountAsKey_CuckooD2;
    procedure Test_CountAsKey_CuckooD4;
    procedure Test_CountAsKey_CuckooD6;

    procedure Test_TryAddOrSetOrGetValue;
  end;

implementation

{ TTestHashMaps }

procedure TTestHashMaps.CountAsKey_Check(const AWhat: string; AValue, AExpectedValue: Integer;
    AAction: PCollectionNotification);
var
  LCollectionNotificationStr: string;
begin
  if Assigned(AAction) then
    LCollectionNotificationStr := GetEnumName(TypeInfo(TCollectionNotification), Ord(AAction^));

  AssertEquals(AWhat + LCollectionNotificationStr, AExpectedValue, AValue);
end;

procedure TTestHashMaps.CountAsKey_Notify(const AKind: string; ASender: TObject; constref
  AItem: Integer; AAction: TCollectionNotification);
var
  LCount: Integer;
begin
  CountAsKey_Check('Item ('+AKind+')', AItem, 0, @AAction);
  LCount := TCustomDictionary<Integer, Integer, TDefaultHashFactory>(ASender).Count;
  case AAction of
    cnAdded:
      CountAsKey_Check('Count', LCount, 1, @AAction);
    cnRemoved:
      CountAsKey_Check('Count', LCount, 0, @AAction);
    cnExtracted: Halt(4);
  end;
end;

procedure TTestHashMaps.CountAsKey_NotifyValue(ASender: TObject; constref AItem: Integer;
  AAction: TCollectionNotification);
begin
  CountAsKey_Notify('Value', ASender, AItem, AAction);
end;

procedure TTestHashMaps.CountAsKey_NotifyKey(ASender: TObject; constref AItem: Integer;
  AAction: TCollectionNotification);
begin
  CountAsKey_Notify('Key', ASender, AItem, AAction);
end;

procedure TTestHashMaps.Test_CountAsKey_OpenAddressingLP;
var
  LDictionary: TOpenAddressingLP<Integer, Integer>;
begin
  // TOpenAddressingLP
  LDictionary := TOpenAddressingLP<Integer, Integer>.Create;
  LDictionary.OnKeyNotify := CountAsKey_NotifyKey;
  LDictionary.OnValueNotify := CountAsKey_NotifyValue;
  CountAsKey_Check('Count', LDictionary.Count, 0, nil);
  LDictionary.Add(LDictionary.Count,LDictionary.Count);
  CountAsKey_Check('Item', LDictionary[0], 0, nil);
  LDictionary.Free;
end;

procedure TTestHashMaps.Test_CountAsKey_OpenAddressingLPT;
var
  LDictionary: TOpenAddressingLPT<Integer, Integer>;
begin
  // TOpenAddressingLPT
  LDictionary := TOpenAddressingLPT<Integer, Integer>.Create;
  LDictionary.OnKeyNotify := CountAsKey_NotifyKey;
  LDictionary.OnValueNotify := CountAsKey_NotifyValue;
  CountAsKey_Check('Count', LDictionary.Count, 0, nil);
  LDictionary.Add(LDictionary.Count,LDictionary.Count);
  CountAsKey_Check('Item', LDictionary[0], 0, nil);
  LDictionary.Free;
end;

procedure TTestHashMaps.Test_CountAsKey_OpenAddressingQP;
var
  LDictionary: TOpenAddressingQP<Integer, Integer>;
begin
  // TOpenAddressingQP
  LDictionary := TOpenAddressingQP<Integer, Integer>.Create;
  LDictionary.OnKeyNotify := CountAsKey_NotifyKey;
  LDictionary.OnValueNotify := CountAsKey_NotifyValue;
  CountAsKey_Check('Count', LDictionary.Count, 0, nil);
  LDictionary.Add(LDictionary.Count,LDictionary.Count);
  CountAsKey_Check('Item', LDictionary[0], 0, nil);
  LDictionary.Free;
end;

procedure TTestHashMaps.Test_CountAsKey_OpenAddressingDH;
var
  LDictionary: TOpenAddressingDH<Integer, Integer>;
begin
  // TOpenAddressingDH
  LDictionary := TOpenAddressingDH<Integer, Integer>.Create;
  LDictionary.OnKeyNotify := CountAsKey_NotifyKey;
  LDictionary.OnValueNotify := CountAsKey_NotifyValue;
  CountAsKey_Check('Count', LDictionary.Count, 0, nil);
  LDictionary.Add(LDictionary.Count,LDictionary.Count);
  CountAsKey_Check('Item', LDictionary[0], 0, nil);
  LDictionary.Free;
end;

procedure TTestHashMaps.Test_CountAsKey_CuckooD2;
var
  LDictionary: TCuckooD2<Integer, Integer>;
begin
  // TCuckooD2
  LDictionary := TCuckooD2<Integer, Integer>.Create;
  LDictionary.OnKeyNotify := CountAsKey_NotifyKey;
  LDictionary.OnValueNotify := CountAsKey_NotifyValue;
  CountAsKey_Check('Count', LDictionary.Count, 0, nil);
  LDictionary.Add(LDictionary.Count,LDictionary.Count);
  CountAsKey_Check('Item', LDictionary[0], 0, nil);
  LDictionary.Free;
end;

procedure TTestHashMaps.Test_CountAsKey_CuckooD4;
var
  LDictionary: TCuckooD4<Integer, Integer>;
begin
  // TCuckooD4
  LDictionary := TCuckooD4<Integer, Integer>.Create;
  LDictionary.OnKeyNotify := CountAsKey_NotifyKey;
  LDictionary.OnValueNotify := CountAsKey_NotifyValue;
  CountAsKey_Check('Count', LDictionary.Count, 0, nil);
  LDictionary.Add(LDictionary.Count,LDictionary.Count);
  CountAsKey_Check('Item', LDictionary[0], 0, nil);
  LDictionary.Free;
end;

procedure TTestHashMaps.Test_CountAsKey_CuckooD6;
var
  LDictionary: TCuckooD6<Integer, Integer>;
begin
  // TCuckooD6
  LDictionary := TCuckooD6<Integer, Integer>.Create;
  LDictionary.OnKeyNotify := CountAsKey_NotifyKey;
  LDictionary.OnValueNotify := CountAsKey_NotifyValue;
  CountAsKey_Check('Count', LDictionary.Count, 0, nil);
  LDictionary.Add(LDictionary.Count,LDictionary.Count);
  CountAsKey_Check('Item', LDictionary[0], 0, nil);
  LDictionary.Free;
end;

procedure TTestHashMaps.Test_TryAddOrSetOrGetValue;
// modified test from Castle Game Engine (https://castle-engine.sourceforge.io)
var
  LObjects: TDictionary<string, TObject>;
  LObject, LFoundObject: TObject;
begin
  LObjects := TDictionary<string, TObject>.Create;
  try
    LObjects.TryGetValue('blah', LFoundObject);
    AssertTrue(nil = LFoundObject);

    LObject := TObject.Create;
    LObjects.AddOrSetValue('nope', LObject);

    LObjects.TryGetValue('blah', LFoundObject);
    AssertTrue(nil = LFoundObject);

    LObject := TObject.Create;
    LObjects.AddOrSetValue('blah', LObject);

    LObjects.TryGetValue('blah', LFoundObject);
    AssertTrue(LObject = LFoundObject);

    LObjects.Remove('blah');

    LObject.Free;

    LObjects.TryGetValue('blah', LFoundObject);
    AssertTrue(nil = LFoundObject);

    LObjects['nope'].Free;
  finally
    FreeAndNil(LObjects)
  end;
end;

begin
  RegisterTest(TTestHashMaps);
end.

