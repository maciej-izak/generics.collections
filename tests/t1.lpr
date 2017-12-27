program t1;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

uses
  Generics.Collections;

type
  PCollectionNotification = ^TCollectionNotification;

  { TFoo }

  TFoo = class
  private
    class procedure Notify(const AKind: string; ASender: TObject; constref AItem: Integer; AAction: TCollectionNotification);
  public
    class procedure NotifyValue(ASender: TObject; constref AItem: Integer; AAction: TCollectionNotification);
    class procedure NotifyKey(ASender: TObject; constref AItem: Integer; AAction: TCollectionNotification);
  end;

var
  d1: TOpenAddressingLP<Integer, Integer>;
  d2: TOpenAddressingLPT<Integer, Integer>;
  d3: TOpenAddressingQP<Integer, Integer>;
  d4: TOpenAddressingDH<Integer, Integer>;
  d5: TCuckooD2<Integer, Integer>;
  d6: TCuckooD4<Integer, Integer>;
  d7: TCuckooD6<Integer, Integer>;

{ TFoo }

procedure Check(const AWhat: string; AValue, AExpectedValue: Integer;
    AAction: PCollectionNotification; AExitCode: Integer);
begin
  if AValue <> AExpectedValue then
  begin
    Write('Expected ', AWhat, ' is ', AExpectedValue,' but ', AValue, ' found');
    if AAction <> nil then
      WriteLn(' (action ', AAction^, ')')
    else
      WriteLn;
    Halt(AExitCode);
  end;
end;

class procedure TFoo.Notify(const AKind: string; ASender: TObject; constref
  AItem: Integer; AAction: TCollectionNotification);
var
  LCount: Integer;
begin
  Check('Item ('+AKind+')', AItem, 0, @AAction, 1);
  // ugly casting but will work, all dictionares are TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>
  // and on the TCustomDictionary level property Count is declared
  LCount := TOpenAddressingLP<Integer, Integer>(ASender).Count;
  case AAction of
    cnAdded:
      Check('Count', LCount, 1, @AAction, 2);
    cnRemoved:
      Check('Count', LCount, 0, @AAction, 3);
    cnExtracted: Halt(4);
  end;
end;

class procedure TFoo.NotifyValue(ASender: TObject; constref AItem: Integer;
  AAction: TCollectionNotification);
begin
  Notify('Value', ASender, AItem, AAction);
end;

class procedure TFoo.NotifyKey(ASender: TObject; constref AItem: Integer;
  AAction: TCollectionNotification);
begin
  Notify('Key', ASender, AItem, AAction);
end;

begin
  // TOpenAddressingLP
  d1 := TOpenAddressingLP<Integer, Integer>.Create;
  d1.OnKeyNotify := TFoo.NotifyKey;
  d1.OnValueNotify := TFoo.NotifyValue;
  WriteLn('TOpenAddressingLP testing...');
  Check('Count', d1.Count, 0, nil, 5);
  d1.Add(d1.Count,d1.Count);
  Check('Item', d1[0], 0, nil, 6);
  d1.Free;
  WriteLn;

  // TOpenAddressingLPT
  d2 := TOpenAddressingLPT<Integer, Integer>.Create;
  d2.OnKeyNotify := TFoo.NotifyKey;
  d2.OnValueNotify := TFoo.NotifyValue;
  WriteLn('TOpenAddressingLPT testing...');
  Check('Count', d2.Count, 0, nil, 7);
  d2.Add(d2.Count,d2.Count);
  Check('Item', d2[0], 0, nil, 8);
  d2.Free;
  WriteLn;

  // TOpenAddressingQP
  d3 := TOpenAddressingQP<Integer, Integer>.Create;
  d3.OnKeyNotify := TFoo.NotifyKey;
  d3.OnValueNotify := TFoo.NotifyValue;
  WriteLn('TOpenAddressingQP testing...');
  Check('Count', d3.Count, 0, nil, 9);
  d3.Add(d3.Count,d3.Count);
  Check('Item', d3[0], 0, nil, 10);
  d3.Free;
  WriteLn;

  // TOpenAddressingDH
  d4 := TOpenAddressingDH<Integer, Integer>.Create;
  d4.OnKeyNotify := TFoo.NotifyKey;
  d4.OnValueNotify := TFoo.NotifyValue;
  WriteLn('TOpenAddressingDH testing...');
  Check('Count', d4.Count, 0, nil, 11);
  d4.Add(d4.Count,d4.Count);
  Check('Item', d4[0], 0, nil, 12);
  d4.Free;
  WriteLn;

  // TCuckooD2
  d5 := TCuckooD2<Integer, Integer>.Create;
  d5.OnKeyNotify := TFoo.NotifyKey;
  d5.OnValueNotify := TFoo.NotifyValue;
  WriteLn('TCuckooD2 testing...');
  Check('Count', d5.Count, 0, nil, 13);
  d5.Add(d5.Count,d5.Count);
  Check('Item', d5[0], 0, nil, 14);
  d5.Free;
  WriteLn;

  // TCuckooD4
  d6 := TCuckooD4<Integer, Integer>.Create;
  d6.OnKeyNotify := TFoo.NotifyKey;
  d6.OnValueNotify := TFoo.NotifyValue;
  WriteLn('TCuckooD4 testing...');
  Check('Count', d6.Count, 0, nil, 15);
  d6.Add(d6.Count,d6.Count);
  Check('Item', d6[0], 0, nil, 16);
  d6.Free;
  WriteLn;

  // TCuckooD6
  d7 := TCuckooD6<Integer, Integer>.Create;
  d7.OnKeyNotify := TFoo.NotifyKey;
  d7.OnValueNotify := TFoo.NotifyValue;
  WriteLn('TCuckooD6 testing...');
  Check('Count', d7.Count, 0, nil, 17);
  d7.Add(d7.Count,d7.Count);
  Check('Item', d7[0], 0, nil, 18);
  d7.Free;
  WriteLn;

  WriteLn('ok');
end.
