unit tests.generics.utils;

{$mode delphi}

interface

uses
  fpcunit, testutils, testregistry,
  Classes, SysUtils, Generics.Collections;

type
  TNotificationRec<T> = record
    Sender: TObject;
    Item: T;
    Action: TCollectionNotification;
    Executed: boolean;
  end;

  TTestCollections = class(TTestCase)
  private type
    TNotificationRec_String = TNotificationRec<string>;
    TNotificationRec_TObject = TNotificationRec<TObject>;
  private
    NotificationsListStr: TList<TNotificationRec_String>;
    NotificationsListObj: TList<TNotificationRec_TObject>;
    NotificationsIndex: Integer;
  protected
    procedure NotificationAdd(ASender: TObject; const AItem: string;
      AAction: TCollectionNotification); overload;
    procedure NotificationAdd(ASender: TObject; const AItems: array of string;
      AAction: TCollectionNotification); overload;
    procedure NotificationAdd(ASender: TObject; const AItem: TObject;
      AAction: TCollectionNotification); overload;
    procedure NotificationAdd(ASender: TObject; const AItems: array of TObject;
      AAction: TCollectionNotification); overload;
    procedure AssertNotificationsExecutedStr;
    procedure ClearNotificationsStr;
    procedure AssertNotificationsExecutedObj;
    procedure ClearNotificationsObj;
    procedure NotifyTestStr(ASender: TObject; constref AItem: string; AAction: TCollectionNotification);
    procedure NotifyTestObj(ASender: TObject; constref AItem: TObject; AAction: TCollectionNotification);

    procedure CreateObjects(var AArray: TArray<TObject>; ACount: Integer);
    procedure FreeObjects(AArray: TArray<TObject>);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TStringList = TList<string>;

  { TStringsEnumerator }

  TStringsEnumerator = class(TInterfacedObject, IEnumerator<string>)
  private
    FEnumerator: TStringList.TEnumerator;
    FCollection: TStringList;

    function GetCurrent: string;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: string read GetCurrent;

    constructor Create(AEnumerator: TStringList.TEnumerator; ACollection: TStringList);
    destructor Destroy; override;
  end;

  { TStringsEnumerable }

  TStringsEnumerable = class(TInterfacedObject, IEnumerable<string>)
  private
    FEnumerable: TStringList;

    function GetEnumerator: IEnumerator<string>;

    constructor Create(const AItems: array of string);
  end;

  TObjectList = TList<TObject>;

  { TObjectEnumerator }

  TObjectEnumerator = class(TInterfacedObject, IEnumerator<TObject>)
  private
    FEnumerator: TObjectList.TEnumerator;
    FCollection: TObjectList;

    function GetCurrent: TObject;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TObject read GetCurrent;

    constructor Create(AEnumerator: TObjectList.TEnumerator; ACollection: TObjectList);
    destructor Destroy; override;
  end;

  { TObjectEnumerable }

  TObjectEnumerable = class(TInterfacedObject, IEnumerable<TObject>)
  private
    FEnumerable: TObjectList;

    function GetEnumerator: IEnumerator<TObject>;

    constructor Create(const AItems: array of TObject);
  end;

function EnumerableStringsIntf(const AItems: array of string): IEnumerable<string>;
function EnumerableStringsObj(const AItems: array of string): TEnumerable<string>;
function EnumerableObjectsIntf(const AItems: array of TObject): IEnumerable<TObject>;
function EnumerableObjectsObj(const AItems: array of TObject): TEnumerable<TObject>;

implementation

function EnumerableStringsIntf(const AItems: array of string): IEnumerable<string>;
begin
  Result := TStringsEnumerable.Create(AItems);
end;

function EnumerableStringsObj(const AItems: array of string): TEnumerable<string>;
begin
  Result := TStringList.Create;
  TStringList(Result).AddRange(AItems);
end;

function EnumerableObjectsIntf(const AItems: array of TObject): IEnumerable<TObject>;
begin
  Result := TObjectEnumerable.Create(AItems);
end;

function EnumerableObjectsObj(const AItems: array of TObject): TEnumerable<TObject>;
begin
  Result := TObjectList.Create;
  TObjectList(Result).AddRange(AItems);
end;

{ TTestCollections }

procedure TTestCollections.NotificationAdd(ASender: TObject;
  const AItem: string; AAction: TCollectionNotification);
var
  LNotification: TNotificationRec_String;
begin
  LNotification.Sender := ASender;
  LNotification.Item := AItem;
  LNotification.Action := AAction;
  LNotification.Executed := False;
  NotificationsListStr.Add(LNotification);
end;

procedure TTestCollections.NotificationAdd(ASender: TObject;
  const AItems: array of string; AAction: TCollectionNotification);
var
  s: string;
begin
  for s in AItems do
    NotificationAdd(ASender, s, AAction);
end;

procedure TTestCollections.NotificationAdd(ASender: TObject;
  const AItem: TObject; AAction: TCollectionNotification);
var
  LNotification: TNotificationRec_TObject;
begin
  LNotification.Sender := ASender;
  LNotification.Item := AItem;
  LNotification.Action := AAction;
  LNotification.Executed := False;
  NotificationsListObj.Add(LNotification);
end;

procedure TTestCollections.NotificationAdd(ASender: TObject;
  const AItems: array of TObject; AAction: TCollectionNotification);
var
  o: TObject;
begin
  for o in AItems do
    NotificationAdd(ASender, o, AAction);
end;

procedure TTestCollections.AssertNotificationsExecutedStr;
var
  p: ^TNotificationRec_String;
begin
  for p in NotificationsListStr.Ptr^ do
    AssertTrue(p^.Executed);
  AssertEquals(NotificationsIndex, NotificationsListStr.Count);
  ClearNotificationsStr;
end;

procedure TTestCollections.ClearNotificationsStr;
begin
  NotificationsListStr.Clear;
  NotificationsIndex := 0;
end;

procedure TTestCollections.AssertNotificationsExecutedObj;
var
  p: ^TNotificationRec_TObject;
begin
  for p in NotificationsListObj.Ptr^ do
    AssertTrue(p^.Executed);
  AssertEquals(NotificationsIndex, NotificationsListObj.Count);
  ClearNotificationsObj;
end;

procedure TTestCollections.ClearNotificationsObj;
begin
  NotificationsListObj.Clear;
  NotificationsIndex := 0;
end;

procedure TTestCollections.NotifyTestStr(ASender: TObject; constref AItem: string; AAction: TCollectionNotification);
var
  LNotification: TNotificationRec_String;
begin
  AssertTrue(NotificationsIndex < NotificationsListStr.Count);
  LNotification := NotificationsListStr[NotificationsIndex];
  AssertTrue(ASender = LNotification.Sender);
  AssertEquals(AItem, LNotification.Item);
  AssertTrue(AAction = LNotification.Action);
  AssertFalse(LNotification.Executed);
  LNotification.Executed := True;
  NotificationsListStr[NotificationsIndex] := LNotification;
  Inc(NotificationsIndex)
end;

procedure TTestCollections.NotifyTestObj(ASender: TObject; constref AItem: TObject; AAction: TCollectionNotification);
var
  LNotification: TNotificationRec_TObject;
begin
  AssertTrue(NotificationsIndex < NotificationsListObj.Count);
  LNotification := NotificationsListObj[NotificationsIndex];
  AssertTrue(ASender = LNotification.Sender);
  AssertTrue(AItem = LNotification.Item);
  AssertTrue(AAction = LNotification.Action);
  AssertFalse(LNotification.Executed);
  LNotification.Executed := True;
  NotificationsListObj[NotificationsIndex] := LNotification;
  Inc(NotificationsIndex)
end;

procedure TTestCollections.CreateObjects(var AArray: TArray<TObject>; ACount: Integer);
var
  i: Integer;
begin
  SetLength(AArray, ACount);
  for i := 0 to ACount - 1 do
    AArray[i] := TObject.Create;
end;

procedure TTestCollections.FreeObjects(AArray: TArray<TObject>);
var
  o: TObject;
begin
  for o in AArray do
    o.Free;
end;

constructor TTestCollections.Create;
begin
  inherited;
  NotificationsListStr := TList<TNotificationRec_String>.Create;
  NotificationsListObj := TList<TNotificationRec_TObject>.Create;
end;

destructor TTestCollections.Destroy;
begin
  NotificationsListStr.Free;
  NotificationsListObj.Free;
  inherited;
end;

{ TStringsEnumerable }

function TStringsEnumerable.GetEnumerator: IEnumerator<string>;
begin
  Result := TStringsEnumerator.Create(FEnumerable.GetEnumerator, FEnumerable);
end;

constructor TStringsEnumerable.Create(const AItems: array of string);
begin
  FEnumerable := TStringList.Create;
  FEnumerable.AddRange(AItems);
end;

{ TStringsEnumerator }

function TStringsEnumerator.GetCurrent: string;
begin
  Result := FEnumerator.Current;
end;

function TStringsEnumerator.MoveNext: Boolean;
begin
  Result := FEnumerator.MoveNext;
end;

procedure TStringsEnumerator.Reset;
begin
  FEnumerator.Free;
  FEnumerator := FCollection.GetEnumerator;
end;

constructor TStringsEnumerator.Create(AEnumerator: TStringList.TEnumerator; ACollection: TStringList);
begin
  FEnumerator := AEnumerator;
  FCollection := ACollection;
end;

destructor TStringsEnumerator.Destroy;
begin
  FEnumerator.Free;
  inherited Destroy;
end;

{ TObjectEnumerable }

function TObjectEnumerable.GetEnumerator: IEnumerator<TObject>;
begin
  Result := TObjectEnumerator.Create(FEnumerable.GetEnumerator, FEnumerable);
end;

constructor TObjectEnumerable.Create(const AItems: array of TObject);
begin
  FEnumerable := TObjectList.Create;
  FEnumerable.AddRange(AItems);
end;

{ TObjectEnumerator }

function TObjectEnumerator.GetCurrent: TObject;
begin
  Result := FEnumerator.Current;
end;

function TObjectEnumerator.MoveNext: Boolean;
begin
  Result := FEnumerator.MoveNext;
end;

procedure TObjectEnumerator.Reset;
begin
  FEnumerator.Free;
  FEnumerator := FCollection.GetEnumerator;
end;

constructor TObjectEnumerator.Create(AEnumerator: TObjectList.TEnumerator; ACollection: TObjectList);
begin
  FEnumerator := AEnumerator;
  FCollection := ACollection;
end;

destructor TObjectEnumerator.Destroy;
begin
  FEnumerator.Free;
  inherited Destroy;
end;

end.

