{*********************************************}
{  TeeBI Software Library                     }
{  TDataCollectionItem (Collection Item)      }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.CollectionItem;

interface

{
  TDataCollectionItem is a derived class of TCollectionItem.

  It can be used to create custom TCollection classes where each item
  can reference to a TDataItem and / or a TDataProvider.

  For example, TBIQuery component uses this class for their collections.
}

uses
  {System.}Classes, BI.DataItem, BI.Expression;

type
  TDataCollectionItem=class(TCollectionItem)
  private
    FName : String;
    FOnChange: TNotifyEvent;

    procedure AddNotify;
    function GetData: TDataItem;
    procedure InternalSetProvider(const Value: TComponent);
    function LoadOrigin:TDataItem;
    procedure Notify(const AEvent:TBIEvent);
    procedure NotifyDataDestroy(const AEvent:TBIEvent);
    function Origin:String;
    procedure ReadOrigin(Reader: TReader);
    procedure RemoveNotify;
    procedure SetDataDirect(const Value: TDataItem);
    procedure WriteOrigin(Writer: TWriter);
  protected
    FData : TDataItem;
    FProvider : TComponent;

    IOrigin : String;
    IUpdating : Integer;

    procedure BeginUpdate; inline;
    procedure Changed; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure EndUpdate;
    function GetDisplayName:String; override;
    procedure Loaded; virtual;
    function Owner:TComponent; virtual;
    procedure SetData(const Value: TDataItem); virtual;
    procedure SetProvider(const Value: TComponent); virtual;
    procedure ValidateData(const AData:TDataItem); virtual;
  public
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Data:TDataItem read GetData write SetData;
    property Name:String read FName write FName;
    property Provider:TComponent read FProvider write SetProvider;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  BI.Store.Component;

{ TDataCollectionItem }

Destructor TDataCollectionItem.Destroy;
begin
  FOnChange:=nil;
  RemoveNotify;
  inherited;
end;

procedure TDataCollectionItem.BeginUpdate;
begin
  Inc(IUpdating);
end;

procedure TDataCollectionItem.Changed;
begin
  if Assigned(FOnChange) then
     FOnChange(Self);
end;

procedure TDataCollectionItem.EndUpdate;
begin
  Dec(IUpdating);

  if IUpdating=0 then
     Changed;
end;

procedure TDataCollectionItem.Assign(Source: TPersistent);
var tmp : TDataCollectionItem;
begin
  if Source is TDataCollectionItem then
  begin
    tmp:=TDataCollectionItem(Source);
    Provider:=tmp.Provider;
  end
  else
    inherited;
end;

procedure TDataCollectionItem.Notify(const AEvent: TBIEvent);
begin
  if AEvent=TBIEvent.Destroyed then
  begin
    IOrigin:=Origin;
    FData:=nil;
  end;

  Changed;
end;

procedure TDataCollectionItem.NotifyDataDestroy(const AEvent:TBIEvent);
begin
  if AEvent=TBIEvent.Destroyed then
     FData:=nil;
end;

procedure TDataCollectionItem.ReadOrigin(Reader: TReader);
begin
  IOrigin:=Reader.ReadString;
end;

function TDataCollectionItem.Origin:String;
begin
  result:=TComponentImporter.Origin(Provider,Data);
end;

procedure TDataCollectionItem.WriteOrigin(Writer: TWriter);
begin
  Writer.WriteString(Origin);
end;

procedure TDataCollectionItem.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor<>nil then
    begin
      result:=True;

      if Filer.Ancestor is TDataCollectionItem then
         result:=TDataCollectionItem(Filer.Ancestor).Data<>Data;
    end
    else
      result:=Data<>nil;
  end;

  function NeedsOrigin:Boolean;
  begin
    if Data=nil then
       result:=True
    else
       result:=TComponentImporter.DataOfProvider(Data)<>Data;
  end;

begin
  inherited;
  Filer.DefineProperty('Origin', ReadOrigin, WriteOrigin, NeedsOrigin and DoWrite);
end;

function TDataCollectionItem.GetData: TDataItem;
begin
  if FData=nil then
  begin
    if IOrigin<>'' then
    begin
      BeginUpdate;
      try
        Data:=LoadOrigin;
      finally
        Dec(IUpdating); // do not force Changed
      end;
    end;
  end;

  result:=FData;
end;

function TDataCollectionItem.GetDisplayName: String;
begin
  result:=Name;

  if result='' then
  begin
    if Provider<>nil then
    begin
      if Provider is TDataProvider then
         result:=TDataProvider(Provider).Title;

      if result='' then
         result:=Provider.ClassName;
    end
    else
    if Data<>nil then
       result:=Data.Name;

    if result='' then
       result:=inherited;
  end;
end;

type
  TDataProviderAccess=class(TDataProvider);
  TDataAccess=class(TDataItem);

procedure TDataCollectionItem.RemoveNotify;
begin
  if FProvider is TDataProvider then
     TDataProviderAccess(Provider).FConsumers.Remove(Notify);

  if FData<>nil then
  begin
    TDataAccess(FData).FConsumers.Remove(Notify);
    TDataAccess(FData).FConsumers.Remove(NotifyDataDestroy);
  end;
end;

procedure TDataCollectionItem.AddNotify;
begin
  if Provider is TDataProvider then
  begin
    TDataProviderAccess(Provider).FConsumers.Add(Notify);

    if FData<>nil then
       TDataAccess(FData).FConsumers.Add(NotifyDataDestroy);
  end
  else
  if FData<>nil then
     TDataAccess(FData).FConsumers.Add(Notify);
end;

procedure TDataCollectionItem.Loaded;
begin
  if IOrigin<>'' then
     Data;
end;

function TDataCollectionItem.LoadOrigin:TDataItem;
begin
  result:=TComponentImporter.TryLoadOrigin(Owner,Provider,IOrigin);
end;

procedure TDataCollectionItem.SetDataDirect(const Value: TDataItem);
var tmp : TComponent;
begin
  tmp:=TComponentImporter.ProviderOf(Value{FData});

  if tmp<>FProvider then
     InternalSetProvider(tmp);

  if tmp=nil then
     IOrigin:=''
  else
     IOrigin:=Origin;
end;

procedure TDataCollectionItem.SetData(const Value: TDataItem);
begin
  if FData<>Value then
  begin
    ValidateData(Value);

    RemoveNotify;
    FData:=Value;
    AddNotify;

    SetDataDirect(Value);
    Changed;
  end;
end;

function TDataCollectionItem.Owner:TComponent;
begin
  if (Collection<>nil) and (Collection.Owner is TComponent) then
     result:=TComponent(Collection.Owner)
  else
     result:=nil;
end;

procedure TDataCollectionItem.InternalSetProvider(const Value: TComponent);
var tmp : TComponent;
begin
  tmp:=Owner;

  if (tmp<>nil) and (FProvider<>nil) then
     FProvider.RemoveFreeNotification(tmp);

  FProvider:=Value;

  if (tmp<>nil) and (FProvider<>nil) then
     FProvider.FreeNotification(tmp);
end;

procedure TDataCollectionItem.SetProvider(const Value: TComponent);
begin
  if Provider<>Value then
  begin
    InternalSetProvider(Value);
    Data:=TComponentImporter.From(Owner,FProvider,IOrigin);
  end;
end;

procedure TDataCollectionItem.ValidateData(const AData: TDataItem);
begin
end;

end.

