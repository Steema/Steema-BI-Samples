{*********************************************}
{  TeeBI Software Library                     }
{  New Data Column editor dialog              }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.NewColumn;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.DataItem, BI.Arrays;

type
  TAddColumn = class(TForm)
    Label1: TLabel;
    EName: TEdit;
    Panel1: TPanel;
    RGStyle: TRadioGroup;
    Label2: TLabel;
    EExpression: TEdit;
    Panel2: TPanel;
    BOk: TButton;
    Button2: TButton;
    CBKind: TComboBox;
    LError: TLabel;
    CBLookup: TComboBox;
    CBDetail: TComboBox;
    procedure ENameChange(Sender: TObject);
    procedure RGStyleClick(Sender: TObject);
    procedure EExpressionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBKindChange(Sender: TObject);
    procedure CBLookupChange(Sender: TObject);
    procedure CBDetailChange(Sender: TObject);
  private
    { Private declarations }

    IData : TDataItem;

    procedure AddKinds;
    procedure AddLookups;
    procedure AddDetails;
    function CurrentDetail:TDataItem;
    function CurrentLookup:TDataItem;
    function FullName(const AData:TDataItem):String;
    procedure InitMissing(const ACol:TDataItem);
    function TryExpression(const Fill:Boolean):TDataItem;
  public
    { Public declarations }
    Constructor CreateData(const AOwner:TComponent; const AData:TDataItem);

    class function NewColumn(const AOwner:TComponent; const AData:TDataItem):TDataItem;
  end;

implementation

{$R *.dfm}

uses
  BI.UI, BI.Persist, BI.Expressions;

const
  TypeData=0;
  TypeLookup=1;
  TypeDetail=2;
  TypeExpression=3;

type
  TDataAccess=class(TDataItem);

{ TAddColumn }

procedure TAddColumn.AddDetails;
var t,tt : Integer;
    tmp : TDataItem;
    tmpData : TDataItem;
begin
  CBDetail.Items.BeginUpdate;
  try
    CBDetail.Clear;

    tmpData:=IData.Parent as TDataItem;

    for t:=0 to tmpData.Items.Count-1 do
    begin
      tmp:=tmpData.Items[t];

      if IData<>tmp then
      begin
        for tt:=0 to tmp.Items.Count-1 do
            if TDataAccess(tmp.Items[tt]).HasMaster then
               if tmp.Items[tt].Master=IData then
               begin
                 CBDetail.Items.AddObject(tmp.Name,tmp);
                 break;
               end;
      end;
    end;

  finally
    CBDetail.Items.EndUpdate;
  end;
end;

function TAddColumn.FullName(const AData:TDataItem):String;
var tmp : TDataItem;
begin
  tmp:=AData;
  result:=tmp.Name;

  while tmp.Parent<>nil do
  begin
    tmp:=tmp.Parent;

    if tmp=TStores.GlobalCache then
       break
    else
       result:=tmp.Name+' '+result;
  end;
end;

procedure TAddColumn.AddLookups;

  procedure AddData(const AData:TDataItem);

    function Exists(const AData:TDataItem):Boolean;
    var t : Integer;
    begin
      for t:=0 to CBLookup.Items.Count-1 do
          if TDataItem(CBLookup.Items.Objects[t]).Parent=AData then
             Exit(True);

      result:=False;
    end;

    (*
    procedure AddInvertedTo(const AData:TDataItem);
    var t,tt : Integer;
        tmp : TDataItem;
    begin
      for t:=0 to AData.Parent.Items.Count-1 do
      begin
        tmp:=AData.Parent.Items[t];

        if tmp<>AData then
           if not Exists(tmp) then
              for tt:=0 to tmp.Items.Count-1 do
                  if tmp.Items[tt].Master=AData then
                  begin
                    AddData(tmp);
                    break;
                  end;
      end;
    end;

    procedure Add(const AItems:TDataItems; const AExcept:TDataItem);
    var Item : TDataItem;
    begin
      for Item in AItems.AsArray do
        //if Item<>AExcept then
        begin
          if TDataAccess(Item).HasMaster then
          begin
            AddData(Item.Master);

            // 1-to-1 inverted relation
            if Item.Master.Master=Item then
               AddInvertedTo(Item.Master.Master);
          end;
        end;
    end;
    *)

    procedure AddAllItems(const AItems:TDataItems);
    var Item : TDataItem;
        tmp : TDataItem;
    begin
      for Item in AItems.AsArray do
      begin
        if Item.Parent<>IData then
           CBLookup.Items.AddObject(FullName(Item),Item);

        if TDataAccess(Item).HasMaster then
        begin
          tmp:=Item.Master.Parent;

          if tmp<>nil then
             AddData(tmp);
        end;
      end;
    end;

  begin
    if not Exists(AData) then
       if AData.AsTable then
          AddAllItems(AData.Items);
  end;

begin
  CBLookup.Items.BeginUpdate;
  try
    CBLookup.Clear;

    AddData(IData);
  finally
    CBLookup.Items.EndUpdate;
  end;
end;

procedure TAddColumn.CBDetailChange(Sender: TObject);
begin
  if CBDetail.ItemIndex>-1 then
     if EName.Text='' then
        EName.Text:=FullName(CurrentDetail);

  ENameChange(Self);
end;

procedure TAddColumn.CBKindChange(Sender: TObject);
begin
  ENameChange(Self);
end;

function TAddColumn.CurrentDetail:TDataItem;
begin
  if CBDetail.ItemIndex=-1 then
     result:=nil
  else
     result:=TDataItem(CBDetail.Items.Objects[CBDetail.ItemIndex]);
end;

function TAddColumn.CurrentLookup:TDataItem;
begin
  if CBLookup.ItemIndex=-1 then
     result:=nil
  else
     result:=TDataItem(CBLookup.Items.Objects[CBLookup.ItemIndex]);
end;

procedure TAddColumn.CBLookupChange(Sender: TObject);
begin
  if CBLookup.ItemIndex>-1 then
  begin
    if EName.Text='' then
       EName.Text:=FullName(CurrentLookup);

    CBKind.ItemIndex:=Ord(CurrentLookup.Kind);
  end
  else
    CBKind.ItemIndex:=-1;

  ENameChange(Self);
end;

constructor TAddColumn.CreateData(const AOwner: TComponent; const AData: TDataItem);
begin
  inherited Create(AOwner);
  IData:=AData;
end;

function TAddColumn.TryExpression(const Fill:Boolean):TDataItem;
begin
  result:=TExpressionColumn.From(IData,EExpression.Text,Fill,EName.Text
    {$IFNDEF FPC}
    ,
    function(const APos:Integer; const AMessage:String):Boolean
    begin
      LError.Caption:=AMessage+' at position: '+IntToStr(APos);
      result:=True;
    end
    {$ENDIF});

  if result<>nil then
  begin
    LError.Caption:='';
    IData.Items.Add(result);
  end;
end;

procedure TAddColumn.EExpressionChange(Sender: TObject);
var tmp : TDataItem;
begin
  ENameChange(Self);

  tmp:=TryExpression(False);
  try
    if tmp=nil then
       CBKind.ItemIndex:=-1
    else
       CBKind.ItemIndex:=Ord(tmp.Kind);
  finally
    tmp.Free;
  end;
end;

procedure TAddColumn.ENameChange(Sender: TObject);
begin
  BOk.Enabled:= (Trim(EName.Text)<>'') and
      (
        (RGStyle.ItemIndex=TypeData)
        or
        (Trim(EExpression.Text)<>'')
        or
        (
          (RGStyle.ItemIndex=TypeLookup)
          and
          (CBLookup.ItemIndex<>-1)
        )
        or
        (
          (RGStyle.ItemIndex=TypeDetail)
          and
          (CBDetail.ItemIndex<>-1)
        )
      );
end;

procedure TAddColumn.AddKinds;
begin
  TCommonUI.AddKinds(CBKind.Items);
  CBKind.ItemIndex:=5; // Default: Text
end;

procedure TAddColumn.FormCreate(Sender: TObject);
begin
  AddKinds;
  AddLookups;
  AddDetails;
end;

procedure TAddColumn.InitMissing(const ACol:TDataItem);
begin
  if ACol.Parent<>nil then
  begin
    ACol.Resize(ACol.Parent.Count);
    ACol.Missing.Init(ACol.Count);
  end;
end;

class function TAddColumn.NewColumn(const AOwner: TComponent;
  const AData: TDataItem): TDataItem;
begin
  result:=nil;

  with TAddColumn.CreateData(AOwner,AData) do
  try
    if ShowModal=mrOk then
    begin
      case RGStyle.ItemIndex of
        TypeData : begin
                     result:=AData.Items.New(EName.Text,TDataKind(CBKind.ItemIndex));
                     InitMissing(result);
                   end;

  TypeExpression : result:=TryExpression(True);

      TypeLookup : result:=TDataItemExpression.NewLookup(EName.Text,IData,CurrentLookup);

      TypeDetail : result:=TDetailData.Create(CurrentDetail,EName.Text);
      end;

      if result<>nil then
         AData.Items.Add(result);
    end;
  finally
    Free;
  end;
end;

procedure TAddColumn.RGStyleClick(Sender: TObject);
begin
  CBKind.Enabled:=RGStyle.ItemIndex=TypeData;
  CBLookup.Enabled:=RGStyle.ItemIndex=TypeLookup;
  CBDetail.Enabled:=RGStyle.ItemIndex=TypeDetail;
  EExpression.Enabled:=RGStyle.ItemIndex=TypeExpression;

  ENameChange(Self);

  if EExpression.Enabled then
     EExpression.SetFocus
  else
  if RGStyle.ItemIndex=TypeData then
     CBKind.SetFocus
  else
  if RGStyle.ItemIndex=TypeLookup then
     CBLookup.SetFocus
  else
  if RGStyle.ItemIndex=TypeDetail then
     CBDetail.SetFocus
end;

end.
