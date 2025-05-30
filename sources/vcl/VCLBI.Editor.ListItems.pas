{*********************************************}
{  TeeBI Software Library                     }
{  Items Selector Editor Dialog               }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.ListItems;

interface

// This dialog can be used to select and re-order child Items of a TDataItem.

(*
  Using this dialog does not modify the Data.

  Use the Items function to obtain the array of checked items with the
  desired order.
*)

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.CheckLst, BI.DataItem;

type
  TListItemsEvent=procedure(const AItems:TDataArray; const ARefresh:Boolean) of object;

  TListExchangedEvent=procedure(const Sender:TObject; const A,B:Integer) of object;

  TFormListItems = class(TForm)
    LBItems: TCheckListBox;
    Panel1: TPanel;
    SBUp: TSpeedButton;
    SBDown: TSpeedButton;
    BAll: TButton;
    BNone: TButton;
    PanelButtons: TPanel;
    PanelOk: TPanel;
    BOk: TButton;
    BCancel: TButton;
    procedure LBItemsClick(Sender: TObject);
    procedure LBItemsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LBItemsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LBItemsClickCheck(Sender: TObject);
    procedure SBDownClick(Sender: TObject);
    procedure SBUpClick(Sender: TObject);
    procedure BAllClick(Sender: TObject);
    procedure BNoneClick(Sender: TObject);
  private
    { Private declarations }

    FOnChanged,
    FOnSelected,
    FOnChecked,
    FOnCheckedAll : TNotifyEvent;

    FOnExchanged : TListExchangedEvent;

    procedure DoChanged;
    procedure Exchange(const A,B:Integer);
    procedure Recreate;

  public
    { Public declarations }

    class procedure AddCombo(const ACombo:TComboBox; const AData:Array of TDataItem); static;

    class procedure AddItems(const AItems:TStrings; const AData:Array of TDataItem;
                             const AddBlank:Boolean=True); overload; static;

    procedure AddItems(const AData:Array of TDataItem;
                       const AddBlank:Boolean=True); overload;

    class procedure AddMap(const AData: TDataItem; const AItems:TStrings); overload; static;
    procedure AddMap(const AData:TDataItem); overload;

    procedure Check(const AIndex:Integer; const ACheck:Boolean);
    procedure CheckAll(const ACheck:Boolean=True);
    function CheckedCount:Integer;

    class function Edit(const AOwner:TComponent;
                        var AItems:TDataArray;
                        const AllChecked:Boolean=True):Boolean; static;

    class function Embed(const AOwner:TComponent; const AParent:TWinControl):TFormListItems; static;

    procedure EnableCheckButtons;
    procedure EnableDrag(const AEnabled:Boolean);

    class procedure DoAddItems(const AItems:TStrings; const AData:Array of TDataItem); static;

    function Items:TDataArray;

    procedure TryAdd(const AData:TDataItem);

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
    property OnChecked:TNotifyEvent read FOnChecked write FOnChecked;
    property OnCheckedAll:TNotifyEvent read FOnCheckedAll write FOnCheckedAll;
    property OnExchanged:TListExchangedEvent read FOnExchanged write FOnExchanged;
    property OnSelected:TNotifyEvent read FOnSelected write FOnSelected;
  end;

implementation

{$R *.dfm}

uses
  System.Types, BI.Arrays, VCLBI.Grid;

procedure TFormListItems.LBItemsClick(Sender: TObject);
begin
  SBUp.Enabled:=LBItems.ItemIndex>0;
  SBDown.Enabled:=LBItems.ItemIndex<LBItems.Items.Count-1;

  if Assigned(FOnSelected) then
     FOnSelected(Self);
end;

function TFormListItems.Items:TDataArray;
var t : Integer;
begin
  result:=nil;

  for t:=0 to LBItems.Count-1 do
      if LBItems.Checked[t] then
         result.Add(TDataItem(LBItems.Items.Objects[t]));
end;

function TFormListItems.CheckedCount:Integer;
var t : Integer;
begin
  result:=0;

  for t:=0 to LBItems.Count-1 do
      if LBItems.Checked[t] then
         Inc(result);
end;

class function TFormListItems.Edit(const AOwner: TComponent;
                                   var AItems: TDataArray;
                                   const AllChecked:Boolean=True): Boolean;
begin
  with TFormListItems.Create(AOwner) do
  try
    AddItems(AItems,False);

    CheckAll(AllChecked);

    PanelButtons.Visible:=True;

    BOk.Enabled:=False;

    result:=ShowModal=mrOk;

    if result then
       AItems:=Items;
  finally
    Free;
  end;
end;

class function TFormListItems.Embed(const AOwner: TComponent;
  const AParent: TWinControl): TFormListItems;
begin
  result:=TFormListItems.Create(AOwner);
  TUICommon.AddForm(result,AParent);
end;

procedure TFormListItems.EnableCheckButtons;
begin
  BAll.Enabled:=CheckedCount<LBItems.Count;
  BNone.Enabled:=CheckedCount>0;
end;

procedure TFormListItems.DoChanged;
begin
  if PanelButtons.Visible then
     BOk.Enabled:=True;

  if Assigned(FOnChanged) then
     FOnChanged(Self);
end;

procedure TFormListItems.Recreate;
begin
  EnableCheckButtons;
  DoChanged;
end;

procedure TFormListItems.SBDownClick(Sender: TObject);
begin
  Exchange(LBItems.ItemIndex,LBItems.ItemIndex+1);
  LBItemsClick(Self);
end;

procedure TFormListItems.SBUpClick(Sender: TObject);
begin
  Exchange(LBItems.ItemIndex,LBItems.ItemIndex-1);
  LBItemsClick(Self);
end;

procedure TFormListItems.Check(const AIndex: Integer; const ACheck: Boolean);
begin
  LBItems.Checked[AIndex]:=ACheck;
end;

procedure TFormListItems.CheckAll(const ACheck:Boolean);
begin
  if ACheck then
     LBItems.CheckAll(TCheckBoxState.cbChecked)
  else
     LBItems.CheckAll(TCheckBoxState.cbUnchecked);

  Recreate;

  if Assigned(FOnCheckedAll) then
     FOnCheckedAll(Self);
end;

procedure TFormListItems.LBItemsClickCheck(Sender: TObject);
begin
  EnableCheckButtons;

  if Assigned(FOnChecked) then
     FOnChecked(Self);

  DoChanged;
end;

procedure TFormListItems.EnableDrag(const AEnabled: Boolean);
begin
  if AEnabled then
  begin
    SBUp.Show;
    SBDown.Show;

    BAll.Left:=SBDown.Left+SBDown.Width+12;

    LBItems.OnDragOver:=LBItemsDragOver;
    LBItems.OnDragDrop:=LBItemsDragDrop;
  end
  else
  begin
    SBUp.Hide;
    SBDown.Hide;

    BAll.Left:=8;

    LBItems.OnDragOver:=nil;
    LBItems.OnDragDrop:=nil;
  end;

  BNone.Left:=BAll.Left+BAll.Width+12;
end;

procedure TFormListItems.Exchange(const A,B:Integer);
begin
  LBItems.Items.Exchange(A,B);

  if Assigned(FOnExchanged) then
     FOnExchanged(Self,A,B);

  Recreate;
end;

procedure TFormListItems.LBItemsDragDrop(Sender, Source: TObject; X, Y: Integer);
var tmp : Integer;
begin
  tmp:=LBItems.ItemAtPos(TUICommon.Point(X,Y),True);

  if tmp<>-1 then
     if tmp<>LBItems.ItemIndex then
        Exchange(tmp,LBItems.ItemIndex);
end;

procedure TFormListItems.LBItemsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var tmp : Integer;
begin
  tmp:=LBItems.ItemAtPos(TUICommon.Point(X,Y),True);

  Accept:=(Sender=Source) and (LBItems.ItemIndex<>-1) and (tmp<>-1) and
          (tmp<>LBItems.ItemIndex);
end;

procedure TFormListItems.TryAdd(const AData:TDataItem);
var t : Integer;
begin
  for t:=0 to LBItems.Count-1 do
      if LBItems.Items.Objects[t]=AData then
         if not LBItems.Checked[t] then
         begin
           Check(t,True);
           DoChanged;
           break;
         end;

  EnableCheckButtons;
end;

class procedure TFormListItems.DoAddItems(const AItems:TStrings; const AData:Array of TDataItem);
var tmp : TDataItem;
begin
  for tmp in AData do
      if tmp<>nil then
         AItems.AddObject(tmp.Name,tmp);
end;

procedure TFormListItems.AddItems(const AData:Array of TDataItem;
                                  const AddBlank:Boolean=True);
begin
  AddItems(LBItems.Items,AData,AddBlank);
  EnableCheckButtons;
end;

class procedure TFormListItems.AddMap(const AData: TDataItem; const AItems:TStrings);
var tmp : TDataMap;
    t : TLoopInteger;
begin
  AData.Stats;

  tmp:=AData.DataMap;

  AItems.BeginUpdate;
  try
    AItems.Clear;

    for t:=0 to tmp.Count-1 do
        AItems.Add(tmp.AsString(t));
  finally
    AItems.EndUpdate;
  end;
end;

procedure TFormListItems.AddMap(const AData: TDataItem);
begin
  AddMap(AData,LBItems.Items);
  EnableCheckButtons;
end;

class procedure TFormListItems.AddItems(const AItems:TStrings; const AData:Array of TDataItem;
                                        const AddBlank:Boolean=True);
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;

    if Length(AData)>0 then
    begin
      if AddBlank then
         AItems.Add('');

      DoAddItems(AItems,AData);
    end;
  finally
    AItems.EndUpdate;
  end;
end;

procedure TFormListItems.BAllClick(Sender: TObject);
begin
  CheckAll(True);
end;

procedure TFormListItems.BNoneClick(Sender: TObject);
begin
  CheckAll(False);
end;

class procedure TFormListItems.AddCombo(const ACombo:TComboBox; const AData:Array of TDataItem);
begin
  AddItems(ACombo.Items,AData);

  if ACombo.Items.Count=0 then
     ACombo.Enabled:=False
  else
  if ACombo.Items.Count>1 then
     ACombo.ItemIndex:=1;
end;

end.
