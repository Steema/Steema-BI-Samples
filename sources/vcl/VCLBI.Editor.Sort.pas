{*********************************************}
{  TeeBI Software Library                     }
{  Sort Editor Dialog                         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Sort;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.Query,
  VCLBI.Editor.ListItems, Vcl.StdCtrls, Vcl.ExtCtrls, BI.DataItem,
  BI.Expressions;

type
  TSortEditor = class(TForm)
    Panel1: TPanel;
    CBAscending: TCheckBox;
    BDelete: TButton;
    Splitter1: TSplitter;
    CBIgnoreCase: TCheckBox;
    LBAvailable: TListBox;
    PanelButtons: TPanel;
    PanelOk: TPanel;
    BOk: TButton;
    BCancel: TButton;
    BAdd: TButton;
    procedure FormShow(Sender: TObject);
    procedure CBAscendingClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure LBAvailableClick(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure CBIgnoreCaseClick(Sender: TObject);
  private
    { Private declarations }

    FOnChanged : TNotifyEvent;
    Sort : TQuerySort;

    IList : TFormListItems;

    IChanging : Boolean;

    procedure AddItem(const AItem:TQuerySortItem);
    procedure AddItems;
    function Current:TQuerySortItem;
    procedure CheckedAll(Sender: TObject);
    procedure CheckedItem(Sender: TObject);
    procedure ExchangedItem(const Sender:TObject; const A,B:Integer);
    procedure Modified;
    procedure SelectedItem(Sender: TObject);
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent;
                        const ASort:TQuerySort):Boolean; overload; static;

    class function Edit(const AOwner: TComponent;
                        const AData:TDataItem;
                        out AItems:TSortItems):Boolean; overload; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const ASort:TQuerySort):TSortEditor; static;

    procedure Refresh(const ASort:TQuerySort);

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

{$R *.dfm}

uses
  VCLBI.Grid;

{ TSortEditor }

procedure TSortEditor.Modified;
begin
  if Assigned(FOnChanged) then
     FOnChanged(Self);
end;

function TSortEditor.Current:TQuerySortItem;
var tmp : Integer;
begin
  tmp:=IList.LBItems.ItemIndex;

  if tmp=-1 then
     result:=nil
  else
     result:=TQuerySortItem(IList.LBItems.Items.Objects[tmp]);
end;

procedure TSortEditor.BAddClick(Sender: TObject);
var tmp : TDataItem;
    tmpIndex : Integer;
begin
  tmpIndex:=LBAvailable.ItemIndex;
  tmp:=TDataItem(LBAvailable.Items.Objects[tmpIndex]);
  AddItem(Sort.AddSort(tmp));

  LBAvailable.Items.Delete(tmpIndex);
  LBAvailableClick(Self);

  IList.EnableCheckButtons;

  IList.LBItems.ItemIndex:=IList.LBItems.Count-1;
  IList.LBItemsClick(Self);
end;

function CheckName(const AName:String):String;
begin
  result:=AName;

  if result='' then
     result:='?';
end;

procedure TSortEditor.BDeleteClick(Sender: TObject);
var tmp : TDataItem;
    tmpIndex : Integer;
begin
  tmp:=Current.Data;
  Current.Free;

  tmpIndex:=LBAvailable.Items.AddObject(CheckName(tmp.Name),tmp);
  LBAvailable.ItemIndex:=tmpIndex;
  LBAvailableClick(Self);

  {$IFDEF FPC}
  if IList.LBItems.ItemIndex<>-1 then
     IList.LBItems.Items.Delete(IList.LBItems.ItemIndex);
  {$ELSE}
  IList.LBItems.DeleteSelected;
  {$ENDIF}

  IList.LBItemsClick(Self);
  IList.EnableCheckButtons;

  BDelete.Enabled:=False;
  Modified;
end;

procedure TSortEditor.CBAscendingClick(Sender: TObject);
begin
  if not IChanging then
  begin
    Current.Ascending:=CBAscending.Checked;
    Modified;
  end;
end;

procedure TSortEditor.CBIgnoreCaseClick(Sender: TObject);
begin
  if not IChanging then
  begin
    Current.IgnoreTextCase:=CBIgnoreCase.Checked;
    Modified;
  end;
end;

procedure TSortEditor.CheckedAll(Sender: TObject);
var t : Integer;
begin
  for t:=0 to IList.LBItems.Count-1 do
      Sort[t].Enabled:=IList.LBItems.Checked[t];

  Modified;
end;

procedure TSortEditor.CheckedItem(Sender: TObject);
begin
  Current.Enabled:=IList.LBItems.Checked[IList.LBItems.ItemIndex];
  Modified;
end;

class function TSortEditor.Edit(const AOwner: TComponent;
  const ASort: TQuerySort):Boolean;
begin
  with TSortEditor.Create(AOwner) do
  try
    Refresh(ASort);

    PanelButtons.Visible:=True;
    PanelButtons.Align:=alBottom;

    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

type
  TQuerySortAccess=class(TQuerySort);

class function TSortEditor.Edit(const AOwner: TComponent;
  const AData:TDataItem; out AItems:TSortItems):Boolean;
var tmpQuery : TBIQuery;
begin
  tmpQuery:=TBIQuery.Create(nil);
  try
    tmpQuery.Dimensions.Add(AData);
    result:=Edit(AOwner,tmpQuery.SortBy);

    if result then
       AItems:=TQuerySortAccess(tmpQuery.SortBy).Sort;
  finally
    tmpQuery.Free;
  end;
end;

class function TSortEditor.Embedd(const AOwner: TComponent;
  const AParent: TWinControl; const ASort: TQuerySort): TSortEditor;
begin
  result:=TSortEditor.Create(AOwner);
  result.Refresh(ASort);

  TUICommon.AddForm(result,AParent);
end;

procedure TSortEditor.SelectedItem(Sender: TObject);
var tmp : TQuerySortItem;
begin
  CBAscending.Enabled:=Current<>nil;
  CBIgnoreCase.Enabled:=CBAscending.Enabled;
  BDelete.Enabled:=CBAscending.Enabled;

  if CBAscending.Enabled then
  begin
    tmp:=Current;

    IChanging:=True;
    try
      CBAscending.Checked:=tmp.Ascending;
      CBIgnoreCase.Checked:=tmp.IgnoreTextCase;
    finally
      IChanging:=False;
    end;
  end;
end;

procedure TSortEditor.FormCreate(Sender: TObject);
begin
  IList:=TFormListItems.Create(Self);

  IList.OnSelected:=SelectedItem;
  IList.OnChecked:=CheckedItem;
  IList.OnCheckedAll:=CheckedAll;
  IList.OnExchanged:=ExchangedItem;
end;

procedure TSortEditor.ExchangedItem(const Sender:TObject; const A,B:Integer);
begin
  Sort[A].Index:=Sort[B].Index;
  Modified;
end;

procedure TSortEditor.FormShow(Sender: TObject);
begin
  TUICommon.AddForm(IList,Self);
end;

procedure TSortEditor.LBAvailableClick(Sender: TObject);
begin
  BAdd.Enabled:=LBAvailable.ItemIndex<>-1;
end;

procedure TSortEditor.AddItem(const AItem:TQuerySortItem);
var tmp : Integer;
    tmpS : String;
begin
  if AItem.Data=nil then
     tmpS:=''
  else
     tmpS:=AItem.Data.Name;

  tmp:=IList.LBItems.Items.AddObject(CheckName(tmpS),AItem);
  IList.Check(tmp,AItem.Enabled);

  if PanelButtons.Visible then
     BOk.Enabled:=True;
end;

procedure TSortEditor.AddItems;

  procedure Add(const AData:TDataItem);
  begin
    LBAvailable.Items.AddObject(CheckName(AData.Name),AData);
  end;

  procedure AddAvailable(const AItem:TQueryItem);
  var t : Integer;
  begin
    if AItem.Data<>nil then
       if Sort.ItemOf(AItem.Data)=nil then
          if AItem.Data.AsTable then
             for t:=0 to AItem.Data.Items.Count-1 do
                 Add(AItem.Data.Items[t])
          else
             Add(AItem.Data);
  end;

  procedure AddAllAvailable;
  var t : Integer;
  begin
    LBAvailable.Items.BeginUpdate;
    try
      LBAvailable.Clear;
      LBAvailable.Sorted:=False;

      for t:=0 to Sort.Query.Dimensions.Count-1 do
          AddAvailable(Sort.Query.Dimensions[t]);

      for t:=0 to Sort.Query.Measures.Count-1 do
          AddAvailable(Sort.Query.Measures[t]);

      LBAvailable.Sorted:=True;
    finally
      LBAvailable.Items.EndUpdate;
    end;

    LBAvailableClick(Self);
  end;

  procedure AddSortItems;
  var t : Integer;
  begin
    IList.LBItems.Clear;

    for t:=0 to Sort.Count-1 do
        AddItem(Sort[t]);

    IList.LBItemsClick(Self);
  end;

begin
  AddSortItems;
  AddAllAvailable;
end;

procedure TSortEditor.Refresh(const ASort: TQuerySort);
begin
  Sort:=ASort;
  AddItems;
end;

end.
