{*********************************************}
{  TeeBI Software Library                     }
{  TDataSelect Editor                         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.DataSelect;

interface

uses
  {$IFNDEF FPC}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst, Vcl.Buttons,
  Vcl.ComCtrls, Vcl.Menus,
  BI.DataItem, BI.DataSource, BI.Expressions;

type
  TDataSelectEditor = class(TForm)
    PageControl1: TPageControl;
    TabItems: TTabSheet;
    Panel2: TPanel;
    BUp: TSpeedButton;
    BDown: TSpeedButton;
    BAdd: TButton;
    BRemoveItem: TButton;
    LItems: TCheckListBox;
    TabSort: TTabSheet;
    TabFilter: TTabSheet;
    LBSort: TCheckListBox;
    EFilter: TEdit;
    Panel3: TPanel;
    BUpSort: TSpeedButton;
    BDownSort: TSpeedButton;
    BAddSort: TButton;
    BRemoveSort: TButton;
    PopupSort: TPopupMenu;
    Data1: TMenuItem;
    Expression1: TMenuItem;
    Panel1: TPanel;
    CBDistinct: TCheckBox;
    LFilter: TLabel;
    Label1: TLabel;
    EMax: TEdit;
    Panel4: TPanel;
    LSortError: TLabel;
    ESortExpression: TEdit;
    LSortExpression: TLabel;
    CBIgnoreCase: TCheckBox;
    CBAscending: TCheckBox;
    Panel5: TPanel;
    Label2: TLabel;
    EItemExpression: TEdit;
    LItemError: TLabel;
    PopupItems: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    TabSQL: TTabSheet;
    MemoSQL: TMemo;
    Label3: TLabel;
    EItemName: TEdit;
    PanelButtons: TPanel;
    Panel9: TPanel;
    BOK: TButton;
    Button2: TButton;
    procedure CBDistinctClick(Sender: TObject);
    procedure LItemsClick(Sender: TObject);
    procedure LItemsClickCheck(Sender: TObject);
    procedure BRemoveItemClick(Sender: TObject);
    procedure BUpClick(Sender: TObject);
    procedure BDownClick(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure LBSortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BAddSortClick(Sender: TObject);
    procedure BRemoveSortClick(Sender: TObject);
    procedure BUpSortClick(Sender: TObject);
    procedure BDownSortClick(Sender: TObject);
    procedure Expression1Click(Sender: TObject);
    procedure Data1Click(Sender: TObject);
    procedure ESortExpressionChange(Sender: TObject);
    procedure EFilterChange(Sender: TObject);
    procedure CBIgnoreCaseClick(Sender: TObject);
    procedure CBAscendingClick(Sender: TObject);
    procedure EMaxChange(Sender: TObject);
    procedure LItemsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LItemsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LBSortDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LBSortDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure EItemExpressionChange(Sender: TObject);
    procedure LBSortClickCheck(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure EItemNameChange(Sender: TObject);
  private
    { Private declarations }

    ISelect : TDataSelect;
    FOnChanged : TNotifyEvent;

    IChanging : Boolean;

    procedure AddItem(const AData:TDataItem);

    function ChangeExpression(const AData:TDataItem; const AError:TLabel;
                              const AText:String;
                              out AExpression:TExpressionColumn):Boolean;

    function ChooseSortItem:TDataItem;
    procedure DoChanged;

    procedure FillItems;
    procedure FillSortBy;

    procedure SetExpressionText(const AEdit:TEdit; const AData:TDataItem);
    procedure SwapItems(const A,B:Integer);
    procedure SwapSort(const A,B:Integer);
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent;
                        const ASelect:TDataSelect;
                        const OnChanged:TNotifyEvent=nil):Boolean; static;

    procedure Refresh(const ASelect:TDataSelect);

    property Select:TDataSelect read ISelect;
    property OnChange:TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

{$R *.dfm}

uses
  VCLBI.DataManager, VCLBI.Grid, BI.Expression, BI.Languages.English,
  BI.SQL, VCLBI.Menus;

{ TDataSelectEditor }

procedure TDataSelectEditor.BUpSortClick(Sender: TObject);
begin
  SwapSort(LBSort.ItemIndex,LBSort.ItemIndex-1);
end;

procedure TDataSelectEditor.BDownSortClick(Sender: TObject);
begin
  SwapSort(LBSort.ItemIndex,LBSort.ItemIndex+1);
end;

procedure TDataSelectEditor.SwapItems(const A,B:Integer);
begin
  if A<>B then
  begin
    LItems.Items.Exchange(A,B);
    ISelect.Items.Exchange(A,B);

    DoChanged;

    LItemsClick(Self);
  end;
end;

procedure TDataSelectEditor.SwapSort(const A,B:Integer);
begin
  if A<>B then
  begin
    LBSort.Items.Exchange(A,B);
    ISelect.SortBy.Exchange(A,B);

    DoChanged;

    LBSortClick(Self);
  end;
end;

procedure TDataSelectEditor.AddItem(const AData:TDataItem);
var tmp : Integer;
begin
  ISelect.Add(AData);

  tmp:=LItems.Items.Add(AData.Name);
  LItems.Checked[tmp]:=True; //AData.Active;
end;

procedure TDataSelectEditor.BAddClick(Sender: TObject);
begin
  TBIMenu.Popup(PopupItems,BAdd);
end;

procedure TDataSelectEditor.BDownClick(Sender: TObject);
begin
  SwapItems(LItems.ItemIndex,LItems.ItemIndex+1);
end;

procedure TDataSelectEditor.BRemoveItemClick(Sender: TObject);

  procedure TryRemoveSort(const AData:TDataItem);
  var t : Integer;
  begin
    t:=0;

    while t<ISelect.SortBy.Count do
        if ISelect.SortBy.Items[t].Data=AData then
        begin
          ISelect.SortBy.Delete(t);
          LBSort.Items.Delete(t);
        end
        else
           Inc(t);
  end;

var tmp : Integer;
begin
  tmp:=LItems.ItemIndex;

  TryRemoveSort(ISelect.Items[tmp].Data);

  LItems.Items.Delete(tmp);
  ISelect.Items.Delete(tmp);

  LItemsClick(Self);
  DoChanged;
end;

procedure TDataSelectEditor.BUpClick(Sender: TObject);
begin
  SwapItems(LItems.ItemIndex,LItems.ItemIndex-1);
end;

procedure TDataSelectEditor.BAddSortClick(Sender: TObject);
begin
  TBIMenu.Popup(PopupSort,BAddSort);
end;

procedure TDataSelectEditor.BRemoveSortClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBSort.ItemIndex;
  LBSort.Items.Delete(tmp);
  ISelect.SortBy.Delete(tmp);

  LBSortClick(Self);
  DoChanged;
end;

procedure TDataSelectEditor.CBAscendingClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBSort.ItemIndex;
  ISelect.SortBy.Items[tmp].Descending:=not CBAscending.Checked;
  DoChanged;
end;

procedure TDataSelectEditor.CBDistinctClick(Sender: TObject);
begin
  if ISelect.Distinct<>CBDistinct.Checked then
  begin
    ISelect.Distinct:=CBDistinct.Checked;
    DoChanged;
  end;
end;

procedure TDataSelectEditor.CBIgnoreCaseClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBSort.ItemIndex;
  ISelect.SortBy.Items[tmp].IgnoreTextCase:=CBIgnoreCase.Checked;
  DoChanged;
end;

function TDataSelectEditor.ChooseSortItem:TDataItem;
begin
  // Pending: This should allow selecting one of the ISelect.Items
  result:=nil;
end;

procedure TDataSelectEditor.Data1Click(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=ChooseSortItem;

  if tmp<>nil then
  begin
    ISelect.SortBy.Add(tmp);
    LBSort.Items.Add(tmp.Name);
    LBSort.ItemIndex:=LBSort.Items.Count-1;
    LBSort.Checked[LBSort.ItemIndex]:=True;
  end;
end;

procedure TDataSelectEditor.DoChanged;
begin
  if not IChanging then
  begin
    MemoSQL.Lines.Text:=TBISQL.From(ISelect);

    if Assigned(FOnChanged) then
       FOnChanged(Self);
  end;
end;

class function TDataSelectEditor.Edit(const AOwner: TComponent;
                                      const ASelect: TDataSelect;
                                      const OnChanged:TNotifyEvent=nil): Boolean;
var tmp : TDataSelectEditor;
begin
  tmp:=TDataSelectEditor.Create(AOwner);
  try
    tmp.Refresh(ASelect);
    tmp.OnChange:=OnChanged;

    tmp.PanelButtons.Show;

    result:=tmp.ShowModal=mrOk;
  finally
    tmp.Free;
  end;
end;

procedure TDataSelectEditor.EFilterChange(Sender: TObject);

  procedure ChangeFilter(const AFilter:TExpression);
  begin
    ISelect.Filter:=AFilter;
    DoChanged;
  end;

var tmp : String;
    tmpFilter : TExpression;
begin
  LFilter.Caption:='';

  if ISelect.Filter=nil then
     tmp:=''
  else
     tmp:=ISelect.Filter.ToString;

  if tmp<>EFilter.Text then
  begin
    if Trim(EFilter.Text)='' then
       ChangeFilter(nil)
    else
    begin
      tmpFilter:=TDataFilter.FromString(ISelect.Data,EFilter.Text
                 {$IFNDEF FPC}
                 ,
                 function(const APos:Integer; const AMessage:String):Boolean
                 begin
                   LFilter.Caption:=IntToStr(APos)+': '+AMessage;
                   result:=True; // do not raise exception
                 end
                 {$ENDIF});

      if tmpFilter<>nil then
         ChangeFilter(tmpFilter);
    end;
  end;
end;

function TDataSelectEditor.ChangeExpression(const AData:TDataItem; const AError:TLabel;
   const AText:String;
   out AExpression:TExpressionColumn):Boolean;
var tmp : String;
    tmpExpression : TExpression;
begin
  result:=False;

  if AData is TExpressionColumn then
  begin
    AExpression:=AData as TExpressionColumn;

    if AExpression.Expression=nil then
       tmp:=''
    else
       tmp:=AExpression.Expression.ToString;
  end
  else
  begin
    AExpression:=nil;
    tmp:=AData.Name;
  end;

  AError.Caption:='';

  if tmp<>AText then
  begin
    tmpExpression:=TDataExpression.FromString(ISelect.MainData,AText
               {$IFNDEF FPC}
               ,
               function(const APos:Integer; const AMessage:String):Boolean
               begin
                 AError.Caption:=Format(BIMsg_ExpressionError,[AMessage,APos]);
                 result:=True; // do not raise exception
               end
               {$ENDIF}
               );

    if tmpExpression=nil then
       AError.Caption:='Empty expression'
    else
    begin
      if AExpression=nil then
         AExpression:=TExpressionColumn.Create(tmpExpression)
      else
      begin
        AExpression.Resize(0);
        AExpression.Expression:=tmpExpression;
      end;

      result:=True;
    end;
  end;
end;

procedure TDataSelectEditor.EItemExpressionChange(Sender: TObject);

  procedure TryReplaceSort(const AOld,ANew:TDataItem);
  var t : Integer;
  begin
    for t:=0 to ISelect.SortBy.Count-1 do
        if ISelect.SortBy.Items[t].Data=AOld then
           ISelect.SortBy.Items[t].Data:=ANew;
  end;

var tmp : Integer;
    tmpData : TDataItem;
    tmpExp : TExpressionColumn;
begin
  if not IChanging then
  begin
    tmp:=LItems.ItemIndex;
    tmpData:=ISelect.Items[tmp].Data;

    if ChangeExpression(tmpData,LItemError,EItemExpression.Text,tmpExp) then
    begin
      TryReplaceSort(ISelect.Items[tmp].Data,tmpExp);

      ISelect.Items[tmp].Data:=tmpExp;
      LItems.Items[tmp]:=tmpExp.Expression.ToString;
      DoChanged;
    end;
  end;
end;

procedure TDataSelectEditor.EItemNameChange(Sender: TObject);
var tmp : Integer;
begin
  if not IChanging then
  begin
    tmp:=LItems.ItemIndex;
    ISelect.Items[tmp].Name:=EItemName.Text;
    DoChanged;
  end;
end;

procedure TDataSelectEditor.EMaxChange(Sender: TObject);
var tmp : Int64;
begin
  if not IChanging then
  begin
    tmp:=StrToInt64Def(EMax.Text,ISelect.Max);

    if tmp<>ISelect.Max then
    begin
      ISelect.Max:=tmp;
      DoChanged;
    end;
  end;
end;

procedure TDataSelectEditor.ESortExpressionChange(Sender: TObject);
var tmp : Integer;
    tmpData : TDataItem;
    tmpExp : TExpressionColumn;
begin
  if not IChanging then
  begin
    tmp:=LBSort.ItemIndex;
    tmpData:=ISelect.SortBy.Items[tmp].Data;

    if ChangeExpression(tmpData,LSortError,ESortExpression.Text,tmpExp) then
    begin
      ISelect.SortBy.Items[tmp].Data:=tmpExp;
      LBSort.Items[tmp]:=tmpExp.Expression.ToString;
      DoChanged;
    end;
  end;
end;

procedure TDataSelectEditor.Expression1Click(Sender: TObject);
begin
  ISelect.SortBy.Add(TExpressionColumn.Create(nil));

  LBSort.Items.Add('?');
  LBSort.ItemIndex:=LBSort.Items.Count-1;
  LBSort.Checked[LBSort.ItemIndex]:=True;

  ESortExpression.SetFocus;
end;

procedure TDataSelectEditor.Refresh(const ASelect: TDataSelect);
begin
  IChanging:=True;
  try
    ISelect:=ASelect;

    CBDistinct.Checked:=ISelect.Distinct;

    EMax.Text:=IntToStr(ISelect.Max);

    FillItems;
    FillSortBy;

    if ISelect.Filter=nil then
       EFilter.Text:=''
    else
       EFilter.Text:=ISelect.Filter.ToString;

    MemoSQL.Lines.Text:=TBISQL.From(ISelect);
  finally
    IChanging:=False;
  end;
end;

procedure TDataSelectEditor.FillItems;
var t : Integer;
begin
  LItems.Clear;

  for t:=0 to High(ISelect.Items) do
  begin
    LItems.AddItem(ISelect.Items[t].Data.Name,ISelect.Items[t].Data);
    LItems.Checked[t]:=ISelect.Items[t].Active;
  end;
end;

procedure TDataSelectEditor.FillSortBy;
var tmp : TSortItem;
begin
  LBSort.Clear;

  for tmp in ISelect.SortBy.Items do
  begin
    if tmp.Data is TExpressionColumn then
       LBSort.Items.Add(TExpressionColumn(tmp.Data).Expression.ToString)
    else
       LBSort.Items.Add(tmp.Data.Name);

    LBSort.Checked[LBSort.Count-1]:=tmp.Active;
  end;
end;

procedure TDataSelectEditor.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabItems;
end;

procedure TDataSelectEditor.LBSortClick(Sender: TObject);

  function IsTextSort(const AIndex:Integer):Boolean;
  begin
    result:=ISelect.SortBy.Items[AIndex].Data.Kind=TDataKind.dkText;
  end;

var tmp : Integer;
    tmpItem : TSortItem;
begin
  IChanging:=True;
  try
    tmp:=LBSort.ItemIndex;

    CBAscending.Enabled:=tmp<>-1;
    CBIgnoreCase.Enabled:=CBAscending.Enabled and IsTextSort(tmp);

    ESortExpression.Enabled:=False;
    ESortExpression.Text:='';

    BRemoveSort.Enabled:=tmp<>-1;
    BUpSort.Enabled:=tmp>0;
    BDownSort.Enabled:=(tmp>=0) and (tmp<LBSort.Count-1);

    if tmp<>-1 then
    begin
      tmpItem:=ISelect.SortBy.Items[tmp];

      CBAscending.Checked:=not tmpItem.Descending;
      CBIgnoreCase.Checked:=tmpItem.IgnoreTextCase;

      ESortExpression.Enabled:=True;

      SetExpressionText(ESortExpression,tmpItem.Data);
    end;
  finally
    IChanging:=False;
  end;
end;

procedure TDataSelectEditor.LBSortClickCheck(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBSort.ItemIndex;

  if tmp<>-1 then
  begin
    ISelect.SortBy.Items[tmp].Active:=LBSort.Checked[tmp];
    DoChanged;
  end;
end;

procedure TDataSelectEditor.LBSortDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var tmp : Integer;
begin
  tmp:=LBSort.ItemAtPos(Point(X,Y),True);

  if tmp<>-1 then
     SwapSort(tmp,LBSort.ItemIndex);
end;

procedure TDataSelectEditor.LBSortDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=Source=Sender;
end;

procedure TDataSelectEditor.LItemsClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LItems.ItemIndex;
  BRemoveItem.Enabled:=tmp<>-1;

  BUp.Enabled:=tmp>0;
  BDown.Enabled:=(tmp>=0) and (tmp<LItems.Count-1);

  EItemExpression.Enabled:=tmp<>-1;
  EItemName.Enabled:=tmp<>-1;

  IChanging:=True;
  try
    if tmp=-1 then
    begin
      EItemExpression.Text:='';
      EItemName.Text:='';
    end
    else
    begin
      SetExpressionText(EItemExpression,ISelect.Items[tmp].Data);
      EItemName.Text:=ISelect.Items[tmp].Name;
    end;
  finally
    IChanging:=False;
  end;
end;

procedure TDataSelectEditor.SetExpressionText(const AEdit:TEdit; const AData:TDataItem);
var tmpExp : TExpression;
begin
  if AData is TExpressionColumn then
  begin
    tmpExp:=TExpressionColumn(AData).Expression;

    if tmpExp=nil then
       AEdit.Text:=''
    else
       AEdit.Text:=tmpExp.ToString
  end
  else
     AEdit.Text:=AData.Name;
end;

procedure TDataSelectEditor.LItemsClickCheck(Sender: TObject);

  procedure TrySwitchSort(const AData:TDataItem; const IsEnabled:Boolean);
  var t : Integer;
  begin
    for t:=0 to LBSort.Count-1 do
        if ISelect.SortBy.Items[t].Data=AData then
        begin
          LBSort.Checked[t]:=IsEnabled;
          ISelect.SortBy.Items[t].Active:=IsEnabled;
        end;
  end;

var tmp : Integer;
begin
  tmp:=LItems.ItemIndex;
  ISelect.Items[tmp].Active:=LItems.Checked[tmp];

  TrySwitchSort(ISelect.Items[tmp].Data,LItems.Checked[tmp]);

  DoChanged;
end;

procedure TDataSelectEditor.LItemsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var tmp : Integer;
begin
  tmp:=LItems.ItemAtPos(Point(X,Y),True);

  if tmp<>-1 then
     SwapItems(tmp,LItems.ItemIndex);
end;

procedure TDataSelectEditor.LItemsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=Source=Sender;
end;

procedure TDataSelectEditor.MenuItem1Click(Sender: TObject);
var tmp : TDataItem;
begin
  tmp:=TDataManager.Choose(Self,nil,True);

  if tmp<>nil then
  begin
    AddItem(tmp);
    DoChanged;
  end;
end;

procedure TDataSelectEditor.MenuItem2Click(Sender: TObject);
begin
  ISelect.Add(TExpressionColumn.Create(nil));

  LItems.Items.Add('?');
  LItems.ItemIndex:=LItems.Items.Count-1;
  LItems.Checked[LItems.ItemIndex]:=True;

  EItemExpression.Enabled:=True;
  EItemExpression.SetFocus;
end;

end.
