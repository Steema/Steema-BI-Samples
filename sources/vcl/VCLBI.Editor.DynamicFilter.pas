{*********************************************}
{  TeeBI Software Library                     }
{  Dynamic Filter Editor                      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.DynamicFilter;

interface

{
  This dialog fills a tree with all possible Data Values that can be accessed
  from a given AData parameter.

  The tree enables checking and unchecking individual items to build a Filter
  expression that can be then used in for example a BIGrid to display rows that
  match the selected items.

  Example 1:
  Using this editor embedded into a Form:

  uses
    BI.Persist, VCLBI.Editor.DynamicFilter;

  var IEditor : TDynamicFilterEditor;

  procedure TFormTest.FilterChanged(Sender: TObject);
  begin
    BIGrid1.SetFilter(IEditor.Filter);
  end;

  procedure TFormTest.FormCreate(Sender: TObject);
  begin
    BIGrid1.Data:=TStore.load('SQLite_Demo')['"Order Details"'];

    IEditor:=TDynamicFilterEditor.Embedd(Self,Panel1,BIGrid1.Data);
    IEditor.OnChange:=FilterChanged;
  end;

  ----------------------

  Example 2:
  Using this editor as a modal dialog:

  procedure TForm23.Button2Click(Sender: TObject);
  begin
    BIGrid1.SetFilter(TDynamicFilterEditor.Choose(Self,BIGrid1.Data));
  end;

}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLBI.DataControl,
  VCLBI.Tree, BI.DataItem, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, BI.Expression,
  VCLBI.Editor.Expression, Vcl.Buttons,
  BI.Expression.Filter, Vcl.CheckLst, BI.CollectionItem,
  VCLBI.Editor.Filter.Item;

type
  TDynamicFilterEditor = class(TForm)
    PanelButtons: TPanel;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    TabData: TTabSheet;
    TabItems: TTabSheet;
    BITree1: TBITree;
    CBItems: TCheckListBox;
    Panel3: TPanel;
    CBEnabled: TCheckBox;
    Panel4: TPanel;
    BAdd: TButton;
    Splitter1: TSplitter;
    BDelete: TButton;
    TabCustom: TTabSheet;
    PanelCustom: TPanel;
    SBCustom: TSpeedButton;
    LError: TLabel;
    Panel2: TPanel;
    CBCustom: TComboBox;
    ECustom: TEdit;
    PanelItem: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ECustomChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBCustomChange(Sender: TObject);
    procedure SBCustomClick(Sender: TObject);
    procedure PanelCustomResize(Sender: TObject);
    procedure BITree1Change(Sender: TObject);
    procedure CBItemsClick(Sender: TObject);
    procedure CBItemsClickCheck(Sender: TObject);
    procedure CBEnabledClick(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
  private
    { Private declarations }

    IChecking : Boolean;

    IFilter : TBIFilter;

    ICustom : TExpression;

    IData,
    IMainData : TDataItem;

    IEditor : TFilterItemEditor;

    FOnChange: TNotifyEvent;

    procedure AddFilter;
    procedure AddFilterItem(const AItem:TFilterItem);
    procedure AddMapValues(const AParent:TBITreeNode; const AData:TDataItem);
    function AddNewFilter(const AData:TDataItem):TFilterItem;
    function CanAddFilter(const AData:TDataItem):Boolean;
    procedure ChangedFilter(Sender: TObject);
    function Current:TFilterItem;
    function CurrentData:TDataItem;
    procedure DoChanged;
    procedure Expanding(Sender: TObject; const Node: TBITreeNode; var AllowExpansion: Boolean);
    function HasDummy(const ANode:TBITreeNode):Boolean;
    function ParseError(const APos:Integer; const AMessage:String):Boolean;
    procedure RefreshProperties(const AItem:TFilterItem);
    function Resolver(const S:String; IsFunction:Boolean):TExpression;
    procedure TreeChecked(Sender: TObject);
    function TryAddFilter(const AData:TDataItem):TFilterItem;
    procedure TryChangeExpression(const AExp:TExpression);
  public
    { Public declarations }

    class function Choose(const AOwner:TComponent;
                          const AData:TDataItem;
                          const AMain:TDataItem=nil):TExpression; static;

    class function Edit(const AOwner:TComponent;
                        const AFilter:TBIFilter;
                        const AData:TDataItem=nil;
                        const AMain:TDataItem=nil):Boolean; static;

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const AFilter:TBIFilter;
                          const AData:TDataItem;
                          const AMain:TDataItem=nil):TDynamicFilterEditor; static;

    procedure Refresh(const AData:TDataItem; const AMain:TDataItem=nil); overload;
    procedure Refresh(const AFilter:TBIFilter; const AData:TDataItem; const AMain:TDataItem=nil); overload;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.dfm}

uses
  System.DateUtils,
  BI.Arrays, BI.Expressions, BI.Expression.DateTime,
  VCLBI.Grid, BI.Summary, BI.Query;

{ TDynamicFilterEditor }

function TDynamicFilterEditor.CurrentData:TDataItem;
var tmp : TBITreeNode;
begin
  tmp:=BITree1.Selected;

  if tmp=nil then
     result:=nil
  else
     result:=BITree1.DataOf(tmp) as TDataItem;
end;

procedure TDynamicFilterEditor.RefreshProperties(const AItem:TFilterItem);
begin
  BDelete.Enabled:=AItem<>nil;

  IEditor.Visible:=AItem<>nil;
  IEditor.Refresh(AItem);
end;

procedure TDynamicFilterEditor.AddFilterItem(const AItem:TFilterItem);
begin
  if AItem.Data=nil then
     CBItems.AddItem('?',AItem)
  else
     CBItems.AddItem(AItem.Data.Name,AItem);

  CBItems.Checked[CBItems.Count-1]:=AItem.Enabled;
end;

type
  TBIFilterAccess=class(TBIFilter);

function TDynamicFilterEditor.AddNewFilter(const AData:TDataItem):TFilterItem;
begin
  TBIFilterAccess(IFilter).IUpdating:=True;
  try
    result:=IFilter.Items.Add(AData);
  finally
    TBIFilterAccess(IFilter).IUpdating:=False;
  end;

  AddFilterItem(result);
end;

procedure TDynamicFilterEditor.BAddClick(Sender: TObject);
begin
  AddNewFilter(CurrentData);

  CBItems.ItemIndex:=CBItems.Count-1;
  CBItemsClick(Self);

  PageControl1.ActivePage:=TabItems;

  BAdd.Enabled:=False;
end;

procedure TDynamicFilterEditor.BDeleteClick(Sender: TObject);
begin
  Current.Free;
  CBItems.Items.Delete(CBItems.ItemIndex);

  CBItemsClick(Self);

  DoChanged;
end;

function TDynamicFilterEditor.CanAddFilter(const AData:TDataItem):Boolean;
begin
  result:=(AData<>nil) and
          (AData.Kind<>TDataKind.dkUnknown);
end;

procedure TDynamicFilterEditor.BITree1Change(Sender: TObject);
begin
  BAdd.Enabled:=CanAddFilter(CurrentData);
end;

procedure TDynamicFilterEditor.CBCustomChange(Sender: TObject);
begin
  DoChanged;
end;

procedure TDynamicFilterEditor.CBEnabledClick(Sender: TObject);
begin
  IFilter.Enabled:=CBEnabled.Checked;
end;

function TDynamicFilterEditor.Current:TFilterItem;
var tmp : Integer;
begin
  tmp:=CBItems.ItemIndex;

  if tmp=-1 then
     result:=nil
  else
     result:=TFilterItem(CBItems.Items.Objects[tmp]);
end;

procedure TDynamicFilterEditor.CBItemsClick(Sender: TObject);
begin
  RefreshProperties(Current);
end;

procedure TDynamicFilterEditor.CBItemsClickCheck(Sender: TObject);
var tmp : Integer;
begin
  tmp:=CBItems.ItemIndex;

  if tmp<>-1 then
     TFilterItem(CBItems.Items.Objects[tmp]).Enabled:=CBItems.Checked[tmp];
end;

class function TDynamicFilterEditor.Choose(const AOwner:TComponent;
                                  const AData: TDataItem;
                                  const AMain:TDataItem): TExpression;
begin
  with TDynamicFilterEditor.Create(AOwner) do
  try
    Refresh(AData,AMain);

    IFilter:=TBIFilter.Create;
    try
      AddFilter;

      if ShowModal=mrOk then
         result:=IFilter.Filter
      else
         result:=nil;
    finally
      IFilter.Free;
    end;

  finally
    Free;
  end;
end;

procedure TDynamicFilterEditor.AddFilter;
var t : Integer;
begin
  CBItems.Items.BeginUpdate;
  try
    CBItems.Clear;

    if IFilter=nil then
       CBEnabled.Enabled:=False
    else
    begin
      CBEnabled.Enabled:=True;
      CBEnabled.Checked:=IFilter.Enabled;

      for t:=0 to IFilter.Items.Count-1 do
          AddFilterItem(IFilter[t]);
    end;

  finally
    CBItems.Items.EndUpdate;
  end;
end;

procedure TDynamicFilterEditor.Refresh(const AFilter: TBIFilter; const AData:TDataItem; const AMain:TDataItem=nil);
begin
  IFilter:=AFilter;
  AddFilter;

  Refresh(AData,AMain);
end;

function TDynamicFilterEditor.Resolver(const S:String; IsFunction:Boolean):TExpression;
begin
  if IsFunction then
     result:=nil
  else
  begin
    result:=TDataExpression.FromString(IData,S
        {$IFNDEF FPC},
                  function(const APos:Integer; const AMessage:String):Boolean
                  begin
                    LError.Caption:=AMessage;
                    result:=True;
                  end
        {$ENDIF}
        );
  end;
end;

procedure TDynamicFilterEditor.TryChangeExpression(const AExp:TExpression);
begin
  ICustom.Free;
  ICustom:=AExp;

  if TDataExpression.KindOf(ICustom)=TDataKind.dkBoolean then
     DoChanged
  else
  if ICustom<>nil then
  begin
    LError.Caption:='Expression is not logical (True/False)';

    ICustom.Free;
    ICustom:=nil;
  end;
end;

procedure TDynamicFilterEditor.SBCustomClick(Sender: TObject);
var tmp : TExpression;
begin
  tmp:=ICustom;

  if TExpressionEditor.Edit(Self,tmp,Resolver) then
  begin
    if tmp<>nil then
       TryChangeExpression(tmp);

    ECustom.Text:=ICustom.ToString;
  end;
end;

function TDynamicFilterEditor.ParseError(const APos:Integer; const AMessage:String):Boolean;
begin
  LError.Caption:=AMessage;
  result:=True;
end;

procedure TDynamicFilterEditor.ECustomChange(Sender: TObject);
var tmp : TExpression;
begin
  LError.Caption:='';

  tmp:=TExpression.FromString(ECustom.Text,Resolver,ParseError);

  TryChangeExpression(tmp);
end;

class function TDynamicFilterEditor.Edit(const AOwner: TComponent;
                                         const AFilter: TBIFilter;
                                         const AData:TDataItem;
                                         const AMain:TDataItem): Boolean;
begin
  with TDynamicFilterEditor.Create(AOwner) do
  try
    Refresh(AFilter,AData,AMain);
    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

class function TDynamicFilterEditor.Embedd(const AOwner: TComponent;
                          const AParent: TWinControl;
                          const AFilter:TBIFilter;
                          const AData:TDataItem;
                          const AMain:TDataItem): TDynamicFilterEditor;
begin
  result:=TDynamicFilterEditor.Create(AOwner);
  result.PanelButtons.Visible:=False;
  result.Refresh(AFilter,AData,AMain);
  TUICommon.AddForm(result,AParent);
end;

const
  DummyNode='**********************';

function TDynamicFilterEditor.HasDummy(const ANode:TBITreeNode):Boolean;
begin
  result:=(BITree1.Plugin.ChildrenCount(ANode)=1) and
          (BITree1.Plugin.TextOf(BITree1.Plugin.Children(ANode,0))=DummyNode);
end;

procedure TDynamicFilterEditor.PanelCustomResize(Sender: TObject);
begin
  ECustom.Width:=PanelCustom.Width-136;
  SBCustom.Left:=ECustom.Left+ECustom.Width+4;
end;

procedure TDynamicFilterEditor.Expanding(Sender: TObject; const Node: TBITreeNode;
    var AllowExpansion: Boolean);
begin
  AllowExpansion:=True;

  if HasDummy(Node) then
  begin
    BITree1.Plugin.Children(Node,0).Free;
    AddMapValues(Node,BITree1.DataOf(Node) as TDataItem);
  end;
end;

procedure TDynamicFilterEditor.DoChanged;
begin
  if Assigned(FOnChange) then
     FOnChange(Self);
end;

function TDynamicFilterEditor.TryAddFilter(const AData:TDataItem):TFilterItem;
begin
  if CanAddFilter(AData) then
     result:=AddNewFilter(AData)
  else
     result:=IFilter.ItemOf(AData);
end;

procedure TDynamicFilterEditor.TreeChecked(Sender: TObject);

  procedure TryChildren(const ANode:TBITreeNode; const IsChecked:Boolean);
  var t,
      tmp : Integer;
      tmpNode : TBITreeNode;
  begin
    tmp:=BITree1.Plugin.ChildrenCount(ANode);

    for t:=0 to tmp-1 do
    begin
      tmpNode:=BITree1.Plugin.Children(ANode,t);
      BITree1.Plugin.SetChecked(tmpNode,IsChecked);
      TryChildren(tmpNode,IsChecked);
    end;
  end;

var tmp : TBITreeNode;
    tmpData : TObject;
    tmpItem : TFilterItem;
    tmpChecked : Boolean;
    tmpIndex : Integer;
    tmpS : String;
begin
  if not IChecking then
  begin
    IChecking:=True;
    try
      tmp:=BITree1.Selected;
      tmpChecked:=BITree1.Plugin.IsChecked(tmp);
      tmpIndex:=BITree1.Plugin.SiblingIndex(tmp);

      if BITree1.Plugin.ChildrenCount(tmp)>0 then
      begin
        TryChildren(tmp,tmpChecked);

        tmpData:=BITree1.DataOf(tmp);

        if tmpData is TDataItem then
        begin
          tmpItem:=TryAddFilter(TDataItem(tmpData));
          tmpItem.Included.Clear;
        end;
      end
      else
      begin
        tmp:=BITree1.Plugin.ParentOf(tmp);
        tmpData:=BITree1.DataOf(tmp);

        if tmpData is TDataItem then
        begin
          tmpItem:=TryAddFilter(TDataItem(tmpData));

          tmpS:=TDataItem(tmpData).DataMap.AsString(tmpIndex);
          tmpItem.IncludeText(tmpS,tmpChecked);
        end;
      end;
    finally
      IChecking:=False;
    end;

    DoChanged;
  end;
end;

procedure TDynamicFilterEditor.ChangedFilter(Sender: TObject);
begin
  DoChanged;
end;

procedure TDynamicFilterEditor.FormCreate(Sender: TObject);
begin
  BITree1.OnExpanding:=Expanding;

  BITree1.Plugin.OnCheck:=TreeChecked;

  IEditor:=TFilterItemEditor.Embedd(Self,PanelItem,nil);
  IEditor.OnChange:=ChangedFilter;
end;

procedure TDynamicFilterEditor.FormDestroy(Sender: TObject);
begin
  ICustom.Free;
end;

type
  TDataAccess=class(TDataItem);

procedure TDynamicFilterEditor.AddMapValues(const AParent:TBITreeNode; const AData:TDataItem);

  function AddItem(const AParent:TBITreeNode; const AText:String):TBITreeNode;
  begin
    result:=BITree1.Add(AParent,AText);
    BITree1.Plugin.SetChecked(result,False);
  end;

  procedure AddIndividual;
  var t : Integer;
  begin
    AData.Load;
    AData.Stats;

    for t:=0 to AData.DataMap.Count-1 do
        AddItem(AParent,AData.DataMap.AsString(t));
  end;

  procedure AddMonth(const AParent:TBITreeNode; const AMonth:Integer);
  begin
    AddItem(AParent,FormatSettings.LongMonthNames[AMonth]);
  end;

  procedure AddQuarter(const AParent:TBITreeNode; const AQuarter:Integer);
  var tmp : TBITreeNode;
      t,
      tmpStart : Integer;
  begin
    tmp:=AddItem(AParent,TDateTimePart.Quarter.AsString(AQuarter-1));

    tmpStart:=1+((AQuarter-1)*3);

    for t:=tmpStart to tmpStart+2 do
        AddMonth(tmp,t);
  end;

  procedure AddDateTime;
  var tmp : TDateTimeStats;
      tmpMin : Integer;
      tmpMax : Integer;
      t,tt : Integer;
      tmpNode : TBITreeNode;
  begin
    tmp:=TDateTimeStats(AData.Stats);

    tmpMin:=TBIDateTime.YearOf(tmp.Min);
    tmpMax:=TBIDateTime.YearOf(tmp.Max);

    if tmpMin<>tmpMax then
    begin
      for t:=tmpMin to tmpMax do
      begin
        tmpNode:=AddItem(AParent,IntToStr(t));

        for tt:=1 to 4 do
            AddQuarter(tmpNode,tt);
      end;
    end
    else
    begin
      tmpMin:=MonthOfTheYear(tmp.Min);
      tmpMax:=MonthOfTheYear(tmp.Max);

      if tmpMin<>tmpMax then
      begin
        for t:=tmpMin to tmpMax do
            AddMonth(AParent,t);
      end
      else
        AddIndividual;
    end;
  end;

begin
  if AData.Kind=TDataKind.dkDateTime then
     AddDateTime
  else
     AddIndividual;
end;

procedure TDynamicFilterEditor.Refresh(const AData:TDataItem;
                                       const AMain:TDataItem);

  {
  function CanGetMaster(const AData:TDataItem):Boolean;
  begin
    result:=False;

    if TDataAccess(AData).HasMaster then
    try
      AData.Master;
      result:=True;
    except
      on Exception do;
    end;
  end;

  function GetDescription(const AData:TDataItem):TDataItem;
  var tmp : TDataItem;
  begin
    if TDataAccess(AData).HasMaster then
       if CanGetMaster(AData) then
          for tmp in AData.Master.Parent.Items.AsArray do
              if tmp.Kind=TDataKind.dkText then
                 Exit(tmp);

    result:=AData;
  end;
  }

  function FindData(const AData:TDataItem):TBITreeNode;
  begin
    result:=BITree1.Plugin.Find(AData);
  end;

  function AddNode(const AParent:TBITreeNode; const AData:TDataItem):TBITreeNode;
  var tmpNode : TBITreeNode;
      tmp : TDataItem;
  begin
    result:=FindData(AData);

    if result=nil then
    begin
      tmp:=AData;

      tmpNode:=FindData(AData.Parent);

      if tmpNode=nil then
         tmpNode:=AParent;

      result:=BITree1.Add(tmpNode,tmp.Name,tmp);

      if tmp.Kind=TDataKind.dkUnknown then
         for tmp in AData.Items.AsArray do
             AddNode(result,tmp)
      else
      begin
        if TSummaryItem.GuessType(tmp)<>TSummaryItemType.Measure then
            BITree1.Add(result,DummyNode);

        if TDataAccess(AData).HasMaster then
           if FindData(AData.Master.Parent)=nil then
              AddNode(nil,AData.Master.Parent);
      end;
    end;
  end;

  procedure AddProvider(const AProvider:TDataProvider);

    procedure TryAdd(const AData:TDataItem);
    begin
      if AData<>nil then
         if AData.Kind=TDataKind.dkUnknown then
            AddNode(nil,AData)
         else
         if AData.Parent<>nil then
            AddNode(nil,AData.Parent);
    end;

  var tmp : TBIQuery;
      t : Integer;
  begin
    if AProvider is TBIQuery then
    begin
      tmp:=TBIQuery(AProvider);

      for t:=0 to tmp.Dimensions.Count-1 do
          TryAdd(tmp.Dimensions[t].Data);

      for t:=0 to tmp.Measures.Count-1 do
          TryAdd(tmp.Measures[t].Data);
    end;
  end;

var tmp : TDataItem;
begin
  IData:=AData;
  IMainData:=AMain;

  BITree1.Clear;

  tmp:=IData;

  if tmp<>nil then
  begin
    tmp.Load;

    BITree1.BeginUpdating;
    try
      if tmp.Provider=nil then
         AddNode(nil,tmp)
      else
         AddProvider(tmp.Provider);

    finally
      BITree1.EndUpdating;
    end;
  end;

  if CBItems.Count=0 then
     PageControl1.ActivePage:=TabData
  else
     PageControl1.ActivePage:=TabItems;
end;

end.
