{*********************************************}
{  TeeBI Software Library                     }
{  Query Editor Dialog                        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Query;

interface

{
  This form is used to edit a TBIQuery (pivot-table) component.

  Data can be drag-dropped from the left tree to the Rows, Columns or Measures
  listboxes.

  The listboxes can also be drag-dropped to reorder elements or to move items
  from one list to another.

}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.CheckLst, Vcl.Buttons,

  BI.DataItem, BI.Summary, BI.DataSource, VCLBI.DataSelect, BI.Query,
  VCLBI.Grid, VCLBI.DataControl, BI.Persist, VCLBI.Editor.DynamicFilter,
  BI.Expression, VCLBI.Editor.Sort, VCLBI.DataManager;

type
  TOnShowQueryEditor=procedure(const AParent:TWinControl;
                               const AProvider:TComponent;
                               const AData:TDataItem);

  TBIQueryEditor = class(TForm)
    PanelSelector: TPanel;
    PanelEdit: TPanel;
    OuterPanel: TPanel;
    SplitterPreview: TSplitter;
    SplitterSelector: TSplitter;
    PanelRows: TPanel;
    PanelColumns: TPanel;
    PanelMeasures: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ListRows: TCheckListBox;
    ListMeasures: TCheckListBox;
    ListColumns: TCheckListBox;
    Panel4: TPanel;
    CBDistinct: TCheckBox;
    Panel5: TPanel;
    Panel7: TPanel;
    BDeleteColumn: TButton;
    PageMeasures: TPageControl;
    TabMeasure: TTabSheet;
    CBAggregate: TComboBox;
    CBMissingAsZero: TCheckBox;
    TabCalc: TTabSheet;
    RGRunning: TRadioGroup;
    CBRunningRows: TCheckBox;
    RGPercentage: TRadioGroup;
    Label4: TLabel;
    LabelItemKind: TLabel;
    Label5: TLabel;
    LExpressionError: TLabel;
    EItemExpression: TEdit;
    CBRemoveRows: TCheckBox;
    CBRemoveCols: TCheckBox;
    PanelOptions: TPanel;
    PanelButtons: TPanel;
    BOK: TButton;
    Button1: TButton;
    LMax: TLabel;
    EMax: TEdit;
    Label1: TLabel;
    BDeleteMeasure: TButton;
    Label2: TLabel;
    BDeleteRow: TButton;
    PageOptions: TPageControl;
    TabItem: TTabSheet;
    TabMeasureOptions: TTabSheet;
    Label3: TLabel;
    CBDatePart: TComboBox;
    GBHistogram: TGroupBox;
    CBHistoActive: TCheckBox;
    SBMeasureUp: TSpeedButton;
    SBMeasureDown: TSpeedButton;
    Label6: TLabel;
    EBins: TEdit;
    UDBins: TUpDown;
    SBSwap: TSpeedButton;
    SBSelector: TSpeedButton;
    Label7: TLabel;
    ETitle: TEdit;
    SpeedButton1: TSpeedButton;
    TabItemData: TTabSheet;
    PagePreview: TPageControl;
    TabGrid: TTabSheet;
    BIGrid1: TBIGrid;
    TabChart: TTabSheet;
    PageData: TPageControl;
    TabData: TTabSheet;
    TabFilter: TTabSheet;
    LStart: TLabel;
    EStart: TEdit;
    TabSort: TTabSheet;
    BIQuery1: TBIQuery;
    TabSQL: TTabSheet;
    MemoSQL: TMemo;
    CBPreview: TCheckBox;
    Panel6: TPanel;
    SBRowUp: TSpeedButton;
    SBRowDown: TSpeedButton;
    Panel8: TPanel;
    SBColUp: TSpeedButton;
    SBColDown: TSpeedButton;
    Label8: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ListRowsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListRowsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListRowsClick(Sender: TObject);
    procedure BDeleteRowClick(Sender: TObject);
    procedure CBDistinctClick(Sender: TObject);
    procedure BDeleteMeasureClick(Sender: TObject);
    procedure ListMeasuresClick(Sender: TObject);
    procedure ListColumnsClick(Sender: TObject);
    procedure BDeleteColumnClick(Sender: TObject);
    procedure CBAggregateChange(Sender: TObject);
    procedure CBMissingAsZeroClick(Sender: TObject);
    procedure RGPercentageClick(Sender: TObject);
    procedure RGRunningClick(Sender: TObject);
    procedure CBRunningRowsClick(Sender: TObject);
    procedure EItemExpressionChange(Sender: TObject);
    procedure CBRemoveRowsClick(Sender: TObject);
    procedure CBRemoveColsClick(Sender: TObject);
    procedure CBDatePartChange(Sender: TObject);
    procedure ListMeasuresDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListRowsClickCheck(Sender: TObject);
    procedure EMaxChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBHistoActiveClick(Sender: TObject);
    procedure SBRowUpClick(Sender: TObject);
    procedure SBRowDownClick(Sender: TObject);
    procedure SBMeasureDownClick(Sender: TObject);
    procedure SBMeasureUpClick(Sender: TObject);
    procedure SBColUpClick(Sender: TObject);
    procedure SBColDownClick(Sender: TObject);
    procedure EBinsChange(Sender: TObject);
    procedure SBSwapClick(Sender: TObject);
    procedure SBSelectorClick(Sender: TObject);
    procedure ETitleChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure PageDataChange(Sender: TObject);
    procedure EStartChange(Sender: TObject);
    procedure PagePreviewChange(Sender: TObject);
    procedure CBPreviewClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

    type
      TAddMasterProc=procedure(const AData:TDataItem) of object;

    var
    IQuery : TBIQuery;

    ISelector : TDataSelector;

    CompTree,
    DataTree : TTreeView;

    IFilter : TDynamicFilterEditor;
    ISort : TSortEditor;

    IChanging,
    ISettingPreview,
    IModified : Boolean;

    IMasterFilter : TMasterFilter;

    IDimension : TQueryDimension;
    IMeasure : TQueryMeasure;

    function AddData(const AList:TCheckListBox; const AData:TDataItem; const IsActive:Boolean=True):TQueryItem;
    procedure AddItem(const AList:TCheckListBox; const AItem:TQueryItem);
    procedure AdjustSelectorCaption;
    procedure ChangeCurrentBy;
    procedure ChangeCurrentMeasure;
    procedure ChangeDimension(const AList:TCheckListBox); overload;
    procedure ChangeDimension(const AItem:TQueryDimension); overload;
    procedure ChangeMeasure; overload;
    procedure ChangeMeasure(const AItem:TQueryMeasure); overload;
    procedure ChangedFilter(Sender: TObject);
    function ChangingQuery:Boolean;
    procedure DeleteDimension(const AList:TCheckListBox);
    procedure DoExchangeItem(const AList:TCheckListBox; const A,B:Integer); overload;
    procedure DoExchangeItem(const AList:TCheckListBox; const Delta:Integer); overload;

    function EditResolver(const S:String; IsFunction:Boolean):TExpression;

    procedure EnableHistogramControls;
    procedure EnableColumnButtons;
    procedure EnableMeasureButtons;
    procedure EnableRowButtons;
    procedure EnableRowSettings;
    procedure EnableSwap;
    procedure FilterComponent(Sender: TComponent; var Valid:Boolean);

    function GetPreview: Boolean;
    function ListOf(const ABy:TQueryDimension):TCheckListBox;
    function Measure:TQueryMeasure;
    procedure Modified;
    procedure RefreshFilter;
    procedure RefreshFilterAndSort;
    procedure RefreshQuery;
    procedure RefreshSelector;
    procedure RemoveFromList(const AList:TCheckListBox); overload;
    procedure RemoveFromList(const AList:TCheckListBox; const AIndex:Integer); overload;
    function ResolveData(const APos:Integer; const AMessage:String):Boolean;
    function Resolver(const S:String; IsFunction:Boolean):TExpression;

    procedure SetDataProperties(const AItem:TQueryDimension); overload;
    procedure SetDataProperties(const AItem:TQueryMeasure); overload;
    procedure SetDataProperties(const AData:TDataItem; const AExpression:TExpression); overload;

    procedure SetItemProperties(const ACurrent:TQueryDimension); overload;
    procedure SetItemProperties(const ACurrent:TQueryMeasure); overload;

    procedure SetPart(const ACombo:TComboBox; const APart:TDateTimePart);
    procedure SetPreview(const Value: Boolean);

    procedure ShowSQL;
    procedure SortChanged(Sender:TObject);
    procedure TryAddMasters(const AProc:TAddMasterProc);
    procedure TryShowPreviewGrid;
    procedure TryShowSQL;
  public
    { Public declarations }

    class
      // Event used by VCLBI.Editor.Chart to display a BIChart at Preview tab
      var OnShowEditor : TOnShowQueryEditor;

    function Selector:TDataSelector;

    class function Edit(const AOwner:TComponent; const AQuery:TBIQuery):Boolean; static;
    class function Embedd(const AOwner: TComponent; const AParent: TWinControl; const AQuery:TBIQuery): TBIQueryEditor; static;

    procedure Refresh(const AQuery:TBIQuery);
    procedure ShowSelector(const AShow:Boolean);

    // Show or hide the "Preview" tab
    property Preview:Boolean read GetPreview write SetPreview;
  end;

implementation

{$R *.dfm}

uses
  System.Types, VCLBI.Component,
  BI.Arrays, BI.Expressions, VCLBI.Editor.Expression, BI.SQL;

const
  RegistryQueryEditor='QueryEditor';

{ TBIQueryEditor }

procedure TBIQueryEditor.BDeleteColumnClick(Sender: TObject);
begin
  DeleteDimension(ListColumns);
end;

function DimensionOf(const AList:TCheckListBox; const AIndex:Integer):TQueryDimension; overload;
begin
  result:=TQueryDimension(AList.Items.Objects[AIndex]);
end;

function DimensionOf(const AList:TCheckListBox):TQueryDimension; overload; inline;
begin
  result:=DimensionOf(AList,AList.ItemIndex);
end;

function MeasureOf(const AList:TCheckListBox; const AIndex:Integer):TQueryMeasure; overload;
begin
  result:=TQueryMeasure(AList.Items.Objects[AIndex]);
end;

function MeasureOf(const AList:TCheckListBox):TQueryMeasure; overload; inline;
begin
  result:=MeasureOf(AList,AList.ItemIndex);
end;

function TBIQueryEditor.ListOf(const ABy:TQueryDimension):TCheckListBox;
var t : Integer;
begin
  for t:=0 to ListRows.Count-1 do
      if DimensionOf(ListRows,t)=ABy then
         Exit(ListRows);

  for t:=0 to ListColumns.Count-1 do
      if DimensionOf(ListColumns,t)=ABy then
         Exit(ListColumns);

  result:=nil;
end;

procedure TBIQueryEditor.BDeleteMeasureClick(Sender: TObject);
begin
  MeasureOf(ListMeasures).Free;

  RemoveFromList(ListMeasures);

  RefreshSelector;

  EnableRowSettings;
  Modified;
end;

procedure TBIQueryEditor.RemoveFromList(const AList:TCheckListBox; const AIndex:Integer);
var tmpEnable : Boolean;
begin
  AList.Items.Delete(AIndex);

  if (AIndex=0) and (AList.Count>0) then
     AList.ItemIndex:=0
  else
  if AIndex=AList.Count then
     AList.ItemIndex:=AList.Count-1;

  tmpEnable:=AList.ItemIndex<>-1;

  if AList=ListRows then
     BDeleteRow.Enabled:=tmpEnable
  else
  if AList=ListColumns then
     BDeleteColumn.Enabled:=tmpEnable
  else
     BDeleteMeasure.Enabled:=tmpEnable;

  AList.OnClick(AList);
end;

procedure TBIQueryEditor.RemoveFromList(const AList:TCheckListBox);
begin
  RemoveFromList(AList,AList.ItemIndex);
end;

procedure TBIQueryEditor.DeleteDimension(const AList:TCheckListBox);
begin
  DimensionOf(AList).Free;

  RemoveFromList(AList);

  RefreshSelector;
  EnableRowSettings;
  Modified;
end;

procedure TBIQueryEditor.DoExchangeItem(const AList: TCheckListBox;
  const Delta: Integer);
begin
  DoExchangeItem(AList,AList.ItemIndex,AList.ItemIndex+Delta);
  Modified;
end;

procedure TBIQueryEditor.BDeleteRowClick(Sender: TObject);
begin
  DeleteDimension(ListRows);
end;

procedure TBIQueryEditor.BOKClick(Sender: TObject);
begin
  if IModified then
  begin
    IQuery.Assign(BIQuery1);
    IModified:=False;
  end;
end;

function TBIQueryEditor.Measure:TQueryMeasure;
begin
  result:=MeasureOf(ListMeasures);
end;

procedure TBIQueryEditor.ChangeMeasure;
begin
  ListMeasures.Items[ListMeasures.ItemIndex]:=MeasureOf(ListMeasures).ToString;
end;

procedure TBIQueryEditor.ChangeDimension(const AList:TCheckListBox);
begin
  AList.Items[AList.ItemIndex]:=DimensionOf(AList).ToString;
end;

procedure TBIQueryEditor.CBAggregateChange(Sender: TObject);
begin
  IMeasure.Aggregate:=TAggregate(CBAggregate.ItemIndex);

  CBMissingAsZero.Enabled:=IMeasure.Aggregate.SupportsAsZero;

  ChangeMeasure;
  Modified;
end;

procedure TBIQueryEditor.CBDistinctClick(Sender: TObject);
begin
  if not ChangingQuery then
  begin
    BIQuery1.Distinct:=CBDistinct.Checked;
    Modified;
  end;
end;

procedure TBIQueryEditor.ChangeMeasure(const AItem:TQueryMeasure);
begin
  ChangeMeasure;
end;

procedure TBIQueryEditor.ChangeDimension(const AItem: TQueryDimension);
begin
  ChangeDimension(ListOf(AItem));
end;

type
  TDataProviderAccess=class(TDataProvider);

function TBIQueryEditor.ChangingQuery:Boolean;
begin
  result:=TDataProviderAccess(BIQuery1).Changing;
end;

procedure TBIQueryEditor.EnableHistogramControls;
begin
  UDBins.Enabled:=CBHistoActive.Checked;
  EBins.Enabled:=UDBins.Enabled;
end;

procedure TBIQueryEditor.CBHistoActiveClick(Sender: TObject);
begin
  IDimension.Histogram.Active:=CBHistoActive.Checked;
  EnableHistogramControls;
  ChangeDimension(IDimension);
  RefreshQuery;
//  Modified;
end;

procedure TBIQueryEditor.CBMissingAsZeroClick(Sender: TObject);
begin
  if not ChangingQuery then
  begin
    Measure.Missing.AsZero:=CBMissingAsZero.Checked;
    RefreshQuery;
  //  Modified;
  end;
end;

procedure TBIQueryEditor.CBPreviewClick(Sender: TObject);
begin
  Preview:=CBPreview.Checked;
end;

procedure TBIQueryEditor.CBRemoveColsClick(Sender: TObject);
begin
  if not ChangingQuery then
  begin
    BIQuery1.RemoveMissing.Columns:=CBRemoveCols.Checked;
    Modified;
  end;
end;

procedure TBIQueryEditor.CBRemoveRowsClick(Sender: TObject);
begin
  if not ChangingQuery then
  begin
    BIQuery1.RemoveMissing.Rows:=CBRemoveRows.Checked;
    Modified;
  end;
end;

procedure TBIQueryEditor.CBDatePartChange(Sender: TObject);
begin
  IDimension.DatePart:=TDateTimePart(CBDatePart.ItemIndex);
  ChangeDimension(IDimension);
  Modified;
end;

procedure TBIQueryEditor.CBRunningRowsClick(Sender: TObject);
begin
  Measure.Calculation.RunningByRows:=CBRunningRows.Checked;
  RefreshQuery;
end;

procedure TBIQueryEditor.EBinsChange(Sender: TObject);
var tmp : THistogram;
begin
  if (not ChangingQuery) and (IDimension<>nil) then
  begin
    tmp:=IDimension.Histogram;

    if tmp.NumBins<>UDBins.Position then
    begin
      tmp.NumBins:=UDBins.Position;
      RefreshQuery;
      //Modified;
    end;
  end;
end;

class function TBIQueryEditor.Edit(const AOwner: TComponent;
  const AQuery: TBIQuery): Boolean;
begin
  if AQuery=nil then
     raise EBIException.Create('Error: Query not assigned');

  with TBIQueryEditor.Create(AOwner) do
  try
    IQuery:=AQuery;
    Refresh(IQuery);

    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

procedure TBIQueryEditor.EMaxChange(Sender: TObject);
begin
  if not ChangingQuery then
  begin
    if Trim(EMax.Text)='' then
       BIQuery1.MaxRows:=0
    else
       BIQuery1.MaxRows:=StrToInt64Def(EMax.Text,BIQuery1.MaxRows);

    Modified;
  end;
end;

procedure TBIQueryEditor.EItemExpressionChange(Sender: TObject);
var tmp : TExpression;
    tmpEmpty : Boolean;
begin
  if not IChanging then
  begin
    tmpEmpty:=Trim(EItemExpression.Text)='';

    if tmpEmpty then
       tmp:=nil
    else
       tmp:=TDataItemExpression.FromString(EItemExpression.Text,Resolver,ResolveData);

    if tmpEmpty or (tmp<>nil) then
    begin
      if IDimension<>nil then
      begin
        IDimension.Expression:=tmp;
        ChangeDimension(IDimension);
      end
      else
      if IMeasure<>nil then
      begin
        IMeasure.Expression:=tmp;
        ChangeMeasure(IMeasure);
      end;

      LExpressionError.Caption:='';

      LabelItemKind.Caption:=TDataExpression.KindOf(tmp).ToString;

      Modified;
    end;
  end;
end;

procedure TBIQueryEditor.FilterComponent(Sender: TComponent; var Valid:Boolean);

  function IsSelector:Boolean;
  begin
    result:=(ISelector<>nil) and
            (
              (Sender=ISelector)
              or
              (Sender=ISelector.Manager)
            );
  end;

begin
  Valid:=(Sender<>Self) and (Sender<>IQuery) and
         (not IsSelector) and
         (Sender<>IFilter);
end;

procedure TBIQueryEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TUICommon.SavePosition(Self,RegistryQueryEditor);
end;

procedure TBIQueryEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if IModified then
     CanClose:=TUICommon.YesNo('Query has been modified. Discard changes?')
  else
     CanClose:=True;
end;

procedure TBIQueryEditor.FormCreate(Sender: TObject);
begin
  BIQuery1:=TBIQuery.Create(Self);

  IMasterFilter:=TMasterFilter.Create;

  PageOptions.ActivePage:=TabItem;
  PageMeasures.ActivePage:=TabMeasure;
  PageData.ActivePage:=TabData;
  PagePreview.ActivePage:=TabGrid;

  ListRows.DragMode:=TDragMode.dmAutomatic;
  ListColumns.DragMode:=TDragMode.dmAutomatic;
  ListMeasures.DragMode:=TDragMode.dmAutomatic;

  // Share drag events
  ListColumns.OnDragDrop:=ListRowsDragDrop;
  ListColumns.OnDragOver:=ListRowsDragOver;
  ListMeasures.OnDragDrop:=ListRowsDragDrop;
end;

procedure TBIQueryEditor.ChangedFilter(Sender: TObject);
begin
  Modified;
end;

procedure TBIQueryEditor.FormDestroy(Sender: TObject);
begin
  BIGrid1.Data.Free;
  IMasterFilter.Free;
end;

procedure TBIQueryEditor.FormShow(Sender: TObject);
begin
  TUICommon.LoadPosition(Self,RegistryQueryEditor);

  if PanelSelector.Visible then
     ShowSelector(True);

  if Preview then
     TryShowPreviewGrid;
end;

function TBIQueryEditor.GetPreview: Boolean;
begin
  result:=PagePreview.Visible;
end;

procedure TBIQueryEditor.RefreshFilter;
begin
  if IFilter<>nil then
     IFilter.Refresh(BIQuery1.Filter,BIQuery1.Data,BIQuery1.Main);
end;

procedure TBIQueryEditor.RefreshFilterAndSort;
begin
  RefreshFilter;

  if ISort<>nil then
     ISort.Refresh(BIQuery1.SortBy);
end;

type
  TDataSelectorAccess=class(TDataSelector);

procedure TBIQueryEditor.Refresh(const AQuery:TBIQuery);

  procedure AddDimensions(const AItems:TQueryDimensions);
  var t : Integer;
      tmp : TCheckListBox;
  begin
    for t:=0 to AItems.Count-1 do
    begin
      case AItems[t].RealStyle of
        TDimensionStyle.Column: tmp:=ListColumns;
      else
        tmp:=ListRows;
      end;

      AddItem(tmp,AItems[t]);
    end;
  end;

  procedure AddMeasures(const AItems:TQueryMeasures);
  var t : Integer;
  begin
    for t:=0 to AItems.Count-1 do
        AddItem(ListMeasures,AItems[t]);
  end;

  procedure SelectFirst(const AList:TCheckListBox);
  begin
    if AList.Count>0 then
    begin
      AList.ItemIndex:=0;
      AList.OnClick(AList);
    end;
  end;

  procedure InitControls;
  begin
    if ISelector<>nil then
       TDataSelectorAccess(ISelector).SetEdited(BIQuery1);

    ListRows.Clear;
    ListColumns.Clear;
    ListMeasures.Clear;

    AddDimensions(BIQuery1.Dimensions);
    AddMeasures(BIQuery1.Measures);

    SelectFirst(ListRows);
    SelectFirst(ListColumns);
    SelectFirst(ListMeasures);

    EnableRowSettings;

    ETitle.Text:=BIQuery1.Title;

    CBRemoveRows.Checked:=BIQuery1.RemoveMissing.Rows;
    CBDistinct.Checked:=BIQuery1.Distinct;
    EMax.Text:=IntToStr(BIQuery1.MaxRows);
    EStart.Text:=IntToStr(BIQuery1.StartRow);

    RefreshFilterAndSort;
  end;

begin
  Enabled:=AQuery<>nil;

  if Enabled then
  begin
    IQuery:=AQuery;

    if IQuery.Name='' then
       Caption:='Query Editor'
    else
       Caption:='Editing '+IQuery.Name;

    try
      IChanging:=True;
      try
        TDataProviderAccess(BIQuery1).BeginUpdate;
        BIQuery1.Assign(IQuery);
        InitControls;
        TDataProviderAccess(BIQuery1).EndUpdate;
      finally
        IChanging:=False;
      end;
    except
      on E:Exception do
         ShowMessage(E.Message);
    end;
  end;
end;

procedure TBIQueryEditor.RefreshQuery;
begin
  BIQuery1.Refresh;
  Modified;
end;

procedure TBIQueryEditor.TryAddMasters(const AProc:TAddMasterProc);
var t : Integer;
begin
  for t:=0 to BIQuery1.Dimensions.Count-1 do
      AProc(BIQuery1.Dimensions[t].RealData);

  for t:=0 to BIQuery1.Measures.Count-1 do
      AProc(BIQuery1.Measures[t].RealData);
end;

procedure TBIQueryEditor.RefreshSelector;
begin
  if ISelector<>nil then
  begin
    IMasterFilter.Clear;

    TryAddMasters(IMasterFilter.Add);

    ISelector.RefreshTree(IMasterFilter);
  end;
end;

procedure TBIQueryEditor.RGPercentageClick(Sender: TObject);
begin
  Measure.Calculation.Percentage:=TCalculationPercentage(RGPercentage.ItemIndex);
  RefreshQuery;
end;

procedure TBIQueryEditor.RGRunningClick(Sender: TObject);
begin
  Measure.Calculation.Running:=TCalculationRunning(RGRunning.ItemIndex);
  RefreshQuery;
end;

type
  TGroupByAccess=class(TGroupBy);

procedure TBIQueryEditor.AddItem(const AList:TCheckListBox; const AItem:TQueryItem);
var tmp : Integer;
begin
  tmp:=AList.Items.AddObject(AItem.ToString,AItem);
  AList.Checked[tmp]:=AItem.Enabled;
end;

function TBIQueryEditor.AddData(const AList:TCheckListBox; const AData:TDataItem; const IsActive:Boolean=True):TQueryItem;

  function AddQueryItem:TQueryItem;
  var tmp : TAggregate;
      tmpStyle : TDimensionStyle;
  begin
    if AList=ListMeasures then
    begin
      if AData.Kind.IsNumeric then
         tmp:=TAggregate.Sum
      else
         tmp:=TAggregate.Count;

      result:=BIQuery1.Measures.Add(AData,tmp,IsActive);
    end
    else
    begin
      if AList=ListRows then
         tmpStyle:=TDimensionStyle.Row
      else
         tmpStyle:=TDimensionStyle.Column;

      result:=BIQuery1.Dimensions.Add(AData,tmpStyle,IsActive);
    end;
  end;

begin
  TDataProviderAccess(BIQuery1).BeginUpdate;
  try
    result:=AddQueryItem;
    AddItem(AList,result);
  finally
    TDataProviderAccess(BIQuery1).EndUpdate;
  end;

  RefreshSelector;
  RefreshFilterAndSort;

  EnableRowSettings;
end;

procedure TBIQueryEditor.EnableSwap;
begin
  SBSwap.Enabled:=(ListRows.Count>0) or (ListColumns.Count>0);
end;

procedure TBIQueryEditor.EStartChange(Sender: TObject);
begin
  if not ChangingQuery then
  begin
    if Trim(EStart.Text)='' then
       BIQuery1.StartRow:=0
    else
       BIQuery1.StartRow:=StrToInt64Def(EStart.Text,BIQuery1.StartRow);

    Modified;
  end;
end;

procedure TBIQueryEditor.ETitleChange(Sender: TObject);
begin
  if not ChangingQuery then
  begin
    BIQuery1.Title:=ETitle.Text;
    Modified;
  end;
end;

procedure TBIQueryEditor.EnableRowSettings;
begin
  // Force calculating Style
  if BIQuery1.Style=TQueryStyle.Unknown then
     BIQuery1.ToString; // <-- pending: a better solution

  CBDistinct.Enabled:=BIQuery1.Style=TQueryStyle.Select;

  EMax.Enabled:=CBDistinct.Enabled;
  LMax.Enabled:=CBDistinct.Enabled;

  EStart.Enabled:=CBDistinct.Enabled;
  LStart.Enabled:=CBDistinct.Enabled;

  CBRemoveRows.Enabled:=BIQuery1.Style=TQueryStyle.Summary;
  CBRemoveCols.Enabled:=CBRemoveRows.Enabled;

  EnableSwap;
end;

procedure TBIQueryEditor.SBColDownClick(Sender: TObject);
begin
  DoExchangeItem(ListColumns,1);
end;

procedure TBIQueryEditor.SBColUpClick(Sender: TObject);
begin
  DoExchangeItem(ListColumns,-1);
end;

procedure TBIQueryEditor.SBMeasureDownClick(Sender: TObject);
begin
  DoExchangeItem(ListMeasures,1);
end;

procedure TBIQueryEditor.SBMeasureUpClick(Sender: TObject);
begin
  DoExchangeItem(ListMeasures,-1);
end;

procedure TBIQueryEditor.SBRowDownClick(Sender: TObject);
begin
  DoExchangeItem(ListRows,1);
end;

procedure TBIQueryEditor.SBRowUpClick(Sender: TObject);
begin
  DoExchangeItem(ListRows,-1);
end;

procedure TBIQueryEditor.ChangeCurrentBy;
var tmp : TQueryDimension;
begin
  tmp:=IDimension;

  TabItem.TabVisible:=True;
  PageOptions.ActivePage:=TabItem;
  TabMeasureOptions.TabVisible:=False;

  if (tmp.RealData<>nil) and (tmp.RealData.Kind=TDataKind.dkDateTime) then
  begin
    CBDatePart.Enabled:=True;
    SetPart(CBDatePart,IDimension.DatePart);
  end
  else
  begin
    CBDatePart.Enabled:=False;
    SetPart(CBDatePart,TDateTimePart.None);
  end;

  CBHistoActive.Checked:=tmp.Histogram.Active;
  UDBins.Position:=tmp.Histogram.NumBins;
  EnableHistogramControls;
end;

procedure TBIQueryEditor.SetDataProperties(const AItem:TQueryDimension);
begin
  SetDataProperties(AItem.Data,AItem.Expression);
end;

procedure TBIQueryEditor.SetDataProperties(const AItem:TQueryMeasure);
begin
  SetDataProperties(AItem.Data,AItem.Expression);
end;

procedure TBIQueryEditor.SetDataProperties(const AData:TDataItem; const AExpression:TExpression);
var tmp : TExpression;
    tmpKind : TDataKind;
begin
  tmp:=AExpression;

  if tmp=nil then
     if AData=nil then
        tmpKind:=TDataKind.dkUnknown
     else
        tmpKind:=AData.Kind
  else
     tmpKind:=TDataExpression.KindOf(tmp);

  LabelItemKind.Caption:=tmpKind.ToString;

  if tmp=nil then
     EItemExpression.Text:=''
  else
     EItemExpression.Text:=tmp.ToString;

  LExpressionError.Caption:='';
end;

procedure EnableUpDown(const AList:TCheckListBox; const AUp,ADown:TControl);
var tmp : Integer;
begin
  tmp:=AList.ItemIndex;

  AUp.Enabled:=tmp>0;
  ADown.Enabled:=(tmp<>-1) and (tmp<AList.Count-1);
end;

procedure TBIQueryEditor.EnableColumnButtons;
begin
  BDeleteColumn.Enabled:=ListColumns.ItemIndex<>-1;
  EnableUpDown(ListColumns,SBColUp,SBColDown);
end;

procedure TBIQueryEditor.ListColumnsClick(Sender: TObject);
begin
  EnableColumnButtons;

  PageOptions.Visible:=BDeleteColumn.Enabled and (BIQuery1.Style=TQueryStyle.Summary);

  if PageOptions.Visible then
     SetItemProperties(DimensionOf(ListColumns));

  if ListColumns.ItemIndex<>-1 then
  begin
    ListRows.ItemIndex:=-1;
    EnableRowButtons;

    ListMeasures.ItemIndex:=-1;
    EnableMeasureButtons;
  end;

  EnableSwap;
end;

procedure TBIQueryEditor.EnableMeasureButtons;
begin
  BDeleteMeasure.Enabled:=ListMeasures.ItemIndex<>-1;
  EnableUpDown(ListMeasures,SBMeasureUp,SBMeasureDown);
end;

procedure TBIQueryEditor.ListMeasuresClick(Sender: TObject);
begin
  EnableMeasureButtons;

  PageOptions.Visible:=BDeleteMeasure.Enabled;

  if PageOptions.Visible then
     SetItemProperties(MeasureOf(ListMeasures));

  if ListMeasures.ItemIndex<>-1 then
  begin
    ListRows.ItemIndex:=-1;
    EnableRowButtons;

    ListColumns.ItemIndex:=-1;
    EnableColumnButtons;
  end;
end;

procedure TBIQueryEditor.ListMeasuresDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);

  function DataFromSelector:TDataItem;
  begin
    result:=nil;

    if ISelector<>nil then
       if Source=DataTree then
          result:=ISelector.Manager.SelectedData
       else
       if Source=CompTree then
          result:=ISelector.ComponentSelector.SelectedData;
  end;

  function DataFromListBox:TDataItem;
  var tmp : Integer;
  begin
    result:=nil;

    if Source is TCheckListBox then
    begin
      tmp:=TCheckListBox(Source).ItemIndex;

      if tmp<>-1 then
         if Source=ListMeasures then
            result:=MeasureOf(TCheckListBox(Source)).Data
         else
            result:=DimensionOf(TCheckListBox(Source)).Data;
    end;
  end;

  function IsMeasure:Boolean;
  var tmp : TDataItem;
  begin
    tmp:=DataFromSelector;

    if tmp=nil then
       tmp:=DataFromListBox;

    result:=(tmp<>nil) and TBIQuery.CanBeMeasure(tmp);
  end;

begin
  Accept:=(Source=ListMeasures) or IsMeasure;
end;

procedure TBIQueryEditor.ChangeCurrentMeasure;
var tmp : TQueryMeasure;
begin
  tmp:=IMeasure;

  TabMeasureOptions.TabVisible:=True;
  PageOptions.ActivePage:=TabMeasureOptions;
  TabItem.TabVisible:=False;

  CBAggregate.ItemIndex:=Ord(tmp.Aggregate);
  CBMissingAsZero.Checked:=tmp.Missing.AsZero;
  RGPercentage.ItemIndex:=Ord(tmp.Calculation.Percentage);
  RGRunning.ItemIndex:=Ord(tmp.Calculation.Running);
  CBRunningRows.Checked:=tmp.Calculation.RunningByRows;
end;

procedure TBIQueryEditor.SetItemProperties(const ACurrent:TQueryDimension);
var tmp : Boolean;
begin
  if IDimension<>ACurrent then
  begin
    IChanging:=True;
    try
      IDimension:=ACurrent;
      IMeasure:=nil;

      tmp:=PageOptions.ActivePage=TabItemData;

      ChangeCurrentBy;

      SetDataProperties(IDimension);

      if tmp then
         PageOptions.ActivePage:=TabItemData;
    finally
      IChanging:=False;
    end;
  end;
end;

procedure TBIQueryEditor.SetItemProperties(const ACurrent:TQueryMeasure);
var tmp : Boolean;
begin
  if IMeasure<>ACurrent then
  begin
    IChanging:=True;
    try
      IMeasure:=ACurrent;
      IDimension:=nil;

      tmp:=PageOptions.ActivePage=TabItemData;

      ChangeCurrentMeasure;

      SetDataProperties(IMeasure);

      if tmp then
         PageOptions.ActivePage:=TabItemData;
    finally
      IChanging:=False;
    end;
  end;
end;

procedure TBIQueryEditor.Modified;
begin
  if Preview then
     TryShowSQL;

  if PanelButtons.Visible then
  begin
    IModified:=True;
    BOK.Enabled:=True;
  end
  else
    IQuery.Assign(BIQuery1);
end;

procedure TBIQueryEditor.SortChanged(Sender:TObject);
begin
  Modified;
end;

procedure TBIQueryEditor.PageDataChange(Sender: TObject);
begin
  if PageData.ActivePage=TabFilter then
  begin
    if IFilter=nil then
    begin
      IFilter:=TDynamicFilterEditor.Embedd(Self,TabFilter,nil,nil);
      IFilter.OnChange:=ChangedFilter;
    end;

    if IFilter.BITree1.Plugin.Count=0 then
       RefreshFilter;
  end
  else
  if PageData.ActivePage=TabSort then
     if ISort=nil then
     begin
       ISort:=TSortEditor.Embedd(Self,TabSort,BIQuery1.SortBy);
       ISort.OnChanged:=SortChanged;
     end;
end;

procedure TBIQueryEditor.ShowSQL;
begin
  MemoSQL.Text:=TBISQL.From(BIQuery1);
end;

procedure TBIQueryEditor.TryShowSQL;
begin
  if PagePreview.ActivePage=TabSQL then
     ShowSQL;
end;

procedure TBIQueryEditor.PagePreviewChange(Sender: TObject);
begin
  TryShowSQL;
end;

procedure TBIQueryEditor.SetPart(const ACombo:TComboBox; const APart:TDateTimePart);
begin
  if ACombo.Items.Count=0 then
     ACombo.Items.Text:=TDateTimePart.AllToText;

  ACombo.ItemIndex:=Ord(APart);
end;

procedure TBIQueryEditor.TryShowPreviewGrid;
begin
  if BIGrid1.Provider=nil then
  begin
    BIGrid1.Provider:=BIQuery1;

    if Assigned(TBIQueryEditor.OnShowEditor) then
       TBIQueryEditor.OnShowEditor(TabChart,BIQuery1,nil)
    else
       TabChart.TabVisible:=False;
  end;
end;

procedure TBIQueryEditor.SetPreview(const Value: Boolean);
begin
  if not ISettingPreview then
  begin
    ISettingPreview:=True;
    try
      PagePreview.Visible:=Value;
      SplitterPreview.Visible:=Value;

      if PagePreview.Visible then
      begin
        PanelEdit.Align:=TAlign.alTop;
        PanelEdit.Height:=OuterPanel.Height div 2;

        TryShowPreviewGrid;
      end
      else
         PanelEdit.Align:=TAlign.alClient;

      CBPreview.Checked:=Value;
    finally
      ISettingPreview:=False;
    end;
  end;
end;

function TBIQueryEditor.ResolveData(const APos:Integer; const AMessage:String):Boolean;
begin
  LExpressionError.Caption:=AMessage;
  result:=True;
end;

function TBIQueryEditor.Resolver(const S:String; IsFunction:Boolean):TExpression;
begin
  if IsFunction then
     result:=nil
  else
     result:=TDataExpression.FromString(BIQuery1.Main,S,ResolveData);
end;

function TBIQueryEditor.EditResolver(const S:String; IsFunction:Boolean):TExpression;
begin
  if IsFunction then
     result:=nil
  else
     result:=TDataExpression.FromString(BIQuery1.Main,S);
end;

procedure TBIQueryEditor.SpeedButton1Click(Sender: TObject);
var tmp : TExpression;
begin
  if IMeasure=nil then
     tmp:=IDimension.Expression
  else
     tmp:=IMeasure.Expression;

  if TExpressionEditor.Edit(Self,tmp,EditResolver) then
  begin
    if IMeasure=nil then
       IDimension.Expression:=tmp
    else
       IMeasure.Expression:=tmp;

    if tmp=nil then
       EItemExpression.Text:=''
    else
       EItemExpression.Text:=tmp.ToString;
  end;
end;

procedure TBIQueryEditor.SBSwapClick(Sender: TObject);

  procedure DoRemove(const AList:TCheckListBox; const AItem:TQueryItem);
  var tmp : Integer;
  begin
    tmp:=AList.Items.IndexOfObject(AItem);
    AList.Items.Delete(tmp);
  end;

var tmp : TQueryDimension;
    t : Integer;
    tmpChanged : Boolean;
begin
  tmpChanged:=False;

  TDataProviderAccess(BIQuery1).BeginUpdate;

  for t:=0 to BIQuery1.Dimensions.Count-1 do
  begin
    tmp:=BIQuery1.Dimensions[t];

    case tmp.RealStyle of
       TDimensionStyle.Row:
          begin
            tmp.Style:=TDimensionStyle.Column;

            DoRemove(ListRows,tmp);
            AddItem(ListColumns,tmp);
            tmpChanged:=True;
          end;

       TDimensionStyle.Column:
          begin
            tmp.Style:=TDimensionStyle.Row;

            DoRemove(ListColumns,tmp);
            AddItem(ListRows,tmp);
            tmpChanged:=True;
          end;
     end;
  end;

  TDataProviderAccess(BIQuery1).EndUpdate;

  if tmpChanged then
     Modified;
end;

procedure TBIQueryEditor.AdjustSelectorCaption;
begin
  if PanelSelector.Visible then
     SBSelector.Caption:='<<'
  else
     SBSelector.Caption:='>>';
end;

function TBIQueryEditor.Selector:TDataSelector;
begin
  if ISelector=nil then
  begin
    ISelector:=TDataSelector.Embedd(Self,TabData,nil);
    ISelector.ComponentSelector.OnFilter:=FilterComponent;

    DataTree:=ISelector.Manager.Tree;
    DataTree.DragMode:=TDragMode.dmAutomatic;

    CompTree:=ISelector.ComponentSelector.Tree;
    CompTree.DragMode:=TDragMode.dmAutomatic;

    TDataSelectorAccess(ISelector).SetEdited(BIQuery1);

    RefreshSelector;
  end;

  result:=ISelector;
end;

procedure TBIQueryEditor.ShowSelector(const AShow:Boolean);
begin
  PanelSelector.Visible:=AShow;
  SplitterSelector.Visible:=PanelSelector.Visible;

  if SplitterSelector.Visible then
     SplitterSelector.Left:=PanelSelector.Left+PanelSelector.Width+1;

  AdjustSelectorCaption;

  if PanelSelector.Visible then
     Selector;
end;

procedure TBIQueryEditor.SBSelectorClick(Sender: TObject);
begin
  ShowSelector(not PanelSelector.Visible);
end;

procedure TBIQueryEditor.EnableRowButtons;
begin
  BDeleteRow.Enabled:=ListRows.ItemIndex<>-1;
  EnableUpDown(ListRows,SBRowUp,SBRowDown);
end;

procedure TBIQueryEditor.ListRowsClick(Sender: TObject);
begin
  EnableRowButtons;

  PageOptions.Visible:=BDeleteRow.Enabled and (BIQuery1.Style=TQueryStyle.Summary);

  if PageOptions.Visible then
     SetItemProperties(DimensionOf(ListRows));

  if ListRows.ItemIndex<>-1 then
  begin
    ListMeasures.ItemIndex:=-1;
    EnableMeasureButtons;

    ListColumns.ItemIndex:=-1;
    EnableColumnButtons;
  end;

  EnableSwap;
end;

procedure TBIQueryEditor.ListRowsClickCheck(Sender: TObject);
var tmp : TCheckListBox;
begin
  tmp:=Sender as TCheckListBox;
  DimensionOf(tmp).Enabled:=tmp.Checked[tmp.ItemIndex];

  EnableRowSettings;

  Modified;
end;

procedure TBIQueryEditor.DoExchangeItem(const AList:TCheckListBox; const A,B:Integer);

  procedure DoMeasures;
  var tmpA,
      tmpB : TQueryMeasure;
  begin
    tmpA:=MeasureOf(AList,A);
    tmpB:=MeasureOf(AList,B);

    BIQuery1.Measures.Exchange(tmpA,tmpB);
  end;

  procedure DoDimensions;
  var tmpA,
      tmpB : TQueryDimension;
  begin
    tmpA:=DimensionOf(AList,A);
    tmpB:=DimensionOf(AList,B);

    BIQuery1.Dimensions.Exchange(tmpA,tmpB);
  end;

begin
  if AList=ListMeasures then
     DoMeasures
  else
     DoDimensions;

  AList.Items.Exchange(A,B);
  AList.OnClick(AList);
end;

procedure TBIQueryEditor.ListRowsDragDrop(Sender, Source: TObject; X,
  Y: Integer);

  procedure AddNewData(const AData:TDataItem; const IsActive:Boolean=True);
  var tmp : TCheckListBox;
  begin
    tmp:=Sender as TCheckListBox;
    AddData(tmp,AData,IsActive);

    tmp.ItemIndex:=tmp.Count-1;
    tmp.OnClick(tmp);
  end;

  procedure AddFromNode(const ATree:TTreeView);
  var tmp : TTreeNode;
  begin
    if ATree=DataTree then
       AddNewData(ISelector.Manager.SelectedData)
    else
    if ATree=CompTree then
    begin
      tmp:=CompTree.Selected;

      if tmp<>nil then
      begin
        if TObject(tmp.Data) is TDataItem then
           AddNewData(TDataItem(TObject(tmp.Data)))
        else
        if TObject(tmp.Data) is TComponent then
           AddNewData(TControlImporter.From(nil,TComponent(tmp.Data)));
      end;
    end;
  end;

  procedure ExchangeMeasure(const AItem:TQueryMeasure);
  begin
    RemoveFromList(TCheckListBox(Source));
    AddItem(Sender as TCheckListBox,AItem);
  end;

  procedure ExchangeDimension(const AItem:TQueryDimension);
  begin
    if Sender=ListRows then
       AItem.Style:=TDimensionStyle.Row
    else
       AItem.Style:=TDimensionStyle.Column;

    RemoveFromList(TCheckListBox(Source));
    AddItem(Sender as TCheckListBox,AItem);
  end;

  procedure ExchangeItem(const AList:TCheckListBox);
  var tmp : Integer;
  begin
    tmp:=AList.ItemAtPos(Point(X,Y),True);

    if (tmp<>-1) and (tmp<>AList.ItemIndex) then
       DoExchangeItem(AList,AList.ItemIndex,tmp);
  end;

  procedure DimensionToMeasure(const ASource:TCheckListBox);
  var tmp : TQueryDimension;
      tmpIndex : Integer;
  begin
    tmp:=DimensionOf(ASource);
    tmpIndex:=ASource.ItemIndex;

    TDataProviderAccess(BIQuery1).BeginUpdate;
    try
      AddNewData(tmp.Data,tmp.Enabled);

      tmp.Free;
      RemoveFromList(ASource,tmpIndex);
    finally
      TDataProviderAccess(BIQuery1).EndUpdate;
    end;
  end;

var tmp : TCheckListBox;
begin
  if Source is TTreeView then
  begin
    AddFromNode(Source as TTreeView);
    Modified;
  end
  else
  if Source is TCheckListBox then
  begin
    tmp:=TCheckListBox(Source);

    if Sender=Source then // in-list same drag
       ExchangeItem(tmp)
    else
    if Sender=ListMeasures then
    begin
      if Source=ListMeasures then
         ExchangeMeasure(MeasureOf(tmp))
      else
         DimensionToMeasure(tmp);
    end
    else
       ExchangeDimension(DimensionOf(tmp));

    EnableRowSettings;
    Modified;
  end;
end;

procedure TBIQueryEditor.ListRowsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=(Source=DataTree) or (Source=CompTree) or
          (Source=ListColumns) or (Source=ListRows);
end;

class function TBIQueryEditor.Embedd(const AOwner: TComponent;
                                     const AParent: TWinControl;
                                     const AQuery: TBIQuery): TBIQueryEditor;
begin
  result:=TBIQueryEditor.Create(AOwner);

  result.PanelButtons.Visible:=False;
  result.PanelSelector.Visible:=False;
  result.SplitterSelector.Visible:=False;
  result.AdjustSelectorCaption;

  result.Refresh(AQuery);

  TUICommon.AddForm(result,AParent);
end;

end.
