{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid plugin for VCL DBGrid              }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Grid.DBGrid;

interface

uses
  {$IFDEF MSWINDOWS}
  WinAPI.Windows,
  {$ENDIF}
  {$IFDEF FPC}
  BI.FPC,
  {$ELSE}
  Messages, UITypes,
  System.Types, // <-- due to inlining
  {$ENDIF}
  Classes, SysUtils,
  VCL.Controls, VCL.Grids, VCL.DBGrids, VCL.Graphics, Data.DB,
  VCL.Buttons, VCL.Menus, VCL.StdCtrls, VCL.ExtCtrls,

  VCLBI.Grid,
  BI.DataItem, BI.UI, BI.DataSet, BI.DataSource,
  VCLBI.Editor.Search, BI.Grid.Plugin;

type
  {$IFDEF FPC}
  TDrawCellEvent=TOnDrawCell;
  {$ENDIF}

  TBIDBGrid=class(TDBGrid)
  private
  const
    OffsetTotals=2;
    function GetSearchEditor: TSearchEditor;

  type
    TGridMenu=record
    public
      Alternate : TMenuItem;
      Button : TSpeedButton;
      Colorize : TMenuItem;
      Detail : TMenuItem;
      Filters : TMenuItem;
      ExportData : TMenuItem;
      GroupBy : TMenuItem;
      Search : TMenuItem;
      Sort : TMenuItem;
      Popup : TPopupMenu;
      RowNumbers : TMenuItem;
    end;

    TGridGroup=record
    private
      procedure CreateGrid(const AGrid:TBIDBGrid);
      function DBGrid:TBIDBGrid;
      procedure ShowGroup(const AData:TDataItem);
    public
      Grid : TBIGrid;
      Split : TSplitter;
      DataSet : TBIDataset;
      GridOf : TBIDBGrid;
      Index : TCursorIndex;
    end;

  var
    FColorizers : TDataColorizers;
    FColumnSort : Boolean;

    FOnAfterDrawCell,
    FOnBeforeDrawCell :  TDrawCellEvent;

    FTotals: Boolean;

    FLastTitle : Integer;
    FLastWidth : Integer;
    FLastFontStyle : TFontStyles;

    IAlternate : TAlternateColor;
    IAssociated : TPanel;
    IHighLight : TSearchEditor;

    IMenu : TGridMenu;

    IDetailGrid : TBIGrid;
    IDetailSplitter : TSplitter;

    IGroup : TGridGroup;

    IWasDragging : Boolean;

    procedure AddDetailMenu;
    procedure AddGroupByMenu;
    class function AddMenuButton(const AParent:TWinControl; const ACaption:String;
                           const AHeight:Integer;
                           const AClick:TNotifyEvent):TSpeedButton; static;
    procedure AlternateChanged(Sender:TObject);
    procedure AlternateClick(Sender:TObject);
    procedure ApplyFilter(const AllowAllRows:Boolean; const ACol:Integer; const Value:String);
    function BIDataset:TBIDataset;
    function CalcSortWidth:Integer;
    function CanColorize:Boolean;
    procedure ChangeDataSet(const ADataSet:TDataSet);
    procedure ChangedFilter(const ACol:Integer; const Value:String);
    procedure ChangedSearch(Sender:TObject);
    procedure CheckDataSource;
    procedure CheckFilterGrid;
    procedure CloseGroup(Sender:TObject);
    function Colorizable:TDataColorizers;
    procedure ColorizeClick(Sender:TObject);
    procedure DestroyMenu;
    function DetailItems:TDataArray;
    procedure DetailClicked(Sender: TObject);
    procedure DoGroupBy(Sender:TObject);
    procedure DoTitleClick(AColumn:TColumn);
    procedure ExportDataClick(Sender:TObject);
    function GetData:TDataItem;
    function GetDetail: TDataItem;
    procedure GroupDataChange(Sender: TObject; Field: TField);
    function HasFilterGrid:Boolean;
    function HasSearchPanel:Boolean;
    procedure MenuClick(Sender:TObject);
    procedure MenuFiltersClick(Sender: TObject);
    procedure MenuPopup(Sender:TObject);
    procedure MenuSearchClick(Sender: TObject);
    procedure MenuSortClick(Sender:TObject);
    function ParentsOf(AField:TField):Integer;
    procedure RepaintTotals;
    procedure ResetFeatures;
    procedure RowNumbersClick(Sender:TObject);
    procedure SetColumnSort(const Value:Boolean);
    procedure SetDetail(const AData:TDataItem);
    function TitleWidth(const AColumn:TColumn):Integer;
    function TryColorize(const ARow,ACol:Integer; const AColumn:TColumn; out AIndex:Integer; out APercent:Double):Boolean;
    function TryFontColor(const AIndex:Integer; const ANewColor,ADefault:TColor):TColor;

    {$IFNDEF FPC}
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPaint;
    {$ENDIF}
  protected
    function ColumnOf(const X:Integer):TColumn; overload;

    {$IFDEF FPC}
    procedure ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer); override;
    {$ELSE}
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    {$ENDIF}

    procedure ColWidthsChanged; override;

    function CreateColumns: TDBGridColumns; override;

    {$IFDEF FPC}
    procedure DoPrepareCanvas(aCol,aRow:Integer; aState: TGridDrawState); override;
    {$ENDIF}

    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;

    function GetClientRect:TRect; override;
    procedure LinkActive(Value: Boolean); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    {$IFDEF FPC}
    procedure HeaderClick(IsColumn: Boolean; index: Integer); override;
    {$ELSE}
    procedure SetColumnAttributes; override;
    procedure TitleClick(Column: TColumn); override;
    {$ENDIF}

    procedure Resize; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure TopLeftChanged; override;
    function TotalWidth:Integer;
    procedure TryClearFilter;
    procedure TryCloseGroup;
  public
    {$IFDEF FPC}
    IndicatorOffset : Integer;
    {$ENDIF}

    ScrollTrack : Boolean;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    function ColumnOf(const AField:TField):TColumn; overload;
    function ColumnOf(const AData:TDataItem):TColumn; overload;
    procedure Traverse(const AColumn:TColumn; const AProc:TProc<TColumn>);

    property Detail:TDataItem read GetDetail write SetDetail;
    property MenuButton:TSpeedButton read IMenu.Button;
    property SearchEditor:TSearchEditor read GetSearchEditor;
    property TopRow;
  published
    property ColumnSort:Boolean read FColumnSort write SetColumnSort default True;
    property DefaultColWidth;
    property DefaultRowHeight;
    {$IFDEF FPC}
    property Flat default True;
    {$ELSE}
    property DrawingStyle default TGridDrawingStyle.gdsThemed;
    {$ENDIF}
    property GridLineWidth;
    {$IFNDEF FPC}
    property IndicatorOffset;
    {$ENDIF}
    property ReadOnly default True;

    property OnAfterDrawCell:TDrawCellEvent read FOnAfterDrawCell write FOnAfterDrawCell;
    property OnBeforeDrawCell:TDrawCellEvent read FOnBeforeDrawCell write FOnBeforeDrawCell;
  end;

  TBIDBGridPlugin=class(TBIGridPlugin)
  private
    IGrid : TBIDBGrid;
    IPanel : TPanel;
  protected
    procedure AutoWidth; override;
    procedure ChangedAlternate(Sender:TObject); override;
    function GetDataSource: TDataSource; override;
    function GetEditorClass:String; override;
    function GetReadOnly:Boolean; override;
    function GetTotals:Boolean; override;
    procedure SetDataSource(const Value: TDataSource); override;
    procedure SetFilters(const Value:Boolean); override;
    procedure SetReadOnly(const Value:Boolean); override;
    procedure SetRowNumber(const Value:Boolean); override;
    procedure SetSearch(const Value:Boolean); override;
    procedure SetTotals(const Value:Boolean); override;
  public
    Constructor Create(const AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure BindTo(const ADataSet:TDataSet); override;
    procedure Colorize(const AItems:TDataColorizers); override;
    procedure Duplicates(const AData:TDataItem; const Hide:Boolean); override;
    function GetObject:TObject; override;
  end;

implementation

uses
  BI.DB.DataSet, Math, BI.Arrays, BI.UI.Colors, VCL.Forms, BI.Search,
  VCLBI.Editor.ExportData, VCLBI.Menus;

const
  BIMenu_GroupBy='&Group by';
  BIMenu_Close='&Close';
  BIMenu_AlternateRows='&Alternate Rows';
  BIMenu_ColorizeColumns='&Colorize';
  BIMenu_ColumnFilters='&Column Filters';
  BIMenu_ColumnSort='&Column Sort';
  BIMenu_Detail='&Detail';
  BIMenu_Export='&Export Data...';
  BIMenu_RowNumbers='&Row Numbers';
  BIMenu_Search='&Search';

  {$IFDEF FPC}
  IndicatorWidth=12; // Hardcoded at DBGrids
  {$ENDIF}

type
  TFilterGrid=class(TPanel)
  private
    ICloseButton : TSpeedButton;
    IGrid : TBIDBGrid;

    Items : Array of TControl;

    procedure ClearCombos;
    procedure ColumnMoved(FromIndex, ToIndex, AOffset: Integer);
    procedure ColWidthsChanged;
    procedure DroppedCombo(Sender:TObject);
    procedure EditChange(Sender:TObject);
    function GetText(const ACol:Integer):String;
    procedure SetLeftCol(const ACol:Integer);
  public
    Constructor Create(AOwner:TComponent); override;

    procedure Clear;
    procedure Reset;
    procedure Show(const AVisible:Boolean);
  end;

{ TBIDBGridPlugin }

Constructor TBIDBGridPlugin.Create(const AOwner: TComponent);
begin
  inherited;

  IPanel:=TPanel.Create(AOwner);
  IPanel.BevelOuter:=TBevelCut.bvNone;
  IPanel.Caption:='';
  IPanel.Align:=TAlign.alClient;

  if AOwner is TWinControl then
     IPanel.Parent:=TWinControl(AOwner);

  IGrid:=TBIDBGrid.Create(AOwner);
  IGrid.Align:=TAlign.alClient;

  IGrid.Parent:=IPanel;
end;

Destructor TBIDBGridPlugin.Destroy;
begin
  IGrid.Free;
  IPanel.Free;
  inherited;
end;

procedure TBIDBGridPlugin.SetRowNumber(const Value: Boolean);
begin
  if (DataSource<>nil) and (DataSource.DataSet is TBIDataset) then
     TBIDataSet(DataSource.DataSet).RowNumbers:=Value;
end;

procedure TBIDBGridPlugin.SetSearch(const Value: Boolean);
begin
  IGrid.SearchEditor.Visible:=Value;

  if IGrid.IMenu.Search<>nil then
     IGrid.IMenu.Search.Checked:=Value;
end;

procedure TBIDBGridPlugin.SetTotals(const Value:Boolean);
begin
  if IGrid.FTotals<>Value then
  begin
    IGrid.FTotals:=Value;

    IGrid.LayoutChanged;

    if Value then
       IGrid.RepaintTotals;
  end;
end;

procedure TBIDBGridPlugin.Colorize(const AItems: TDataColorizers);
begin
  if (IGrid.FColorizers<>nil) or (AItems<>nil) then // <-- avoid un-needed Repaint
  begin
    IGrid.FColorizers:=AItems;
    IGrid.Repaint;
  end;
end;

procedure TBIDBGridPlugin.Duplicates(const AData:TDataItem; const Hide:Boolean);
var tmpD : TDataSource;
    tmp : TField;
begin
  tmpD:=IGrid.DataSource;

  if tmpD<>nil then
  begin
    tmp:=TBIDataSetSource.FieldOf(AData,tmpD.DataSet);

    if tmp<>nil then
    begin
      (tmpD.DataSet as TBIDataSet).SetFieldOnGetText(tmp,Hide);
      IGrid.InvalidateGrid;
    end;
  end;
end;

function TBIDBGridPlugin.GetObject: TObject;
begin
  result:=IGrid;
end;

function TBIDBGridPlugin.GetDataSource: TDataSource;
begin
  result:=IGrid.DataSource;
end;

function TBIDBGridPlugin.GetEditorClass: String;
begin
  result:='TBIDBGridEditor';
end;

function TBIDBGridPlugin.GetReadOnly: Boolean;
begin
  result:=IGrid.ReadOnly;
end;

function TBIDBGridPlugin.GetTotals: Boolean;
begin
  result:=IGrid.FTotals;
end;

procedure TBIDBGridPlugin.ChangedAlternate(Sender:TObject);
begin
  IAlternate:=TAlternateColor(Sender);
  IGrid.AlternateChanged(Sender);
end;

procedure TBIDBGridPlugin.AutoWidth;
begin
  inherited;
  IGrid.Parent.Width:=IGrid.TotalWidth;
end;

procedure TBIDBGridPlugin.BindTo(const ADataSet:TDataSet);
begin
  IGrid.ChangeDataSet(ADataSet);
end;

procedure TBIDBGridPlugin.SetDataSource(const Value: TDataSource);
begin
  IGrid.DataSource:=Value;
end;

procedure TBIDBGridPlugin.SetFilters(const Value: Boolean);
begin
  IGrid.CheckFilterGrid;
  TFilterGrid(IGrid.IAssociated).Show(Value);
end;

procedure TBIDBGridPlugin.SetReadOnly(const Value: Boolean);
begin
  IGrid.ReadOnly:=Value;
end;

type
  TBIDBGridColumns=class(TDBGridColumns)
  private
    IUpdating : Boolean;
  protected
    procedure Update(Item: TCollectionItem); override;
  end;

{ TBIDBGridColumns }

procedure TBIDBGridColumns.Update(Item: TCollectionItem);
begin
  if not IUpdating then
     inherited;
end;

{ TBIDBGrid }

Constructor TBIDBGrid.Create(AOwner: TComponent);
begin
  inherited;

  FLastTitle:=-1;

  ScrollTrack:=True;

  {$IFDEF FPC}
  BorderStyle:=TBorderStyle.bsNone;
  Flat:=True;
  Options:=Options+[dgHeaderHotTracking,dgThumbTracking];

  IndicatorOffset:=1; // <-- set to 0 when no dgIndicator ! (make it a function)
  
  {$ELSE}
  DrawingStyle:=TGridDrawingStyle.gdsThemed;
  {$ENDIF}

  FColumnSort:=True;

  ReadOnly:=True;

  IMenu.Button:=AddMenuButton(Self,'...',DefaultRowHeight-2,MenuClick);
  IMenu.Button.Width:=IndicatorWidth;
end;

Destructor TBIDBGrid.Destroy;
begin
  if IGroup.DataSet<>nil then
     IGroup.DataSet.Data.Free;

  if IGroup.GridOf<>nil then
     IAlternate.Free;

  inherited;
end;

procedure TBIDBGrid.CheckFilterGrid;

  procedure AddButton(const AEvent:TNotifyEvent);
  const
    CloseCaption='x';
  begin
    TFilterGrid(IAssociated).ICloseButton:=AddMenuButton(IAssociated,CloseCaption,
                            8+(DefaultRowHeight div 2),AEvent);

    TFilterGrid(IAssociated).ICloseButton.Hint:='Close';
    TFilterGrid(IAssociated).ICloseButton.ShowHint:=True;
  end;

begin
  if IAssociated=nil then
  begin
    IAssociated:=TFilterGrid.Create(Owner);
    TFilterGrid(IAssociated).IGrid:=Self;
    TFilterGrid(IAssociated).Hide;
    TFilterGrid(IAssociated).Parent:=Parent;

    AddButton(MenuFiltersClick);
  end;
end;

procedure TBIDBGrid.ChangedSearch(Sender:TObject);
begin
  Repaint;
end;

class function TBIDBGrid.AddMenuButton(const AParent:TWinControl; const ACaption:String;
                                 const AHeight:Integer;
                                 const AClick:TNotifyEvent):TSpeedButton;
begin
  result:=TSpeedButton.Create(AParent);

  result.Flat:=True;
  result.Caption:=ACaption;
  result.Height:=AHeight;
  result.Width:=result.Height;

  result.Parent:=AParent;
  result.OnClick:=AClick;
end;

procedure TBIDBGrid.AlternateChanged(Sender: TObject);
begin
  IAlternate:=TAlternateColor(Sender);

  {$IFDEF FPC}
  if IAlternate.Enabled then
     AlternateColor:=IAlternate.Color
  else
     AlternateColor:=Color;
  {$ELSE}
  Repaint;
  {$ENDIF}
end;

procedure TBIDBGrid.AlternateClick(Sender:TObject);
var tmp : TMenuItem;
begin
  tmp:=(Sender as TMenuItem);
  tmp.Checked:=not tmp.Checked;
  IAlternate.Enabled:=tmp.Checked;
end;

procedure TBIDBGrid.ColorizeClick(Sender:TObject);
var tmp : TMenuItem;
begin
  tmp:=(Sender as TMenuItem);
  tmp.Checked:=not tmp.Checked;

  if tmp.Checked then
     FColorizers:=Colorizable
  else
     FColorizers:=nil;

  Repaint;
end;

procedure TBIDBGrid.ExportDataClick(Sender:TObject);
begin
  TExportData.Show(Self,GetData);
end;

procedure TBIDBGrid.RowNumbersClick(Sender:TObject);
var tmp : TMenuItem;
begin
  tmp:=(Sender as TMenuItem);
  tmp.Checked:=not tmp.Checked;

  BIDataSet.RowNumbers:=tmp.Checked;

  if HasFilterGrid then
     TFilterGrid(IAssociated).Show(True);
end;

function TBIDBGrid.BIDataset:TBIDataset;
begin
  if (DataSource<>nil) and (DataSource.DataSet is TBIDataset) then
     result:=TBIDataset(DataSource.DataSet)
  else
     result:=nil;
end;

type
  TDataAccess=class(TDataItem);

procedure TBIDBGrid.GroupDataChange(Sender: TObject; Field: TField);

  procedure FindItems(out AMasters,ADetails:TDataArray;
                      const AMaster:TDataItem;
                      const ADetail:TDataItem);
  //var t : Integer;
  begin
    AMasters:=nil;
    ADetails:=nil;

    // Pending: (multiple-field master-detail relationship)
    // Find and return all masters in AMaster, that have corresponding
    // detail items using ADetail as a starting point.

    // For now, just return the first item
    AMasters.Add(AMaster.Items[0]);
    ADetails.Add(ADetail);

    {
    for t:=0 to ADetail.Count-1 do
        if TDataAccess(ADetail[t]).HasMaster then
           if ADetail[t].Master.Parent=AMaster then
           begin
             AMasters.Add(ADetail[t].Master);
             ADetails.Add(ADetail[t]);
           end;
    }
  end;

var tmp,
    tmpMaster : TBIDataset;
    tmpDetails,
    tmpMasters : TDataArray;
    tmpPos : TInteger;
begin
  tmp:=BIDataset;

  FindItems(tmpMasters,tmpDetails,tmp.Data,TDataItem(tmp.Tag));

  tmpMaster:=IGroup.GridOf.BIDataset;

  tmpPos:=tmp.Cursor.Position(tmp.RecNo-1);

  IGroup.GridOf.IGroup.Index:=TDataCursor.DetailIndex(tmpMaster.Data,tmpMasters,tmpDetails,tmpPos);

  IGroup.GridOf.ApplyFilter(False,-1,'');

  IGroup.GridOf.TryClearFilter;
end;

procedure TBIDBGrid.LinkActive(Value: Boolean);
begin
  ResetFeatures;
  inherited;
end;

procedure TBIDBGrid.TryClearFilter;
begin
  if IAssociated<>nil then
     TFilterGrid(IAssociated).ClearCombos;
end;

procedure TBIDBGrid.TryCloseGroup;
var tmp : TMenuItem;
    t : Integer;
begin
  if IMenu.GroupBy<>nil then
  begin
    tmp:=IMenu.GroupBy;

    for t:=0 to tmp.Count-1 do
        if tmp.Items[t].Checked then
        begin
          DoGroupBy(tmp.Items[t]);

          TryClearFilter;

          break;
        end;
  end;
end;

procedure TBIDBGrid.DestroyMenu;
begin
  IMenu.Popup.Free;
  IMenu.Popup:=nil;
  IMenu.GroupBy:=nil;
end;

procedure TBIDBGrid.CloseGroup(Sender:TObject);
begin
  if IGroup.GridOf<>nil then
  begin
    IGroup.GridOf.TryCloseGroup;
    DestroyMenu;
  end;
end;

procedure TBIDBGrid.DoGroupBy(Sender:TObject);

  procedure ShowGroup(const AVisible:Boolean; const AData:TDataItem);
  begin
    IGroup.Grid.Visible:=AVisible;
    IGroup.Split.Visible:=AVisible;

    if AVisible then
    begin
      IGroup.ShowGroup(AData);
      IGroup.Split.Left:=IGroup.Grid.BoundsRect.Right+1;
    end
    else
    begin
      if IGroup.Grid.DataSource<>nil then
         IGroup.Grid.DataSource.DataSet:=nil;

      IGroup.DataSet.Cursor.SortBy.Clear;

      if IGroup.Index<>nil then
         IGroup.Index:=nil;

      ApplyFilter(True,-1,'');
    end;
  end;

var tmp : TMenuItem;
begin
  tmp:=(Sender as TMenuItem);
  tmp.Checked:=not tmp.Checked;

  if tmp.Checked then
  begin
    if IGroup.Grid=nil then
       IGroup.CreateGrid(Self);

    ShowGroup(True,TDataItem(tmp.Tag));
  end
  else
    ShowGroup(False,nil);
end;

function TBIDBGrid.DetailItems:TDataArray;
var tmp : TDataItem;

  function IsDetail(const AData:TDataItem):Boolean;
  var tmpItem : TDataItem;
  begin
    if TDataAccess(AData).HasMaster then
    begin
      tmpItem:=AData.Master;
      result:=(tmpItem=tmp) or (tmpItem.Parent=tmp);
    end
    else
      result:=False;
  end;

  procedure AddUnique(const AData:TDataItem);
  begin
    if not result.Exists(AData) then
       result.Add(AData);
  end;

  procedure TryAdd(const AData:TDataItem);
  var tmpItem : TDataItem;
  begin
    if AData.AsTable then
    begin
      for tmpItem in AData.Items.AsArray do
          if IsDetail(tmpItem) then
             AddUnique(AData);
    end
    else
    if IsDetail(AData) then
       AddUnique(AData);
  end;

  procedure TryAddSelf(const AData:TDataItem);
  var tmpItem : TDataItem;
  begin
    if AData.AsTable then
    begin
      for tmpItem in AData.Items.AsArray do
          if IsDetail(tmpItem) then
             AddUnique(tmpItem);

      if IsDetail(AData) then
         AddUnique(AData);
    end;
  end;

var tmpItem : TDataItem;
begin
  result:=nil;

  tmp:=GetData;

  if tmp<>nil then
  begin
    if tmp.Parent<>nil then
       if tmp.Parent.Kind=TDataKind.dkUnknown then
          for tmpItem in tmp.Parent.Items.AsArray do
              if tmpItem<>tmp then
                 TryAdd(tmpItem);

    for tmpItem in tmp.Items.AsArray do
        TryAddSelf(tmpItem);
  end;
end;

function TBIDBGrid.GetData:TDataItem;
var tmp : TBIDataset;
begin
  tmp:=BIDataset;

  if tmp=nil then
     result:=nil
  else
     result:=tmp.Data;
end;

function TBIDBGrid.GetDetail: TDataItem;
var tmp : TBIDBGrid;
begin
  if IDetailGrid=nil then
     result:=nil
  else
  begin
    tmp:=TBIDBGrid(IDetailGrid.Plugin.GetObject);

    if tmp=nil then
       result:=nil
    else
       result:=tmp.BIDataset.Data;
  end;
end;

function TBIDBGrid.GetSearchEditor: TSearchEditor;
begin
  if IHighLight=nil then
  begin
    IHighLight:=TSearchEditor.Embed(Owner,Parent,TAlign.alTop);
    IHighLight.Visible:=False;

    IHighLight.OnChanged:=ChangedSearch;
    IHighLight.OnGetDataSet:=BIDataset;
    IHighLight.SBClose.OnClick:=MenuSearchClick;
  end;

  result:=IHighLight;
end;

procedure TBIDBGrid.AddGroupByMenu;

  function CanGroupBy(const AData:TDataItem):Boolean;
  begin
    result:=(AData.Kind<>TDataKind.dkUnknown) and (not AData.Unique);
  end;

  // Recurvise (examine sub-tables)
  procedure AddItems(const AItems:TDataItems);
  var t : Integer;
      tmpItem : TDataItem;
      tmpMenu : TMenuItem;
  begin
    for t:=0 to AItems.Count-1 do
    begin
      tmpItem:=AItems[t];

      tmpItem.Stats;

      if CanGroupBy(tmpItem) then
      begin
        tmpMenu:=TBIMenu.NewItem(IMenu.Popup,tmpItem.Name,DoGroupBy);
        tmpMenu.RadioItem:=True;
        tmpMenu.Tag:=NativeInt(tmpItem);

        IMenu.GroupBy.Add(tmpMenu);
      end
      else
      if tmpItem.Kind=TDataKind.dkUnknown then
         if tmpItem.AsTable then
            AddItems(tmpItem.Items);
    end;
  end;

var tmp : TDataItem;
begin
  IMenu.GroupBy.Clear;

  tmp:=GetData;

  if (tmp<>nil) and ((tmp.Kind<>TDataKind.dkUnknown) or tmp.AsTable) then
  begin
    tmp.Stats;
    AddItems(tmp.Items);
  end;

  IMenu.GroupBy.Visible:=IMenu.GroupBy.Count>0;
end;

function TBIDBGrid.Colorizable:TDataColorizers;

  function Guess(const AData:TDataItem):TDataColorizers;

    procedure TryAdd(const AData:TDataItem);
    begin
      if AData.Kind.IsNumeric or (AData.Kind=TDataKind.dkDateTime) then
         result.Add(AData);
    end;

  var tmp : TDataItem;
  begin
    if AData<>nil then
     if AData.AsTable then
        for tmp in AData.Items.AsArray do
            TryAdd(tmp)
        else
            TryAdd(AData);
  end;

var tmp : TBIDataset;
begin
  tmp:=BIDataset;

  if tmp=nil then
     result:=nil
  else
     result:=Guess(tmp.Data);
end;

function TBIDBGrid.CanColorize:Boolean;
begin
  result:=Colorizable<>nil;
end;

procedure TBIDBGrid.MenuPopup(Sender:TObject);

  procedure SetSelectedDetail(const AItems:TDataArray);
  begin
    if IDetailGrid=nil then
       IMenu.Detail.Items[0].Checked:=True;
  end;

var tmp : TBIDataset;
    tmpDetail : TDataArray;
begin
  IMenu.Alternate.Visible:=IAlternate<>nil;

  if IAlternate<>nil then
     IMenu.Alternate.Checked:=IAlternate.Enabled;

  tmp:=BIDataset;

  IMenu.ExportData.Enabled:=tmp<>nil;

  IMenu.RowNumbers.Enabled:=tmp<>nil;
  IMenu.RowNumbers.Checked:=(tmp<>nil) and tmp.RowNumbers;

  IMenu.Sort.Checked:=ColumnSort;

  //IMenu.Filters.Visible:=(IAssociated<>nil);

  IMenu.Filters.Checked:=HasFilterGrid; {IMenu.Filters.Visible and IAssociated.Visible;}

  IMenu.Colorize.Enabled:=CanColorize;

  tmpDetail:=DetailItems;

  IMenu.Detail.Enabled:=tmpDetail<>nil;

  if IMenu.Detail.Enabled then
     SetSelectedDetail(tmpDetail);

  IMenu.ExportData.Enabled:=TExportData.HasAnyFormat;
end;

procedure TBIDBGrid.MenuFiltersClick(Sender: TObject);
var tmp : Boolean;
begin
  tmp:=not HasFilterGrid;

  if IMenu.Filters<>nil then
     IMenu.Filters.Checked:=tmp;

  if tmp then
  begin
    CheckFilterGrid;
    TFilterGrid(IAssociated).Show(True);
  end
  else
  if IAssociated<>nil then
     TFilterGrid(IAssociated).Show(False);
end;

procedure TBIDBGrid.DetailClicked(Sender: TObject);
var tmp : TObject;
    tmpMenu : TMenuItem;
begin
  tmpMenu:=TMenuItem(Sender);

  tmpMenu.Checked:=True;

  tmp:=TObject(tmpMenu.Tag);

  if tmp is TDataItem then
     Detail:=TDataItem(tmp)
  else
     Detail:=nil;
end;

procedure TBIDBGrid.MenuSearchClick(Sender: TObject);
var tmp : Boolean;
begin
  if IMenu.Search=nil then
     tmp:=False
  else
  begin
    tmp:=not IMenu.Search.Checked;
    IMenu.Search.Checked:=tmp;
  end;

  if tmp then
  begin
    SearchEditor.Visible:=True;
    IHighLight.ESearch.SetFocus;
  end
  else
  if IHighLight<>nil then
     IHighLight.Visible:=False;
end;

procedure TBIDBGrid.MenuSortClick(Sender: TObject);
begin
  IMenu.Sort.Checked:=not IMenu.Sort.Checked;
  ColumnSort:=IMenu.Sort.Checked;
end;

procedure TBIDBGrid.AddDetailMenu;
var tmp : TMenuItem;
    tmpItem : TDataItem;
begin
  tmp:=TBIMenu.NewItem(IMenu.Popup,'None',DetailClicked);
  tmp.RadioItem:=True;
  tmp.Checked:=True;

  IMenu.Detail.Add(tmp);

  for tmpItem in DetailItems do
  begin
    tmp:=TBIMenu.NewItem(IMenu.Popup,tmpItem.Name,DetailClicked);
    tmp.RadioItem:=True;
    tmp.Tag:=NativeInt(tmpItem);

    IMenu.Detail.Add(tmp);
  end;
end;

procedure TBIDBGrid.MenuClick(Sender:TObject);
begin
  if IMenu.Popup=nil then
  begin
    IMenu.Popup:=TPopupMenu.Create(Self);

    if IGroup.GridOf=nil then
    begin
      IMenu.GroupBy:=TBIMenu.Add(IMenu.Popup,BIMenu_GroupBy);
      AddGroupByMenu;
    end
    else
      IMenu.GroupBy:=TBIMenu.Add(IMenu.Popup,BIMenu_Close,CloseGroup);

    TBIMenu.AddSeparator(IMenu.Popup);
    IMenu.Alternate:=TBIMenu.Add(IMenu.Popup,BIMenu_AlternateRows,AlternateClick);
    IMenu.Colorize:=TBIMenu.Add(IMenu.Popup,BIMenu_ColorizeColumns,ColorizeClick);
    IMenu.Filters:=TBIMenu.Add(IMenu.Popup,BIMenu_ColumnFilters,MenuFiltersClick);
    IMenu.Search:=TBIMenu.Add(IMenu.Popup,BIMenu_Search,MenuSearchClick);
    IMenu.Sort:=TBIMenu.Add(IMenu.Popup,BIMenu_ColumnSort,MenuSortClick);
    IMenu.RowNumbers:=TBIMenu.Add(IMenu.Popup,BIMenu_RowNumbers,RowNumbersClick);

    TBIMenu.AddSeparator(IMenu.Popup);
    IMenu.Detail:=TBIMenu.Add(IMenu.Popup,BIMenu_Detail);

    AddDetailMenu;

    IMenu.ExportData:=TBIMenu.Add(IMenu.Popup,BIMenu_Export,ExportDataClick);

    IMenu.Popup.OnPopup:=MenuPopup;
  end;

  TBIMenu.Popup(IMenu.Popup,IMenu.Button);
end;

function TBIDBGrid.CreateColumns: TDBGridColumns;
begin
  result:=TBIDBGridColumns.Create(Self, TColumn);
end;

function TBIDBGrid.CalcSortWidth:Integer;
begin
  result:=DefaultRowHeight div 2;
end;

function TBIDBGrid.TryColorize(const ARow,ACol:Integer; const AColumn:TColumn; out AIndex:Integer; out APercent:Double):Boolean;
var tmpData : TBIDataSet;
    tmpItem : TDataItem;
    tmpRecNo : Integer;
begin
  result:=False;

  if AColumn<>nil then
  if DataSource.DataSet is TBIDataset then
  begin
    tmpData:=TBIDataSet(DataSource.DataSet);
    tmpItem:=tmpData.DataOf(AColumn.Field);

    if tmpItem<>nil then
    begin
      tmpRecNo:=tmpData.Cursor.Position(tmpData.RecNo-1+ARow-Row);
      result:=FColorizers.TryColorize(tmpItem,tmpRecNo,APercent,AIndex);
    end;
  end;
end;

function TBIDBGrid.TryFontColor(const AIndex:Integer; const ANewColor,ADefault:TColor):TColor;
begin
  if FColorizers[AIndex].TextColor=TColorizeTextColor.Automatic then
     if TColorFunction.ToRGB(ANewColor)>$7FFFFF then
        result:={$IFDEF FPC}clBlack{$ELSE}TColors.Black{$ENDIF}
     else
        result:={$IFDEF FPC}clWhite{$ELSE}TColors.White{$ENDIF}
  else
     result:=ADefault;
end;

{$IFDEF FPC}
procedure TBIDBGrid.DoPrepareCanvas(aCol,aRow:Integer; aState: TGridDrawState);
var NewColor : TColor;
    tmpIndex : Integer;
    tmpPercent : Double;
begin
  inherited;

  if (FColorizers<>nil) and (ARow>=TopRow) then
  begin
    if TryColorize(ARow,ACol,ColumnOf(ACol),tmpIndex,tmpPercent) then
    begin
      NewColor:=FColorizers[tmpIndex].ColorOf(tmpPercent);

      Canvas.Brush.Color:=NewColor;
      Canvas.Font.Color:=TryFontColor(tmpIndex,NewColor,Canvas.Font.Color);
    end;
  end;
end;
{$ENDIF}

type
  TColumnFormat=record
  private
    FCol : TColumn;

    OldBackColor,
    OldFontColor : TColor;

    function Columns:TBIDBGridColumns;
  public
    procedure Restore;
    procedure Save(const ACol:TColumn);
  end;

function TColumnFormat.Columns:TBIDBGridColumns;
begin
  result:=TBIDBGridColumns(TBIDBGrid(FCol.Grid).Columns);
end;

procedure TColumnFormat.Save(const ACol:TColumn);
begin
  FCol:=ACol;
  OldBackColor:=FCol.Color;
  OldFontColor:=FCol.Font.Color;

  Columns.IUpdating:=True;
end;

procedure TColumnFormat.Restore;
begin
  FCol.Color:=OldBackColor;
  FCol.Font.Color:=OldFontColor;

  Columns.IUpdating:=False;
end;

procedure TBIDBGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  OldBrush : TBrushStyle;
  OldColor : TColor;
  OldPen : TPenStyle;

  procedure SaveCanvas;
  begin
    OldBrush:=Canvas.Brush.Style;
    OldColor:=Canvas.Brush.Color;
    OldPen:=Canvas.Pen.Style;
  end;

  procedure RestoreCanvas;
  begin
    Canvas.Brush.Style:=OldBrush;
    Canvas.Brush.Color:=OldColor;
    Canvas.Pen.Style:=OldPen;
  end;

  procedure DrawSortIndicator(const Desc:Boolean);
  const
    SortIndicatorMargin=4;

  var SortWidth,
      Y0,
      Y1,
      X : Integer;
  begin
    SortWidth:=CalcSortWidth;

    X:=ARect.Right-SortWidth-SortIndicatorMargin;

    Y0:=ARect.Top+5;
    Y1:=Y0+SortWidth;

    if Desc then
       Canvas.Polygon([ Point(X,Y1),
                        Point(X+(SortWidth div 2),Y0-1),
                        Point(X+SortWidth,Y1)
                    ])
    else
       Canvas.Polygon([ Point(X,Y0),
                        Point(X+(SortWidth div 2),Y1),
                        Point(X+SortWidth,Y0)
                    ]);
  end;

var
  Col : TColumn;

  procedure TryDrawSortIndicator;
  var MasterCol : TColumn;
      tmpCol : TDataItem;
      tmpData : TBIDataset;
      tmpIndex : Integer;
  begin
    if Col<>nil then
    begin
      {$IFDEF FPC}
      MasterCol:=nil;
      {$ELSE}
      CalcTitleRect(Col,ARow,MasterCol);
      {$ENDIF}

      if (MasterCol<>nil) and (MasterCol.Field<>nil) then
      if ParentsOf(MasterCol.Field)=ARow then
      if DataSource<>nil then
         if DataSource.DataSet is TBIDataSet then
            if DataSource.DataSet.Active then
            begin
              tmpData:=TBIDataSet(DataSource.DataSet);
              tmpCol:=tmpData.DataOf(MasterCol.Field);

              if tmpCol<>nil then
              begin
                tmpIndex:=tmpData.Cursor.SortBy.IndexOf(tmpCol);

                if tmpIndex<>-1 then
                begin
                  SaveCanvas;
                  try
                    Canvas.Brush.Style:=bsSolid;
                    Canvas.Brush.Color:=clDkGray;
                    Canvas.Pen.Style:=psClear;

                    DrawSortIndicator(tmpData.Cursor.SortBy.Items[tmpIndex].Descending);
                  finally
                    RestoreCanvas;
                  end;
                end;
              end;
            end;
    end;
  end;

  function RectPercent(const APercent:Single):TRect;
  begin
    result:=ARect;
    result.Right:=result.Left+Round((result.Right-result.Left)*APercent);
  end;

  procedure TryOnAfterDrawCell;
  begin
    if Assigned(FOnAfterDrawCell) then
       FOnAfterDrawCell(Self,ACol,ARow,ARect,AState);
  end;

  procedure TryOnBeforeDrawCell;
  begin
    if Assigned(FOnBeforeDrawCell) then
       FOnBeforeDrawCell(Self,ACol,ARow,ARect,AState);
  end;

  function DoColorize:Boolean;
  var NewColor : TColor;
      tmpIndex : Integer;
      tmpPercent : Double;
      tmpFormat : TColumnFormat;
  begin
    result:=TryColorize(ARow,ACol,Col,tmpIndex,tmpPercent);

    if result then
    begin
      NewColor:=FColorizers[tmpIndex].ColorOf(tmpPercent);

      tmpFormat.Save(Col);

      Col.Color:=NewColor;
      Col.Font.Color:=TryFontColor(tmpIndex,NewColor,Col.Font.Color);

      TryOnBeforeDrawCell;

      if FColorizers[tmpIndex].Mode=TColorizeMode.Full then
         inherited
      else
         inherited DrawCell(ACol,ARow,RectPercent(FColorizers[tmpIndex].Normalized(tmpPercent)),AState);

      TryOnAfterDrawCell;

      tmpFormat.Restore;
    end;
  end;

  procedure DrawBack(const AColor:TColor; const ABorder:TColor=clNone);
  var tmp : TColumnFormat;
  begin
    tmp.Save(Col);

    Col.Color:=AColor;

    TryOnBeforeDrawCell;
    inherited;

    if ABorder<>clNone then
    begin
      Canvas.Pen.Color:=ABorder;
      Canvas.Pen.Style:=psSolid;
      Canvas.Brush.Style:=bsClear;
      Canvas.Rectangle(ARect);
    end;

    TryOnAfterDrawCell;

    tmp.Restore;
  end;

  function DatasetNotEmpty:Boolean;
  begin
    result:=(DataSource<>nil) and (DataSource.DataSet<>nil) and
            DataSource.DataSet.Active and
            (DataSource.DataSet.RecordCount>0);
  end;

  function HighLight:Boolean;
  var tmpRecNo : TInteger;
      tmpData : TBIDataSet;
      tmpItem : TDataItem;
      tmpIndex : TInteger;
  begin
    result:=False;

    if (ARow>=TopRow) and (Col<>nil) and (DataSource.DataSet is TBIDataset) then
    begin
      tmpData:=TBIDataSet(DataSource.DataSet);
      tmpItem:=tmpData.DataOf(Col.Field);
      tmpRecNo:=tmpData.Cursor.Position(tmpData.RecNo-1+ARow-Row);

      if tmpRecNo>-1 then
      begin
        result:=IHighLight.HasHit(tmpRecNo,tmpItem,tmpIndex);

        if result then
           if tmpIndex=IHighLight.Current-1 then
              DrawBack(IHighLight.HighLightColor,IHighLight.CurrentColor)
           else
              DrawBack(IHighLight.HighLightColor);
      end;
    end;
  end;

var tmpDone : Boolean;
    tmpRow : Integer;
begin
  Col:=ColumnOf(ACol);

  if Col=nil then
     tmpDone:=False
  else
  begin
    if HasSearchPanel and IHighLight.HasHits then
       tmpDone:=HighLight
    else
       tmpDone:=False;

    if not tmpDone then
       if (FColorizers<>nil) and (ARow>=TopRow) then
          tmpDone:=DoColorize
       else
          tmpDone:=False;

    if not tmpDone then
       if (IAlternate<>nil) and IAlternate.Enabled and DataSetNotEmpty then
       begin
         tmpRow:=DataSource.DataSet.RecNo+ARow-Row;

         if (tmpRow>0) and ((tmpRow mod 2)=0) then
         begin
           DrawBack(IAlternate.Color);
           tmpDone:=True;
         end;
       end;
  end;

  if not tmpDone then
  begin
    TryOnBeforeDrawCell;
    inherited;
    TryOnAfterDrawCell;
  end;

  if (ARow<TopRow) and ColumnSort then
     TryDrawSortIndicator;
end;

function TBIDBGrid.TitleWidth(const AColumn:TColumn):Integer;
begin
  Canvas.Font:=AColumn.Title.Font;
  result:=Canvas.TextWidth(AColumn.Title.Caption)+4;
end;

procedure TBIDBGrid.TopLeftChanged;
begin
  inherited;

  if IAssociated<>nil then
     TFilterGrid(IAssociated).SetLeftCol(LeftCol);
end;

function TBIDBGrid.HasFilterGrid:Boolean;
begin
  result:=(IAssociated<>nil) and IAssociated.Visible;
end;

function TBIDBGrid.HasSearchPanel:Boolean;
begin
  result:=(IHighLight<>nil) and IHighLight.Visible;
end;

{$IFNDEF FPC}
procedure TBIDBGrid.SetColumnAttributes;
var t : Integer;
    tmp : TColumn;
begin
  inherited;

  for t:=0 to Columns.Count-1 do
  begin
    tmp:=Columns[t];

    if tmp.Field<>nil then
    begin
      if (Parent<>nil) and Parent.Showing then
      begin
         if Max(TitleWidth(tmp),tmp.Field.DisplayWidth)<(ColWidths[t+IndicatorOffset]) then
         begin
           // For right alignment, if sort triangle is displayed,
           // title text must be painted offset to left to not overlap the triangle
           if t=FLastTitle then
              tmp.Title.Alignment:=TAlignment.taLeftJustify
           else
              tmp.Title.Alignment:=tmp.Field.Alignment;
         end;
      end
      else
        tmp.Title.Alignment:=tmp.Field.Alignment;

      // Picklist
      if tmp.Field is TBooleanField then
      begin
        tmp.PickList.Clear;
        tmp.PickList.Add(BoolToStr(False,True));
        tmp.PickList.Add(BoolToStr(True,True));
      end;

      // Pending:
      // PickList for Master-Detail relationships (when number of choices is < 1000 ?),
      // or an "..." ellipsi button to show a floating grid to select a Master row.
      // An alternative is to use tmp.Field.Lookup properties.

    end;
  end;

  if HasFilterGrid then
     TFilterGrid(IAssociated).Show(True);
end;
{$ENDIF}

procedure TBIDBGrid.SetColumnSort(const Value: Boolean);
begin
  if FColumnSort<>Value then
  begin
    FColumnSort:=Value;
    Repaint;
  end;
end;

procedure TBIDBGrid.SetDetail(const AData: TDataItem);

  procedure HideDetail;
  var tmp : TBIDBGrid;
  begin
    if IDetailGrid<>nil then
    begin
      IDetailGrid.Visible:=False;

      tmp:=TBIDBGrid(IDetailGrid.Plugin.GetObject);

      tmp.BIDataset.Close;
      tmp.BIDataset.Data:=nil;
      tmp.BIDataset.Master:=nil;
    end;

    if IDetailSplitter<>nil then
       IDetailSplitter.Visible:=False;
  end;

var tmpGrid : TBIDBGrid;
begin
  if AData=nil then
     HideDetail
  else
  begin
    if IDetailGrid=nil then
    begin
      IDetailGrid:=TBIGrid.Create(Self);
      IDetailGrid.Height:=Height div 2;
      IDetailGrid.Align:=TAlign.alBottom;
      IDetailGrid.Parent:=Parent;

      tmpGrid:=TBIDBGrid(IDetailGrid.Plugin.GetObject);

      tmpGrid.ChangeDataSet(TBIDataSet.Create(IDetailGrid));

      IDetailSplitter:=TSplitter.Create(Self);
      IDetailSplitter.Align:=TAlign.alBottom;
      IDetailSplitter.Top:=(Height div 2)-1;
      IDetailSplitter.Parent:=Parent;
    end
    else
      tmpGrid:=TBIDBGrid(IDetailGrid.Plugin.GetObject);

    tmpGrid.BIDataset.Close;
    tmpGrid.BIDataset.Data:=AData;
    tmpGrid.BIDataset.Master:=BIDataset;
    tmpGrid.BIDataset.Open;

    IDetailGrid.Visible:=True;
    IDetailSplitter.Visible:=True;
  end;
end;

procedure TBIDBGrid.SetParent(AParent: TWinControl);
begin
  inherited;

  if IAssociated<>nil then
  begin
    IAssociated.Parent:=Parent;
    IAssociated.Color:=Color;
  end;

  if IHighLight<>nil then
     IHighLight.Parent:=Parent;
end;

procedure TBIDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FTotals and ( (FGridState=gsColSizing) or (FGridState=gsColMoving) ) then
  begin
    inherited;
    RepaintTotals;
  end
  else
    inherited;

  IWasDragging:=False;
end;

{$IFNDEF FPC}
procedure TBIDBGrid.WMNCPaint(var Message: TMessage);
var t : Integer;
    C,R : TRect;
    tmpB : Integer;
    S : String;
    Col : TDataItem;
begin
  inherited;

  if FTotals and (DataSource.DataSet is TBIDataSet) then
  begin
    C:=ClientRect;
    tmpB:=C.Bottom;

    R:=C;
    R.Top:=tmpB;
    R.Bottom:=R.Top+DefaultRowHeight+OffsetTotals;

    Canvas.FillRect(R);

    Canvas.Pen.Color:=clBlack;
    Canvas.MoveTo(R.Left,R.Top);
    Canvas.LineTo(R.Right,R.Top);

    Canvas.Font:=Font;

    for t:=LeftCol to ColCount-1 do
    if TBIDataSetSource.FieldKind(Columns[t-1].Field.DataType).IsNumeric then
    begin
      R:=CellRect(t,RowCount-1);

      Inc(R.Left);

      R.Top:=tmpB+2;
      R.Bottom:=R.Top+DefaultRowHeight+OffsetTotals;

      Col:=TBIDataSet(DataSource.DataSet).DataOf(Columns[t-1].Field);

      if Col<>nil then
      begin
        S:=FormatFloat('0.##',Col.Stats.Sum);

        Canvas.TextOut(R.Right-2-Canvas.TextWidth(S),R.Top,S);
      end;
    end;
  end;
end;

procedure TBIDBGrid.WmVScroll(var Message: TWMVScroll);
begin
  if ScrollTrack then
     if Message.ScrollCode=SB_THUMBTRACK then
        Message.ScrollCode:=SB_THUMBPOSITION;

  inherited;
end;
{$ENDIF}

procedure TBIDBGrid.RepaintTotals;
begin
  {$IFNDEF FPC}
  SendMessage(Handle,WM_NCPaint,0,0);
  {$ENDIF}
end;

procedure TBIDBGrid.Resize;
begin
  inherited;

  if IAssociated<>nil then
     TFilterGrid(IAssociated).Reset;

  if IHighLight<>nil then
     if IndicatorOffset=1 then
        IHighLight.Reposition(IndicatorWidth)
     else
        IHighLight.Reposition(0);
end;

function TBIDBGrid.GetClientRect: TRect;
begin
  result:=inherited;

  if FTotals then
     result.Bottom:=result.Bottom-(DefaultRowHeight+OffsetTotals);
end;

function TBIDBGrid.ColumnOf(const X:Integer):TColumn;
begin
  if (X>=FixedCols) and (X<=Columns.Count) then
     result:=Columns[{$IFDEF FPC}X-1{$ELSE}RawToDataColumn(X){$ENDIF}]
  else
     result:=nil;
end;

function TBIDBGrid.ColumnOf(const AField:TField): TColumn;
var t : Integer;
begin
  for t:=0 to Columns.Count-1 do
      if Columns[t].Field=AField then
         Exit(Columns[t]);

  result:=nil;
end;

{$IFDEF FPC}
procedure TBIDBGrid.ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer);
begin
  if IsColumn then
  begin
    IWasDragging:=True;
    inherited;
    TFilterGrid(IAssociated).ColumnMoved(FromIndex,ToIndex,IndicatorOffset);
  end;
end;

{$ELSE}
procedure TBIDBGrid.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  IWasDragging:=True;
  inherited;
  TFilterGrid(IAssociated).ColumnMoved(FromIndex,ToIndex,IndicatorOffset);
end;
{$ENDIF}

function TBIDBGrid.ColumnOf(const AData: TDataItem): TColumn;
var tmp : TField;
begin
  if (DataSource<>nil) and (DataSource.DataSet<>nil) then
     tmp:=TBIDataSetSource.FieldOf(AData,DataSource.DataSet)
  else
     tmp:=nil;

  if tmp=nil then
     result:=nil
  else
     result:=ColumnOf(tmp);
end;

procedure TBIDBGrid.ColWidthsChanged;
begin
  IWasDragging:=True;

  inherited;

  if IGroup.GridOf<>nil then
     Width:=TotalWidth;

  if IAssociated<>nil then
     TFilterGrid(IAssociated).ColWidthsChanged;

  IWasDragging:=False;
end;

procedure TBIDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);

  procedure TryChangeMouseCursor;
  var p : TGridcoord;
      Col : TColumn;
      tmp : TCursor;
  begin
    // LCL needs check against crHSplit
    if (dgTitles in Options) and (Cursor<>crHSplit) then
    begin
      tmp:=crDefault;

      p:=MouseCoord(X,Y);

      Col:=ColumnOf(p.X);

      if (Col<>nil) and (p.Y<=ParentsOf(Col.Field)) then
         tmp:=crHandPoint;

      Cursor:=tmp;
    end;
  end;

begin
  inherited;

  if ColumnSort then
     TryChangeMouseCursor;
end;

procedure TBIDBGrid.Traverse(const AColumn:TColumn; const AProc:TProc<TColumn>);

  procedure DoProc(const AColumn:TColumn);
  {$IFNDEF FPC}
  var t : Integer;
  {$ENDIF}
  begin
    AProc(AColumn);

    {$IFNDEF FPC}
    if AColumn.Field is TObjectField then
       for t:=0 to TObjectField(AColumn.Field).FieldCount-1 do
           DoProc(ColumnOf(TObjectField(AColumn.Field).Fields[t]));
    {$ENDIF}
  end;

begin
  if AColumn<>nil then
     DoProc(AColumn);
end;

procedure TBIDBGrid.DoTitleClick(AColumn:TColumn);

  procedure ChangeWidth(const ColIndex,AWidth:Integer);

  {$IFDEF FPC}
  var FUpdateFields : Boolean;
  {$ENDIF}

  var Old : Boolean;
  begin
    Old:={$IFDEF FPC}False{$ELSE}FUpdateFields{$ENDIF};
    FUpdateFields:=False;
    try
      Columns.BeginUpdate;
      Columns[ColIndex].Width:=AWidth;
      Columns.EndUpdate;
    finally
      FUpdateFields:=Old;
    end;
  end;

  procedure TryInvertOrder;
  var tmpLeft : Integer;
      tmpDef : Integer;
  begin
    if FLastTitle<>-1 then
    begin
      Columns[FLastTitle].Title.Font.Style:=FLastFontStyle;
      ChangeWidth(FLastTitle,FLastWidth);
    end;

    FLastTitle:=AColumn.Index;
    FLastFontStyle:=Columns[FLastTitle].Title.Font.Style;

    tmpLeft:=LeftCol;

    if DataSource.DataSet is TBIDataSet then
       if DataSource.DataSet.Active then
          TBIDataSet(DataSource.DataSet).InvertSortBy(AColumn.Field);

    Columns[FLastTitle].Title.Font.Style:=[fsBold];

    LeftCol:=tmpLeft;

    FLastWidth:=Columns[FLastTitle].Width;
    tmpDef:=Columns[FLastTitle].DefaultWidth;

    ChangeWidth(FLastTitle,Max(FLastWidth,tmpDef+2*CalcSortWidth));

    Columns[FLastTitle].Title.Alignment:=taLeftJustify;
  end;

var tmp : TField;
    {$IFNDEF FPC}
    Y : Integer;
    P : TPoint;
    {$ENDIF}
begin
  if not IWasDragging then
  begin
    tmp:=AColumn.Field;

    if tmp<>nil then
    begin
      // Check the mouse Y position on click, to determine if the column should
      // be expanded / collapsed, or sorted.

      {$IFNDEF FPC}
      if tmp.ParentField<>nil then
      begin
        P:=ScreenToClient(Mouse.CursorPos);
        Y:=MouseCoord(P.X,P.Y).Y;

        if Y<ParentsOf(tmp) then
        begin
          tmp:=tmp.ParentField;
          AColumn:=AColumn.ParentColumn;
        end;
      end;

      if tmp is TADTField then
      begin
        AColumn.Expanded:=not AColumn.Expanded;

        if not AColumn.Expanded then
           AColumn.Width:=TitleWidth(AColumn);
      end
      else
      {$ENDIF}
      if ColumnSort then
         TryInvertOrder;
    end;
  end;
end;

{$IFDEF FPC}
procedure TBIDBGrid.HeaderClick(IsColumn: Boolean; index: Integer);
begin
  inherited;

  if IsColumn then
  begin
    if dgIndicator in Options then
       Dec(Index);

    DoTitleClick(Columns[Index]);
  end;
end;
{$ELSE}
procedure TBIDBGrid.TitleClick(Column: TColumn);
begin
  inherited;
  DoTitleClick(Column);
end;
{$ENDIF}

function TBIDBGrid.ParentsOf(AField:TField):Integer;
begin
  result:=0;

  {$IFNDEF FPC}
  if AField<>nil then
  while AField.ParentField<>nil do
  begin
    Inc(result);
    AField:=AField.ParentField;
  end;
  {$ENDIF}
end;

type
  TSearchValue=record
  public
    Data : TDataItem;
    Text : String;
  end;

  TSearchValueArray=Array of TSearchValue;

  TSearchValues=record
  public
    Items : TSearchValueArray;

    function AllEmpty:Boolean;
    function Filled:TSearchValueArray;
  end;

// Returns True is all Values Text are empty
function TSearchValues.AllEmpty:Boolean;
var t : Integer;
begin
  result:=True;

  for t:=Low(Items) to High(Items) do
      if Items[t].Text<>'' then
         Exit(False);
end;

// Return Values that have Text only
function TSearchValues.Filled:TSearchValueArray;
var tmp : String;
    L,t : Integer;
begin
  result:=nil;

  L:=0;

  for t:=0 to High(Items) do
  begin
    tmp:=Items[t].Text;

    if tmp<>'' then
    begin
      SetLength(result,L+1);

      result[L].Text:=tmp;
      result[L].Data:=Items[t].Data;

      Inc(L);
    end;
  end;
end;

procedure TBIDBGrid.CheckDataSource;
begin
  if (DataSource=nil) or (DataSource.Owner<>Self) then
     DataSource:=TDataSource.Create(Self);
end;

procedure TBIDBGrid.ResetFeatures;
begin
  FLastTitle:=-1;

  if IGroup.Grid<>nil then
  begin
    IGroup.Grid.BindTo(nil);
    IGroup.Grid.Hide;
  end;

  IGroup.Index:=nil;

  if IHighLight<>nil then
     IHighLight.Clear;

  if IDetailGrid<>nil then
     Detail:=nil;

  DestroyMenu;

  if IAssociated<>nil then
     TFilterGrid(IAssociated).Clear;
end;

procedure TBIDBGrid.ChangeDataSet(const ADataSet: TDataSet);
begin
  CheckDataSource;

  ResetFeatures;

  DataSource.DataSet:=ADataSet;
end;

procedure TBIDBGrid.ChangedFilter(const ACol:Integer; const Value:String);
begin
  ApplyFilter(True,ACol,Value);
end;

type
  TBIDatasetAccess=class(TBIDataset);

procedure TBIDBGrid.ApplyFilter(const AllowAllRows:Boolean; const ACol:Integer; const Value:String);

  procedure Fill(var Values:TSearchValues; const ADataset:TBIDataset);
  var t,
      tmpOff,
      L : Integer;
      tmpCol : TColumn;
  begin
    L:=Length(TFilterGrid(IAssociated).Items);
    SetLength(Values.Items,L);

    if dgIndicator in Options then
       tmpOff:=1
    else
       tmpOff:=0;

    for t:=0 to L-1 do
    begin
      tmpCol:=ColumnOf(t+tmpOff);

      if tmpCol=nil then
         Values.Items[t].Data:=nil
      else
         Values.Items[t].Data:=ADataset.DataOf(tmpCol.Field);

      Values.Items[t].Text:=UpperCase(TFilterGrid(IAssociated).GetText(t));
    end;
  end;

  function Search(const Values:TSearchValueArray; const AData:TBIDataset): TCursorIndex;
  var L : Integer;
      tmpRowItem : TDataItem;

    function Match(const ARow:TInteger):Boolean;
    var t : Integer;
        tmpData : TDataItem;
    begin
      result:=True;

      for t:=0 to L do
      begin
        tmpData:=Values[t].Data;

        if tmpData=tmpRowItem then
        begin
          if Pos(Values[t].Text,IntToStr(ARow+1))=0 then
          begin
            result:=False;
            break;
          end;
        end
        else
        if tmpData.Kind=TDataKind.dkUnknown then
        begin
          if Pos(Values[t].Text,UpperCase(tmpData.Items[ARow].Name))=0 then
          begin
            result:=False;
            break;
          end;
        end
        else
        if tmpData.Missing[ARow] or (Pos(Values[t].Text,UpperCase(tmpData.DataToString(ARow)))=0) then
        begin
          result:=False;
          break;
        end;
      end;
    end;

  var t : TLoopInteger;
  begin
    result:=nil;

    tmpRowItem:=TBIDatasetAccess(AData).GetRowItem;

    L:=High(Values);

    if IGroup.Index=nil then
    begin
      if (AData.Data.Kind=TDataKind.dkUnknown) and (not AData.Data.AsTable) then
      begin
        for t:=0 to AData.Data.Items.Count-1 do
            if Match(t) then
               result.Append(t);
      end
      else
      begin
        for t:=0 to AData.Data.Count-1 do
            if Match(t) then
               result.Append(t);
      end;
    end
    else
      for t:=0 to IGroup.Index.Count-1 do
          if Match(IGroup.Index[t]) then
             result.Append(IGroup.Index[t]);
  end;

var tmpData : TBIDataset;
    tmpIndex : TCursorIndex;

    AllRows : Boolean;
    Values : TSearchValues;

    Old : TPanel;
    OldLeft : Integer;
begin
  tmpData:=BIDataSet;

  if (tmpData<>nil) and tmpData.Active then
  begin
    if Value<>'' then
       CheckFilterGrid;

    if HasFilterGrid then
    begin
      Fill(Values,tmpData);

      if ACol<>-1 then
         Values.Items[ACol].Text:=UpperCase(Value);
    end;

    AllRows:=Values.AllEmpty;

    if AllRows then
    begin
      tmpIndex:=IGroup.Index;
      AllRows:=AllowAllRows;
    end
    else
      tmpIndex:=Search(Values.Filled,tmpData);

    OldLeft:=LeftCol;

    Old:=IAssociated;
    IAssociated:=nil;

    tmpData.PrepareIndex(tmpIndex,AllRows);

    IAssociated:=Old;

    if IAssociated<>nil then
       TFilterGrid(IAssociated).SetLeftCol(OldLeft);

    LeftCol:=OldLeft;
  end;
end;

function TBIDBGrid.TotalWidth:Integer;
var t,
    tmp : Integer;
begin
  result:=0;

  if Columns.Count>0 then
  begin
    tmp:=0;

    for t:=0 to Columns.Count-1 do
        Inc(tmp,Columns[t].Width);

    if dgIndicator in Options then
       Inc(tmp,IndicatorWidth);

    if dgColLines in Options then
       Inc(tmp,3);

    Inc(tmp,GetSystemMetrics(SM_CXVSCROLL)+3);

    result:=tmp;
  end;
end;

{ TFilterGrid }

Constructor TFilterGrid.Create(AOwner: TComponent);
begin
  inherited;

  Align:=TAlign.alTop;
  Height:=24;
  BevelOuter:=TBevelCut.bvNone;
  BorderStyle:=bsNone;
  Caption:='';
end;

procedure TFilterGrid.Clear;
var t : Integer;
begin
  for t:=0 to High(Items) do
      Items[t].Free;

  Items:=nil;
end;

procedure TFilterGrid.ClearCombos;
var t : Integer;
begin
  for t:=0 to High(Items) do
      if Items[t] is TComboBox then
         TComboBox(Items[t]).Items.Clear;
end;

procedure TFilterGrid.ColumnMoved(FromIndex, ToIndex, AOffset: Integer);
var tmp : TControl;
    t,
    tmpMax : Integer;
begin
  if Items<>nil then
  begin
    tmp:=Items[FromIndex-AOffset];

    if FromIndex>ToIndex then
       for t:=FromIndex-AOffset downto ToIndex+1-AOffset do
           Items[t]:=Items[t-1]
    else
    begin
      tmpMax:=ToIndex+1-AOffset;

      if tmpMax>High(Items)-1 then
         tmpMax:=High(Items)-1;

      for t:=FromIndex-AOffset to tmpMax do
          Items[t]:=Items[t+1];
    end;

    Items[ToIndex-AOffset]:=tmp;
  end;
end;

procedure TFilterGrid.ColWidthsChanged;
begin
  Reset;
end;

procedure TFilterGrid.DroppedCombo(Sender: TObject);

  procedure AddNames(const ANames:TDataItems; const AItems:TStrings);
  var t : Integer;
  begin
    AItems.BeginUpdate;
    try
      for t:=0 to ANames.Count-1 do
          AItems.Add(ANames[t].Name);
    finally
      AItems.EndUpdate;
    end;
  end;

  procedure AddMapTo(const AMap:TDataMap; const AItems:TStrings);
  var t : Integer;
  begin
    AItems.BeginUpdate;
    try
      for t:=0 to AMap.Count-1 do
          AItems.Add(AMap.AsString(t)); // Pending: BIDataSet.FloatFormat for floats
    finally
      AItems.EndUpdate;
    end;
  end;

var tmp : TComboBox;
    tmpCol : TColumn;
    tmpBI : TBIDataset;
    tmpData : TDataItem;
    tmpMap : TDataMap;
    t : TLoopInteger;
begin
  tmpBI:=IGrid.BIDataset;

  if tmpBI<>nil then
  begin
    tmp:=(Sender as TComboBox);

    if tmp.Items.Count=0 then
    begin
      tmpCol:=TColumn(tmp.Tag);

      if tmpCol<>nil then
      begin
        tmpData:=tmpBI.DataOf(tmpCol.Field);

        if tmpData<>nil then
        begin
          if IGrid.IGroup.Index=nil then
          begin
            if tmpData.Kind=TDataKind.dkUnknown then
               AddNames(tmpData.Items,tmp.Items)
            else
            begin
              tmpData.Stats;
              tmpMap:=tmpData.DataMap;

              if tmpMap<>nil then
                 AddMapTo(tmpMap,tmp.Items);
            end;
          end
          else
          begin
            tmpMap:=TTextMap.Create;
            try
              for t:=0 to High(IGrid.IGroup.Index) do
                  // Pending: BIDataSet.FloatFormat for floats
                  TTextMap(tmpMap).AddMap(tmpData.DataToString(IGrid.IGroup.Index[t]));

              AddMapTo(tmpMap,tmp.Items);
            finally
              tmpMap.Free;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFilterGrid.Reset;
var t : Integer;
    tmpR : TRect;
begin
  ICloseButton.Left:=IGrid.Left;

  if Items<>nil then
  begin
    for t:=0 to High(Items) do
    begin
      if Items[t]<>nil then
      begin
        tmpR:=IGrid.CellRect(t+IGrid.IndicatorOffset,0);

        Items[t].Left:=IGrid.Left+tmpR.Left;
        Items[t].Width:=tmpR.Right-tmpR.Left;
      end;
    end;
  end;
end;

procedure TFilterGrid.Show(const AVisible:Boolean);

  function CreateComboBox(const ATag:NativeInt):TComboBox;
  begin
    result:=TComboBox.Create(Self{IGrid});
    result.DropDownCount:=25;
    result.Parent:=Self;

    result.Tag:=ATag;
    result.OnDropDown:=DroppedCombo;
    result.OnChange:=EditChange;

    //if tmpData.Kind.IsNumeric then
    // DropDownList Alignment=TA_RIGHT ??
  end;

  function CreateEdit(const ATag:NativeInt):TEdit;
  begin
    result:=TEdit.Create(Self{IGrid});
    // result.BorderStyle:=bsNone;
    result.Parent:=Self;

    result.Tag:=ATag;
    result.OnChange:=EditChange;
  end;

  function CreateItem(const AData:TDataItem; const ATag:NativeInt):TControl;
  begin
    if AData=nil then //) or (tmpData.Kind=TDataKind.dkUnknown) then
       result:=nil // No possible filtering combo or edit
    else
    begin
      AData.Stats;

      if AData.Unique then
         result:=CreateEdit(ATag)
      else
         result:=CreateComboBox(ATag);

      result.Parent:=Self;
    end;
  end;

var
  tmpBI : TBIDataset;

  // Checks all columns Visible property to create or destroy Items.
  // Returns True when any Item is destroyed because its Column is no longer Visible
  function CheckColumnVisibility:Boolean;
  var t : Integer;
      tmpCol : TColumn;
  begin
    result:=False;

    for t:=0 to IGrid.Columns.Count-1 do
    begin
      tmpCol:=IGrid.Columns[t];

      if tmpCol.Visible then
      begin
        if Items[t]=nil then
           Items[t]:=CreateItem(tmpBI.DataOf(tmpCol.Field),NativeInt(tmpCol)); // Column is now Visible
      end
      else
      if Items[t]<>nil then
      begin
        // Column is no longer Visible, destroy Item
        Items[t].Free;
        Items[t]:=nil;
        result:=True;
      end;
    end;
  end;

  procedure CreateAllItems;
  var t : Integer;
      tmpCol : TColumn;
  begin
    SetLength(Items,IGrid.Columns.Count);

    for t:=0 to IGrid.Columns.Count-1 do
    begin
      tmpCol:=IGrid.Columns[t];

      if tmpCol.Visible then
         Items[t]:=CreateItem(tmpBI.DataOf(tmpCol.Field),NativeInt(tmpCol))
      else
         Items[t]:=nil;
    end;
  end;

begin
  if AVisible then
  begin
    tmpBI:=IGrid.BIDataset;

    if (tmpBI<>nil) and (tmpBI.Data<>nil) then
    begin
      tmpBI.Data.Stats;

      if Items=nil then
         CreateAllItems
      else
      if CheckColumnVisibility then
         IGrid.ApplyFilter(True,-1,'');  // Removed one or more Columns

      Reset;
    end;
  end;

  Visible:=AVisible;

  IGrid.ApplyFilter(True,-1,'');  // Remove filter
end;

procedure TFilterGrid.EditChange(Sender:TObject);
var t : Integer;
begin
  for t:=0 to High(Items) do
      if Items[t]=Sender then
      begin
        IGrid.ChangedFilter(t,GetText(t));
        break;
      end;
end;

function TFilterGrid.GetText(const ACol: Integer): String;
begin
  if Items[ACol] is TEdit then
     result:=TEdit(Items[ACol]).Text
  else
     result:=TComboBox(Items[ACol]).Text;
end;

procedure TFilterGrid.SetLeftCol(const ACol: Integer);
begin
  Reset;
end;

{ TBIDBGrid.TGridGroup }

procedure TBIDBGrid.TGridGroup.CreateGrid(const AGrid: TBIDBGrid);
begin
  Grid:=TBIGrid.Create(AGrid.Owner);
  Grid.Align:=TAlign.alLeft;
  Grid.ReadOnly:=True;
  Grid.Parent:=AGrid.Parent;//.Parent;

  DBGrid.IGroup.GridOf:=AGrid;
  DBGrid.IAlternate:=TAlternateColor.Create;
  DBGrid.IAlternate.OnChange:=DBGrid.AlternateChanged;

  Split:=TSplitter.Create(AGrid.Owner);
  Split.Align:=TAlign.alRight;
  Split.Parent:=Grid.Parent;
  Split.Align:=TAlign.alLeft;
end;

function TBIDBGrid.TGridGroup.DBGrid: TBIDBGrid;
begin
  result:=TBIDBGrid(Grid.Plugin.GetObject);
end;

procedure TBIDBGrid.TGridGroup.ShowGroup(const AData:TDataItem);
var tmp : TDataItem;
begin
  DBGrid.CheckDataSource;

  Grid.DataSource.OnDataChange:=DBGrid.GroupDataChange;

  if DataSet=nil then
     DataSet:=TBIDataSet.Create(Grid.Owner);

  tmp:=TDataMapAsData.FromData(AData,False,TDataMapAsData.TDataMapOrder.Item,True);

  if tmp<>nil then
  begin
    // If AData has missing values, insert extra result items at position 0,
    // one for "Missing" //Pending: and another one for "Not Missing"
    if AData.Missing.MissingCount>0 then
    begin
      tmp.Insert(0);
      tmp.Items[0].Missing[0]:=True;
    end;

    DataSet.Data.Free;

    // Call Clear *after* destroying DataSet.Data
    DataSet.Cursor.Clear;

    DataSet.Data:=tmp;

    DataSet.Tag:=NativeInt(AData);

       DataSet.Data.Items[0].Name:=AData.Name;

    DataSet.Close;
    DBGrid.ChangeDataSet(DataSet);
    DataSet.Open;

    Grid.Width:=DBGrid.TotalWidth;
  end;
end;

initialization
  TBIGrid.Engine:=TBIDBGridPlugin;
finalization
  TBIGrid.Engine:=nil;
end.
