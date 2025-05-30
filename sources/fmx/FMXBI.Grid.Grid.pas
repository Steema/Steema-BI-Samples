{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid plugin for Firemonkey Grid         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Grid.Grid;

interface

uses
  System.Classes, System.SysUtils, System.Types,
  {$IF CompilerVersion>26}
  System.Math.Vectors,
  {$ENDIF}
  FMX.Types, FMX.Grid,
  FMX.Bind.Editors, Data.Bind.Components,
  Data.Bind.DBScope, Data.Bind.Grid, FMX.Bind.Grid,
  //FMXBI.Grid,
  BI.DataSet,
  BI.DataItem, FMX.Controls, Data.DB, System.UITypes, BI.UI, FMX.Header,
  FMX.Menus,

  {$IF CompilerVersion<26}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  System.Rtti, BI.Grid.Plugin;

{$IF CompilerVersion>27}
{$DEFINE NODEFAULTDRAWING}
{$ENDIF}

type
  // Unfortunately, it is not possible to derive this class from FMX TGrid.
  // Bindings work partially, columns are created but the grid shows empty.
  TBIFMXGrid=class
  private
    type
      TPrivateGrid=class(TGrid)
      private
        FMXGrid : TBIFMXGrid;

        {$IFDEF NODEFAULTDRAWING}
        function GetAlignOf(const AColumn:TColumn):TTextAlign;

        procedure InternalDrawCell(Sender: TObject; const Canvas: TCanvas;
            const Column: TColumn; const Bounds: TRectF;
            const Row: Integer; const Value: TValue; const State: TGridDrawStates);
        {$ENDIF}
      protected
        procedure DoAddObject(const AObject: TFmxObject); override;

        {$IF CompilerVersion<31}
        {$IF CompilerVersion>26}
        procedure DoDrawColumnHeader(const Canvas: TCanvas; const Item: THeaderItem;
                         const Bounds: TRectF); override;
        {$ENDIF}
        {$ENDIF}
      public
        Constructor Create(AOwner:TComponent); override;

        property ReadOnly default True;
      end;

    var
    FBindingEditor: TBindListGridEditor;
    FColumnSort : Boolean;

    FLastTitle : Integer;

    BindingList : TBindingsList;
    LinkGrid: TLinkGridToDataSource;

    FOnRowChanged : TNotifyEvent;

    IDataSet : TBIDataSet;

    // Necessary because TBIFMXGrid does not derive from TGrid,
    // so encapsulation instead of inheritance is needed.
    IFMXGrid : TGrid;

    FColorizers : TDataColorizers;

    procedure AutoSizeWidth(const AColumn:TColumn; const ACanvas:TCanvas);
    function CalcSortWidth:Single;

    function FieldOf(const AColumn:TColumn):TField;

    {$IF CompilerVersion>25}
    procedure ClickedHeader(Column:TColumn);

    {$IF CompilerVersion>=31}
    procedure DrawColumnHeader(Sender: TObject; const Canvas: TCanvas;
                               const Column: TColumn;
                               const Bounds: TRectF);
    {$ENDIF}

    {$ENDIF}

    procedure CreateBindings;
    procedure DataChange(Sender: TObject; Field: TField);

    procedure DrawSortIndicator(const ACanvas:TCanvas; const ARect:TRectF; const Desc:Boolean);

    procedure RecalcColumnWidth(const AColumn:TColumn);
    procedure SetColumnSort(const Value: Boolean);
    procedure SetHeaderStyle(const AStyle:TFontStyles);
    function Sorted(const AName:String; out ADescending:Boolean):Boolean;
    function TryColorize(const ARow:Integer; const AColumn:TColumn; out AIndex:Integer; out APercent:Double):Boolean;
    function TryFontColor(const AIndex:Integer; const ANewColor,ADefault:TAlphaColor):TAlphaColor;
  protected
    BindSource : TBindSourceDB;

    function DataOf(const AColumn:TColumn; out ADataSet:TBIDataSet):TDataItem;
    function TotalWidth:Single;
  public
    Constructor Create(AOwner:TComponent);
    Destructor Destroy; override;

    property Grid:TGrid read IFMXGrid;

  //published
    property ColumnSort:Boolean read FColumnSort write SetColumnSort default True;
    property OnRowChanged:TNotifyEvent read FOnRowChanged write FOnRowChanged;
  end;

  TBIFMXGridPlugin=class(TBIGridPlugin)
  private
    IGrid : TBIFMXGrid;
  protected
    procedure AutoWidth; override;
    procedure ChangedAlternate(Sender:TObject); override;
    function GetCanSort: Boolean; override;
    function GetDataSource: TDataSource; override;
    function GetReadOnly:Boolean; override;
    function GetSortEnabled: Boolean; override;
    function GetTotals:Boolean; override;
    procedure SetDataSource(const Value: TDataSource); override;
    procedure SetReadOnly(const Value:Boolean); override;
    procedure SetOnRowChanged(const AEvent:TNotifyEvent); override;
    procedure SetPopup(const Value:TObject); override;
    procedure SetSortEnabled(const Value: Boolean); override;
    procedure SetTotals(const Value:Boolean); override;
  public
    Constructor Create(const AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure BindTo(const ADataSet:TDataSet); override;
    procedure Colorize(const AItems:TDataColorizers); override;
    procedure Duplicates(const AData:TDataItem; const Hide:Boolean); override;
    function GetControl:TObject; override;
    function GetObject:TObject; override;
  end;

implementation

uses
  {$IF CompilerVersion>25}
//  FMX.Graphics,
  {$ENDIF}
  {$IF CompilerVersion<=27}
  FMX.Platform,
  {$ENDIF}

  FMX.Bind.DBLinks,

  {$IF CompilerVersion>=31}
  FMX.Presentation.Factory, FMX.Presentation.Style, FMX.Grid.Style,

  FMX.TextLayout, FMX.Consts,
  {$ENDIF}

  FMXBI.Grid, // <-- needed only to access TUICommon.Alignxxx

  FMX.Edit, BI.DB.DataSet, BI.UI.Colors;

{ TBIFMXGrid }

Constructor TBIFMXGrid.Create(AOwner: TComponent);
begin
  inherited Create;

  FLastTitle:=-1;

  IFMXGrid:=TPrivateGrid.Create(AOwner);

  TPrivateGrid(IFMXGrid).FMXGrid:=Self;

  if AOwner is TControl then
     IFMXGrid.Parent:=TControl(AOwner);

  CreateBindings;

  FColumnSort:=True;

  {$IF CompilerVersion>26}
  IFMXGrid.OnHeaderClick:=ClickedHeader;

  {$IF CompilerVersion>=31}
  IFMXGrid.OnDrawColumnHeader:=DrawColumnHeader;
  {$ENDIF}

  {$ENDIF}
end;

Destructor TBIFMXGrid.Destroy;
begin
  LinkGrid.Free;
  BindingList.Free;
  BindSource.Free;

  IFMXGrid.Free;

  inherited;
end;

{$IF CompilerVersion>=31}
procedure TBIFMXGrid.DrawColumnHeader(Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
    const Bounds: TRectF);
var tmpDesc : Boolean;
begin
  if Sorted(Column.Header,tmpDesc) then
     DrawSortIndicator(Canvas,Bounds,tmpDesc);
end;
{$ELSE}

{$IF CompilerVersion>26}
procedure TBIFMXGrid.TPrivateGrid.DoDrawColumnHeader(const Canvas: TCanvas;
  const Item: THeaderItem; const Bounds: TRectF);
var tmpDesc : Boolean;
begin
  inherited;

  if FMXGrid.Sorted(Item.Text,tmpDesc) then
     FMXGrid.DrawSortIndicator(Canvas,Bounds,tmpDesc);
end;
{$ENDIF}

{$ENDIF}

procedure TBIFMXGrid.CreateBindings;
begin
  FBindingEditor:=TBindListGridEditor.Create(IFMXGrid);

  BindSource:=TBindSourceDB.Create(IFMXGrid);
  BindingList:=TBindingsList.Create(IFMXGrid);

  {$WARNINGS OFF}
  LinkGrid:=TLinkGridToDataSource.Create(BindingList);
  {$WARNINGS ON}

  LinkGrid.DataSource:=BindSource;
  LinkGrid.GridControl:=IFMXGrid;

  BindSource.DataSource.OnDataChange:=DataChange;
end;

procedure TBIFMXGrid.DataChange(Sender: TObject; Field: TField);
begin
  if Assigned(FOnRowChanged) then
     FOnRowChanged(Self);
end;

procedure TBIFMXGrid.SetColumnSort(const Value: Boolean);
begin
  if FColumnSort<>Value then
  begin
    FColumnSort:=Value;
    IFMXGrid.Repaint;
  end;
end;

procedure TBIFMXGrid.SetHeaderStyle(const AStyle:TFontStyles);
const
  StyleSetting=TStyledSetting.{$IF CompilerVersion>26}Style{$ELSE}ssStyle{$ENDIF};

var IHeader : THeader;
    tmpItem : THeaderItem;
begin
  if FLastTitle<>-1 then
  begin
    {$IF CompilerVersion>29}
    if IFMXGrid.FindStyleResource<THeader>('header', IHeader) then
    {$ELSE}
    IHeader:=IFMXGrid.FindStyleResource('header') as THeader;

    if IHeader<>nil then
    {$ENDIF}
    begin
      if IHeader<>nil then
      begin
        tmpItem:=THeaderItem(IHeader.Children[FLastTitle]);

        if tmpItem<>nil then
        begin
          if AStyle=[] then
             tmpItem.StyledSettings:=tmpItem.StyledSettings+[StyleSetting]
          else
             tmpItem.StyledSettings:=tmpItem.StyledSettings-[StyleSetting];

          tmpItem.Font.Style:=AStyle;
        end;
      end;
    end;
  end;
end;

function TBIFMXGrid.Sorted(const AName: String;
  out ADescending: Boolean): Boolean;
var tmp : TSortItem;
begin
  result:=False;

  if IDataSet<>nil then
     for tmp in IDataSet.Cursor.SortBy.Items do
         if SameText(tmp.Data.Name,AName) then
         begin
           ADescending:=tmp.Descending;
           result:=True;

           break;
         end;
end;

type
  TDataAccess=class(TDataItem);

procedure TBIFMXGrid.AutoSizeWidth(const AColumn:TColumn; const ACanvas:TCanvas);
var tmp : TDataItem;
    tmpDataset : TBIDataset;
    tmpLength : Single;
begin
  tmp:=DataOf(AColumn,tmpDataset);

  if tmp<>nil then
  begin
    if tmp.Kind=TDataKind.dkText then
    begin
      // Average character width (aproximation)
      tmpLength:=TDataAccess(tmp).MaxTextLength*ACanvas.TextWidth('AaBbCcDdEeFf .')*(1/14);

      if tmpLength>AColumn.Width then
         AColumn.Width:=tmpLength;
    end;
  end;
end;

procedure TBIFMXGrid.RecalcColumnWidth(const AColumn:TColumn);
var tmpCanvas : TCanvas;
    tmpDesc : Boolean;
begin
  tmpCanvas:=AColumn.Canvas;

  if tmpCanvas=nil then
     tmpCanvas:=TCanvasManager.MeasureCanvas;

  {$IF CompilerVersion>26}
  tmpCanvas.Font.Assign(Grid.TextSettings.Font);
  {$ENDIF}

  if tmpCanvas<>nil then
     AColumn.Width:=tmpCanvas.TextWidth(AColumn.Header+'W');

  if Sorted(AColumn.Header,tmpDesc) then
     AColumn.Width:=AColumn.Width+CalcSortWidth;

  AutoSizeWidth(AColumn,tmpCanvas);
end;

function TBIFMXGrid.TotalWidth: Single;
var t : Integer;
begin
  result:=0;

  for t:=0 to IFMXGrid.ColumnCount-1 do
      result:=result+IFMXGrid.Columns[t].Width;
end;

type
  TBindSourceDBAccess=class(TBindSourceDB);

// Returns the TField associated with AColumn.
// Problem: "Header" might not be the field FieldName !
function TBIFMXGrid.FieldOf(const AColumn:TColumn):TField;
var tmp : TObject;
begin
  if BindSource=nil then
     result:=nil
  else
  begin
    tmp:=TBindSourceDBAccess(BindSource).GetMember(AColumn.Header);

    if tmp is TField then
       result:=TField(tmp)
    else
       result:=nil;
  end;
end;

{$IF CompilerVersion>25}
procedure TBIFMXGrid.ClickedHeader(Column:TColumn);
var tmp : Integer;
begin
  if ColumnSort and (Column<>nil) then
  begin
    IFMXGrid.BeginUpdate;

    SetHeaderStyle([]);

    tmp:=Column.Index;

    if IDataSet<>nil then
       IDataSet.InvertSortBy(FieldOf(Column));

    FLastTitle:=tmp;
    SetHeaderStyle([TFontStyle.fsBold]);

    RecalcColumnWidth(Column);

    //IFMXGrid.Repaint;
    IFMXGrid.EndUpdate;
  end;
end;
{$ENDIF}

function TBIFMXGrid.DataOf(const AColumn:TColumn; out ADataSet:TBIDataSet):TDataItem;
var tmpField : TField;
begin
  result:=nil;

  if AColumn<>nil then
  begin
    tmpField:=FieldOf(AColumn);

    if tmpField<>nil then
    begin
      if tmpField.DataSet is TBIDataset then
      begin
        ADataset:=TBIDataSet(tmpField.DataSet);
        result:=ADataset.DataOf(tmpField);
      end;
    end;
  end;
end;

function TBIFMXGrid.TryColorize(const ARow:Integer; const AColumn:TColumn; out AIndex:Integer; out APercent:Double):Boolean;
var tmpData : TBIDataSet;
    tmpItem : TDataItem;
    tmpRecNo : Integer;
begin
  result:=False;

  tmpItem:=DataOf(AColumn,tmpData);

  if (tmpItem<>nil) and (tmpItem.Kind<>TDataKind.dkText) then
  begin
    tmpRecNo:=tmpData.Cursor.Position(tmpData.RecNo-1);
    result:=FColorizers.TryColorize(tmpItem,tmpRecNo,APercent,AIndex);
  end;
end;

function SwapRGB(const AColor: TAlphaColor): Integer;
begin
  TColorRec(result).A:=0;
  TColorRec(result).R:=TAlphaColorRec(AColor).B;
  TColorRec(result).G:=TAlphaColorRec(AColor).G;
  TColorRec(result).B:=TAlphaColorRec(AColor).R;
end;

function TBIFMXGrid.TryFontColor(const AIndex:Integer; const ANewColor,ADefault:TAlphaColor):TAlphaColor;
begin
  if FColorizers[AIndex].TextColor=TColorizeTextColor.Automatic then
     if SwapRGB(ANewColor)>$7FFFFF then
        result:=TAlphaColors.Black
     else
        result:=TAlphaColors.White
  else
     result:=ADefault;
end;

{ TBIFMXGridPlugin }

Constructor TBIFMXGridPlugin.Create(const AOwner: TComponent);
begin
  inherited;
  IGrid:=TBIFMXGrid.Create(AOwner); // <-- "nil" to avoid embedding it as a children
end;

Destructor TBIFMXGridPlugin.Destroy;
begin
  IGrid.Free;
  inherited;
end;

procedure TBIFMXGridPlugin.ChangedAlternate(Sender: TObject);
begin
  {$IF CompilerVersion>26} // XE6 and up
  if IAlternate.Enabled then
     IGrid.Grid.Options:=IGrid.Grid.Options+[TGridOption.AlternatingRowBackground]
  else
     IGrid.Grid.Options:=IGrid.Grid.Options-[TGridOption.AlternatingRowBackground];

  IGrid.Grid.Repaint;
  {$ENDIF}
end;

procedure TBIFMXGridPlugin.Colorize(const AItems: TDataColorizers);
begin
  IGrid.FColorizers:=AItems;
  IGrid.Grid.Repaint;;
end;

procedure TBIFMXGridPlugin.AutoWidth;
begin
  inherited;
  IGrid.Grid.{ParentControl.}Width:=IGrid.TotalWidth+4;
end;

procedure TBIFMXGridPlugin.BindTo(const ADataSet: TDataSet);

  {
  // Forcing boolean columns to show as checkboxes:
  procedure AddColumns;
  var Col : TDataItem;
      tmp : TLinkGridToDataSourceColumn;
  begin
    LinkGrid.Columns.Clear;

    for Col in AData.Items.AsArray do
    begin
      tmp:=LinkGrid.Columns.Add;
      tmp.MemberName:=Col.Name;

      case Col.Kind of
        dkBoolean: tmp.ColumnStyle:='CheckColumn';
      end;
    end;
  end;
  }

begin
  IGrid.FLastTitle:=-1;
  IGrid.SetHeaderStyle([]);

  IGrid.BindSource.DataSet:=ADataSet;

  if ADataSet is TBIDataset then
     IGrid.IDataSet:=TBIDataSet(ADataSet)
  else
     IGrid.IDataSet:=nil;

  IGrid.IFMXGrid.Repaint;
end;

procedure TBIFMXGridPlugin.Duplicates(const AData: TDataItem;
  const Hide: Boolean);
var tmpD : TDataSet;
    tmp : TField;
begin
  tmpD:=IGrid.IDataSet;

  if tmpD<>nil then
  begin
    tmp:=TBIDataSetSource.FieldOf(AData,tmpD);

    if tmp<>nil then
    begin
      (tmpD as TBIDataSet).SetFieldOnGetText(tmp,Hide);
      IGrid.IFMXGrid.Repaint;
    end;
  end;
end;

function TBIFMXGridPlugin.GetObject: TObject;
begin
  result:=IGrid;
end;

function TBIFMXGridPlugin.GetReadOnly: Boolean;
begin
  result:=IGrid.Grid.ReadOnly;
end;

function TBIFMXGridPlugin.GetControl: TObject;
begin
  result:=IGrid.Grid;
end;

// Firemonkey does not provide DataSource as a property (it uses Live Bindings)
function TBIFMXGridPlugin.GetDataSource: TDataSource;
begin
  result:=IGrid.BindSource.DataSource;
end;

function TBIFMXGridPlugin.GetTotals: Boolean;
begin
  result:=False;
end;

procedure TBIFMXGridPlugin.SetDataSource(const Value: TDataSource);
begin
end;

procedure TBIFMXGridPlugin.SetOnRowChanged(const AEvent: TNotifyEvent);
begin
  IGrid.OnRowChanged:=AEvent;
end;

procedure TBIFMXGridPlugin.SetPopup(const Value: TObject);
begin
  IGrid.Grid.PopupMenu:=Value as TPopupMenu;
end;

procedure TBIFMXGridPlugin.SetReadOnly(const Value: Boolean);
begin
  IGrid.Grid.ReadOnly:=Value;
end;

procedure TBIFMXGridPlugin.SetTotals(const Value: Boolean);
begin
end;

function TBIFMXGridPlugin.GetCanSort: Boolean;
begin
  result:=True;
end;

function TBIFMXGridPlugin.GetSortEnabled: Boolean;
begin
  result:=IGrid.ColumnSort;
end;

procedure TBIFMXGridPlugin.SetSortEnabled(const Value: Boolean);
begin
  IGrid.ColumnSort:=Value;
end;

{ TBIFMXGrid.TPrivateGrid }

Constructor TBIFMXGrid.TPrivateGrid.Create(AOwner: TComponent);
begin
  inherited;

  Stored:=False;
  Align:=TUICommon.AlignClient;

  ReadOnly:=True;

  {$IF CompilerVersion>26}
  Options:=Options+[TGridOption.AlwaysShowSelection];
  {$ENDIF}

  {$IFDEF NODEFAULTDRAWING}
  DefaultDrawing:=False;
  OnDrawColumnCell:=InternalDrawCell;
  {$ENDIF}
end;

procedure TBIFMXGrid.TPrivateGrid.DoAddObject(const AObject: TFmxObject);
begin
  inherited;

  if AObject is TColumn then
     FMXGrid.RecalcColumnWidth(TColumn(AObject));
end;

function TBIFMXGrid.CalcSortWidth:Single;
begin
  result:={$IF CompilerVersion>=31}IFMXGrid.Model.EffectiveRowHeight{$ELSE}IFMXGrid.RowHeight{$ENDIF} *0.5;
end;

procedure TBIFMXGrid.DrawSortIndicator(const ACanvas:TCanvas; const ARect:TRectF; const Desc:Boolean);
const
  SortIndicatorMargin=4;

var SortWidth,
    Y0,
    Y1,
    X : Single;
    tmp : TPolygon;
begin
  SortWidth:=CalcSortWidth;

  X:=ARect.Right-SortWidth-SortIndicatorMargin;

  Y0:=ARect.Top+5;
  Y1:=Y0+SortWidth;

  ACanvas.Fill.Kind:=TBrushKind.{$IF CompilerVersion>26}Solid{$ELSE}bkSolid{$ENDIF};
  ACanvas.Fill.Color:=TAlphaColors.Black;

  SetLength(tmp,3);

  if Desc then
  begin
    tmp[0]:=PointF(X,Y1);
    tmp[1]:=PointF(X+(SortWidth*0.5),Y0-1);
    tmp[2]:=PointF(X+SortWidth,Y1);
  end
  else
  begin
    tmp[0]:=PointF(X,Y0);
    tmp[1]:=PointF(X+(SortWidth*0.5),Y1);
    tmp[2]:=PointF(X+SortWidth,Y0);
  end;

  ACanvas.FillPolygon(tmp,1);
end;

{$IFDEF NODEFAULTDRAWING}

function TBIFMXGrid.TPrivateGrid.GetAlignOf(const AColumn:TColumn):TTextAlign;
var tmp : TField;
begin
  tmp:=FMXGrid.FieldOf(AColumn);

  if (tmp<>nil) and (tmp.Alignment=TAlignment.taRightJustify) then
     result:=TTextAlign.{$IF CompilerVersion>25}Trailing{$ELSE}taTrailing{$ENDIF}
  else
     result:=TTextAlign.{$IF CompilerVersion>25}Leading{$ELSE}taLeading{$ENDIF};
end;

{$IF CompilerVersion>=31}
type
  TColumnAccess=class(TColumn);
{$ENDIF}

procedure TBIFMXGrid.TPrivateGrid.InternalDrawCell(Sender: TObject; const Canvas: TCanvas;
    const Column: TColumn; const Bounds: TRectF;
    const Row: Integer; const Value: TValue; const State: TGridDrawStates);

  procedure DoColorize(const AColor:TAlphaColor);
  var Old : TAlphaColor;
      tmp : TRectF;
  begin
    Old:=Canvas.Fill.Color;
    Canvas.Fill.Color:=AColor;

    tmp:=Bounds;

    if TGridOption.ColLines in Options then
       tmp.Left:=tmp.Left+1;

    if TGridOption.RowLines in Options then
       tmp.Bottom:=tmp.Bottom-1;

    Canvas.FillRect(tmp,0,0,AllCorners,1,Canvas.Fill);
    Canvas.Fill.Color:=Old;
  end;

var
  NewColor : TAlphaColor;
  tmpColor : Boolean;
  tmpIndex : Integer;

  {$IF CompilerVersion>=31}

  // Hack: Copied from TColumn.DefaultDrawCell
  procedure DoDrawCell;
  var tmp : TTextLayout;
  begin
    tmp:=TColumnAccess(Column).Layout;

    tmp.BeginUpdate;
    try
      tmp.TopLeft:=Bounds.TopLeft;
      tmp.MaxSize:=PointF(Bounds.Width, Bounds.Height);
      try
        tmp.Text:=Column.ValueToString(Value);

        if tmpColor then
           tmp.Color:=FMXGrid.TryFontColor(tmpIndex,NewColor,tmp.Color);

      except
        tmp.Text:=SMsgDlgError;
      end;

      tmp.HorizontalAlign:=GetAlignOf(Column);
    finally
      tmp.EndUpdate;
    end;

    tmp.RenderLayout(Canvas);
  end;
  {$ENDIF}

{$IF CompilerVersion<31}
var tmpR : TRectF;
{$ENDIF}
var
  tmpPercent : Double;
begin
  NewColor:=TAlphaColors.Null;

  if State=[] then
  begin
    tmpColor:=FMXGrid.TryColorize(Row,Column,tmpIndex,tmpPercent);

    if tmpColor then
    begin
      NewColor:=FMXGrid.FColorizers[tmpIndex].AlphaColorOf(tmpPercent);
      DoColorize(NewColor);
    end;
  end
  else
    tmpColor:=False;

  {$IF CompilerVersion<31}
  tmpR:=Bounds;

  TextSettings.HorzAlign:=GetAlignOf(Column);

  if TextSettings.HorzAlign=TTextAlign.Trailing then
     tmpR.Offset(-2*2 { TColumn.HorizTextMargin },0); // Hard-coded margin !

  if tmpColor then
     TextSettingsControl.ResultingTextSettings.FontColor:=
        FMXGrid.TryFontColor(tmpIndex,NewColor,TextSettingsControl.ResultingTextSettings.FontColor);

// IDE version < 10.1 Berlin, FMX Grid does not support TAdtField (sub-dataset)
//  if Value.IsObject and (Value.AsObject is TAdtField) then
//  else

     DefaultDrawColumnCell(Canvas,Column,tmpR,Row,Value,State);
  {$ELSE}
  DoDrawCell;
  {$ENDIF}
end;
{$ENDIF}

initialization
  TBIGrid.Engine:=TBIFMXGridPlugin;

  {$IF CompilerVersion>=31}
  RegisterFmxClasses([TBIFMXGrid.TPrivateGrid]);
  TPresentationProxyFactory.Current.Register(TBIFMXGrid.TPrivateGrid, TControlType.Styled, TStyledPresentationProxy<TStyledGrid>);
  {$ENDIF}

finalization
  {$IF CompilerVersion>=31}
  TPresentationProxyFactory.Current.Unregister(TBIFMXGrid.TPrivateGrid, TControlType.Styled, TStyledPresentationProxy<TStyledGrid>);
  {$ENDIF}

  TBIGrid.Engine:=nil;
end.

