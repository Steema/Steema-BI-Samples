{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid plugin for SMDBGrid                }
{  http://www.scalabium.com/smdbgrid.htm      }
{                                             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Grid.SMDBGrid;

interface

uses
  WinAPI.Windows, WinAPI.Messages,
  System.Types, System.UITypes, System.Classes, System.SysUtils,
  VCL.Controls, VCL.Grids, SMDBGrid, VCL.Graphics, Data.DB,
  VCLBI.Grid, BI.DataItem, VCL.DBGrids;

type
  TBISMDBGrid=class(TSMDBGrid)
  private
  const
    OffsetTotals=2;

  var
    FTotals: Boolean;

    FLastTitle : Integer;
    FLastWidth : Integer;
    FLastFontStyle : TFontStyles;

    function CalcSortWidth:Integer;
    function ColumnOf(const X:Integer):TColumn; overload;
    function ParentsOf(AField:TField):Integer;
    procedure RepaintTotals;

    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPaint;
  protected
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    function GetClientRect:TRect; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure TitleClick(Column: TColumn); override;
  public
    Constructor Create(AOwner:TComponent); override;

    function ColumnOf(const AField:TField):TColumn; overload;
    function ColumnOf(const AColumn:TDataItem):TColumn; overload;
    procedure Traverse(const AColumn:TColumn; const AProc:TProc<TColumn>);
  published
    property DefaultColWidth;
    property DefaultRowHeight;
    property DrawingStyle default TGridDrawingStyle.gdsThemed;
    property GridLineWidth;
    property IndicatorOffset;
    property ReadOnly default True;
  end;

  TBISMDBGridPlugin=class(TBIGridPlugin)
  private
    IGrid : TBISMDBGrid;

  protected
    function GetDataSource: TDataSource; override;
    function GetReadOnly:Boolean; override;
    function GetTotals:Boolean; override;
    procedure SetDataSource(const Value: TDataSource); override;
    procedure SetReadOnly(const Value:Boolean); override;
    procedure SetTotals(const Value:Boolean); override;
  public
    Constructor Create(const AOwner:TComponent); override;

    procedure BindTo(const ADataSet:TDataSet); override;
    procedure Duplicates(const AData:TDataItem; const Hide:Boolean); override;
    function GetObject:TObject; override;
  end;

implementation

uses
  BI.DataSet, BI.DB.DataSet, System.Math;

{ TBISMDBGridPlugin }

procedure TBISMDBGridPlugin.BindTo(const ADataSet:TDataSet);
begin
  if (IGrid.DataSource=nil) or (IGrid.DataSource.Owner<>IGrid) then
     IGrid.DataSource:=TDataSource.Create(IGrid);

  IGrid.FLastTitle:=-1;
  IGrid.DataSource.DataSet:=ADataSet;
end;

procedure TBISMDBGridPlugin.SetDataSource(const Value: TDataSource);
begin
  IGrid.DataSource:=Value;
end;

procedure TBISMDBGridPlugin.SetReadOnly(const Value: Boolean);
begin
  IGrid.ReadOnly:=Value;
end;

procedure TBISMDBGridPlugin.SetTotals(const Value:Boolean);
begin
  if IGrid.FTotals<>Value then
  begin
    IGrid.FTotals:=Value;

    IGrid.LayoutChanged;

    if Value then
       IGrid.RepaintTotals;
  end;
end;

constructor TBISMDBGridPlugin.Create(const AOwner: TComponent);
begin
  inherited;

  IGrid:=TBISMDBGrid.Create(AOwner);
  IGrid.Align:=TAlign.alClient;

  if AOwner is TWinControl then
     IGrid.Parent:=TWinControl(AOwner);
end;

procedure TBISMDBGridPlugin.Duplicates(const AData:TDataItem; const Hide:Boolean);
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

function TBISMDBGridPlugin.GetObject: TObject;
begin
  result:=IGrid;
end;

function TBISMDBGridPlugin.GetDataSource: TDataSource;
begin
  result:=IGrid.DataSource;
end;

function TBISMDBGridPlugin.GetReadOnly: Boolean;
begin
  result:=IGrid.ReadOnly;
end;

function TBISMDBGridPlugin.GetTotals: Boolean;
begin
  result:=IGrid.FTotals;
end;

{ TBISMDBGrid }

Constructor TBISMDBGrid.Create(AOwner: TComponent);
begin
  inherited;

  FLastTitle:=-1;

  DrawingStyle:=TGridDrawingStyle{ TBISMDBGrid }.gdsThemed;
  ReadOnly:=True;
end;

function TBISMDBGrid.CalcSortWidth:Integer;
begin
  result:=DefaultRowHeight div 2;
end;

procedure TBISMDBGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);

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
      tmpTop,
      Y0,
      Y1,
      X : Integer;
  begin
    tmpTop:=(ARect.Bottom-ARect.Top) div 3;

    SortWidth:=CalcSortWidth;

    X:=ARect.Right-SortWidth-SortIndicatorMargin;

    Y0:=ARect.Top+tmpTop;
    Y1:=ARect.Bottom-tmpTop;

    if Desc then
       Canvas.Polygon([ Point(X,Y1),
                        Point(X+(SortWidth div 2),Y0),
                        Point(X+SortWidth,Y1)
                    ])
    else
       Canvas.Polygon([ Point(X,Y0),
                        Point(X+(SortWidth div 2),Y1),
                        Point(X+SortWidth,Y0)
                    ]);
  end;

var MasterCol,
    Col : TColumn;
    tmpCol : TDataItem;
    tmpData : TBIDataset;
    tmpIndex : Integer;
begin
  inherited;

  if ARow<TopRow then
  begin
    Col:=ColumnOf(ACol);

    if Col<>nil then
    begin
      CalcTitleRect(Col,ARow,MasterCol);

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
end;

procedure TBISMDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FTotals and ( (FGridState=gsColSizing) or (FGridState=gsColMoving) ) then
  begin
    inherited;
    RepaintTotals;
  end
  else
    inherited;
end;

procedure TBISMDBGrid.WMNCPaint(var Message: TMessage);
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

procedure TBISMDBGrid.WmVScroll(var Message: TWMVScroll);
begin
  if Message.ScrollCode=SB_THUMBTRACK then
     Message.ScrollCode:=SB_THUMBPOSITION;

  inherited;
end;

procedure TBISMDBGrid.RepaintTotals;
begin
  SendMessage(Handle,WM_NCPaint,0,0);
end;

function TBISMDBGrid.GetClientRect: TRect;
begin
  result:=inherited;

  if FTotals then
     result.Bottom:=result.Bottom-(DefaultRowHeight+OffsetTotals);
end;

function TBISMDBGrid.ColumnOf(const X:Integer):TColumn;
var tmp : Integer;
begin
  if (X>=FixedCols) and (X<=Columns.Count) then
  begin
    tmp:=RawToDataColumn(X);

    if tmp=-1 then
       result:=nil
    else
       result:=Columns[tmp];
  end
  else
     result:=nil;
end;

function TBISMDBGrid.ColumnOf(const AField:TField): TColumn;
var t : Integer;
begin
  for t:=0 to Columns.Count-1 do
      if Columns[t].Field=AField then
         Exit(Columns[t]);

  result:=nil;
end;

function TBISMDBGrid.ColumnOf(const AColumn: TDataItem): TColumn;
var tmp : TField;
begin
  if (DataSource<>nil) and (DataSource.DataSet<>nil) then
     tmp:=TBIDataSetSource.FieldOf(AColumn,DataSource.DataSet)
  else
     tmp:=nil;

  if tmp=nil then
     result:=nil
  else
     result:=ColumnOf(tmp);
end;

procedure TBISMDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var p : TGridcoord;
    Col : TColumn;
    tmp : TCursor;
begin
  inherited;

  tmp:=crDefault;

  p:=MouseCoord(X,Y);

  Col:=ColumnOf(p.X);

  if (Col<>nil) and (p.Y<=ParentsOf(Col.Field)) then
     tmp:=crHandPoint;

  Cursor:=tmp;
end;

procedure TBISMDBGrid.Traverse(const AColumn:TColumn; const AProc:TProc<TColumn>);

  procedure DoProc(const AColumn:TColumn);
  var t : Integer;
  begin
    AProc(AColumn);

    if AColumn.Field is TObjectField then
       for t:=0 to TObjectField(AColumn.Field).FieldCount-1 do
           DoProc(ColumnOf(TObjectField(AColumn.Field).Fields[t]));
  end;

begin
  if AColumn<>nil then
     DoProc(AColumn);
end;

procedure TBISMDBGrid.TitleClick(Column: TColumn);

  procedure ChangeWidth(const ColIndex,AWidth:Integer);
  var Old : Boolean;
  begin
    Old:=FUpdateFields;
    FUpdateFields:=False;
    try
      Columns[ColIndex].Width:=AWidth;
    finally
      FUpdateFields:=Old;
    end;
  end;

var Y,
    tmpDef,
    tmpLeft : Integer;
    P : TPoint;
    tmp : TField;
begin
  inherited;

  tmp:=Column.Field;

  if tmp<>nil then
  begin
    // Check the mouse Y position on click, to determine if the column should
    // be expanded / collapsed, or sorted.

    if tmp.ParentField<>nil then
    begin
      P:=ScreenToClient(Mouse.CursorPos);
      Y:=MouseCoord(P.X,P.Y).Y;

      if Y<ParentsOf(tmp) then
      begin
        tmp:=tmp.ParentField;
        Column:=Column.ParentColumn;
      end;
    end;

    if tmp is TADTField then
    begin
      Column.Expanded:=not Column.Expanded;

      if not Column.Expanded then
      begin
        Canvas.Font:=Column.Title.Font;
        Column.Width:=Canvas.TextWidth(Column.Title.Caption) + 4;
      end;
    end
    else
    begin
      if FLastTitle<>-1 then
      begin
        Columns[FLastTitle].Title.Font.Style:=FLastFontStyle;
        ChangeWidth(FLastTitle,FLastWidth);
      end;

      FLastTitle:=Column.Index;
      FLastFontStyle:=Columns[FLastTitle].Title.Font.Style;

      tmpLeft:=LeftCol;

      if DataSource.DataSet is TBIDataSet then
         if DataSource.DataSet.Active then
            TBIDataSet(DataSource.DataSet).InvertSortBy(Column.Field);

      Columns[FLastTitle].Title.Font.Style:=[fsBold];
      LeftCol:=tmpLeft;

      FLastWidth:=Columns[FLastTitle].Width;
      tmpDef:=Columns[FLastTitle].DefaultWidth;

      ChangeWidth(FLastTitle,Max(FLastWidth,tmpDef+CalcSortWidth));
    end;
  end;
end;

function TBISMDBGrid.ParentsOf(AField:TField):Integer;
begin
  result:=0;

  if AField<>nil then
  while AField.ParentField<>nil do
  begin
    Inc(result);
    AField:=AField.ParentField;
  end;
end;

initialization
  TBIGrid.Engine:=TBISMDBGridPlugin;
finalization
  TBIGrid.Engine:=nil;
end.
