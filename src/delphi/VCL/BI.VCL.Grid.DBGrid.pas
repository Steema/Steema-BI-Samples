{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid plugin for VCL DBGrid              }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Grid.DBGrid;

interface

uses
  {$IFNDEF FPC}
  Windows, Messages, UITypes,
  {$ENDIF}
  Types, Classes, SysUtils,
  Controls, Grids, DBGrids, Graphics, DB,
  BI.VCL.Grid, BI.Data, BI.UI;

type
  TBIDBGrid=class;

  TAlternate=class(TPersistent)
  private
    FColor : TColor;
    FEnabled : Boolean;

    IGrid : TBIDBGrid;
    procedure SetColor(const Value: TColor);
    procedure SetEnabled(const Value: Boolean);
  public
  const
    DefaultColor=$E6E6E6;

    property Color:TColor read FColor write SetColor default DefaultColor;
    property Enabled:Boolean read FEnabled write SetEnabled default False;
  end;

  {$IFDEF FPC}
  TProc<T>=procedure(const Value:T);

  TDrawCellEvent=TOnDrawCell;
  {$ENDIF}

  TBIDBGrid=class(TDBGrid)
  private
  const
    OffsetTotals=2;

  var
    FAlternate: TAlternate;
    FColorizers : TDataColorizers;

    FOnAfterDrawCell,
    FOnBeforeDrawCell :  TDrawCellEvent;

    FTotals: Boolean;

    FLastTitle : Integer;
    FLastWidth : Integer;
    FLastFontStyle : TFontStyles;

    function CalcSortWidth:Integer;
    function ParentsOf(AField:TField):Integer;
    procedure RepaintTotals;
    procedure SetAlternate(const Value: TAlternate);
    function TitleWidth(const AColumn:TColumn):Integer;
    function TryColorize(const ARow,ACol:Integer; out AColumn:TColumn; out AIndex:Integer; out APercent:Double):Boolean;

    {$IFNDEF FPC}
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPaint;
    {$ENDIF}
  protected
    function ColumnOf(const X:Integer):TColumn; overload;
    function CreateColumns: TDBGridColumns; override;

    {$IFDEF FPC}
    procedure DoPrepareCanvas(aCol,aRow:Integer; aState: TGridDrawState); override;
    {$ENDIF}

    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    function GetClientRect:TRect; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    {$IFNDEF FPC}
    procedure SetColumnAttributes; override;
    procedure TitleClick(Column: TColumn); override;
    {$ENDIF}
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    function ColumnOf(const AField:TField):TColumn; overload;
    function ColumnOf(const AData:TDataItem):TColumn; overload;
    procedure Traverse(const AColumn:TColumn; const AProc:TProc<TColumn>);

    property TopRow;
  published
    property Alternate:TAlternate read FAlternate write SetAlternate;
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
  protected
    function GetDataSource: TDataSource; override;
    function GetReadOnly:Boolean; override;
    function GetTotals:Boolean; override;
    procedure SetDataSource(const Value: TDataSource); override;
    procedure SetReadOnly(const Value:Boolean); override;
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
