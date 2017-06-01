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
