{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid plugin for TeeGrid                 }
{                                             }
{  https://www.steema.com/product/gridvcl     }
{                                             }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Grid.TeeGrid;

interface

(*
  This unit connects TBIGrid with TTeeGrid.

  To activate, simply add this unit to your "uses",
  or manually set the desired plugin class:

  TBIGrid.Engine:=TBITeeGridPlugin;

  TeeGrid provides automatic support for many TeeBI features like column
  totals, custom cell coloring, column sorting etc.

*)

uses
  System.Types, System.UITypes, System.Classes, System.SysUtils,
  FMX.Controls, FMX.Menus, Data.DB,

  BI.Grid.Plugin, BI.DataItem, BI.UI,

  BI.GridData,
  FMXTee.Grid,
  Tee.Grid.Totals;

type
  TBITeeGrid=class(TTeeGrid)
  private
    IData : TBIGridData;
    IDataSource : TDataSource;

    FHeader : TTotalsHeader;
    FTotals: TColumnTotals;

    procedure BindTo(const ADataSet:TDataSet);
    function GetTotals:Boolean;
    procedure SetTotals(const Value:Boolean);
  public
    Constructor Create(AOwner:TComponent); override;
  end;

  TBITeeGridPlugin=class(TBIGridPlugin)
  private
    IGrid : TBITeeGrid;

  protected
    procedure AutoWidth; override;
    procedure ChangedAlternate(Sender:TObject); override;
    function GetCanSort: Boolean; override;
    function GetDataSource: TDataSource; override;
    function GetEditorClass:String; override;
    function GetReadOnly:Boolean; override;
    function GetSortEnabled: Boolean; override;
    function GetTotals:Boolean; override;
    procedure SetDataSource(const Value: TDataSource); override;
    procedure SetFilters(const Value:Boolean); override;
    procedure SetOnRowChanged(const AEvent:TNotifyEvent); override;
    procedure SetPopup(const Value:TObject); override;
    procedure SetReadOnly(const Value:Boolean); override;
    procedure SetRowNumber(const Value:Boolean); override;
    procedure SetSearch(const Value:Boolean); override;
    procedure SetSortEnabled(const Value: Boolean); override;
    procedure SetTotals(const Value:Boolean); override;
  public
    Constructor Create(const AOwner:TComponent); override;

    procedure BindTo(const ADataSet:TDataSet); override;
    procedure Colorize(const AItems:TDataColorizers); override;
    procedure Duplicates(const AData:TDataItem; const Hide:Boolean); override;
    function GetControl:TObject; override;
    function GetObject:TObject; override;
  end;

implementation
