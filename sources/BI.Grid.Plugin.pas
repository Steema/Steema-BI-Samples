{*********************************************}
{  TeeBI Software Library                     }
{  Abstract TBIGridPlugin class               }
{  Copyright (c) 2018-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Grid.Plugin;

{

  This abstract class is intented to use different Grid controls to display
  TeeBI data, like for example the standard VCL/FMX TGrid, Steema's TeeGrid
  and others.
}

interface

uses
  System.Classes,
  BI.DataItem, BI.UI, Data.DB;

type
  TBIGridPluginClass=class of TBIGridPlugin;

  // Pending: Try to merge VCL and FMX TBIGridPlugin classes into a single one.

  // Abstract class to define Grid control classes as "plugins" of TBIGrid.
  // See VCLBI.Grid.DBGrid unit for an example, using the VCL TDBGrid.
  TBIGridPlugin=class abstract
  protected
    IAlternate : TAlternateColor;

    procedure AutoWidth; virtual; abstract;
    procedure ChangedAlternate(Sender:TObject); virtual; abstract;
    function GetCanSort: Boolean; virtual; abstract;
    function GetDataSource: TDataSource; virtual; abstract;
    function GetEditorClass:String; virtual; abstract;
    function GetReadOnly:Boolean; virtual; abstract;
    function GetSortEnabled: Boolean; virtual; abstract;
    function GetTotals:Boolean; virtual; abstract;
    procedure SetDataSource(const Value: TDataSource); virtual; abstract;
    procedure SetFilters(const Value:Boolean); virtual; abstract;
    procedure SetOnRowChanged(const AEvent:TNotifyEvent); virtual; abstract;
    procedure SetReadOnly(const Value:Boolean); virtual; abstract;
    procedure SetRowNumber(const Value:Boolean); virtual; abstract;
    procedure SetPopup(const Value:TObject); virtual; abstract;
    procedure SetSearch(const Value:Boolean); virtual; abstract;
    procedure SetSortEnabled(const Value: Boolean); virtual; abstract;
    procedure SetTotals(const Value:Boolean); virtual; abstract;
  public
    Constructor Create(const AOwner:TComponent); virtual; abstract;

    procedure BindTo(const ADataSet:TDataSet); virtual; abstract;
    procedure Colorize(const AItems:TDataColorizers); virtual; abstract;
    procedure Duplicates(const AData:TDataItem; const Hide:Boolean); virtual; abstract;

    function GetControl:TObject; virtual; abstract; // Returns TObject, for compat VCL <-> FMX
    function GetObject:TObject; virtual; abstract;

    property CanSort:Boolean read GetCanSort;
    property DataSource:TDataSource read GetDataSource write SetDataSource;
    property EditorClass:String read GetEditorClass;
    property ReadOnly:Boolean read GetReadOnly write SetReadOnly;
    property SortEnabled:Boolean read GetSortEnabled write SetSortEnabled;
    property Totals:Boolean read GetTotals write SetTotals;
  end;

implementation

end.
