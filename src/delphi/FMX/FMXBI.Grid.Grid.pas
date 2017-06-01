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
