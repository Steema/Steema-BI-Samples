{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid plugin for Firemonkey Grid         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Grid.Grid;

interface

uses
  System.Classes, FMX.Grid, FMX.Bind.Editors, Data.Bind.Components,
  Data.Bind.DBScope, Data.Bind.Grid, FMX.Bind.Grid, BI.FMX.Grid, BI.DataSet,
  BI.Data, FMX.Controls, Data.DB, System.UITypes, BI.UI;

type
  // Unfortunately, it is not possible to derive this class from FMX TGrid.
  // Bindings work partially, columns are created but the grid shows empty.
  TBIFMXGrid=class
  private
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

    {$IF CompilerVersion>25}
    procedure ClickedHeader(Column:TColumn);
    {$ENDIF}

    procedure CreateBindings;
    procedure DataChange(Sender: TObject; Field: TField);
    procedure SetColumnSort(const Value: Boolean);
    procedure SetHeaderStyle(const AStyle:TFontStyles);
  protected
    BindSource : TBindSourceDB;
  public
    Constructor Create(AOwner:TComponent);
    Destructor Destroy; override;

    procedure AutoSizeColumns;

    property Grid:TGrid read IFMXGrid;

  //published
    property ColumnSort:Boolean read FColumnSort write SetColumnSort default True;
    property OnRowChanged:TNotifyEvent read FOnRowChanged write FOnRowChanged;
  end;

  TBIFMXGridPlugin=class(TBIGridPlugin)
  private
    IGrid : TBIFMXGrid;
  protected
    procedure ChangedAlternate(Sender:TObject); override;
    function GetDataSource: TDataSource; override;
    function GetTotals:Boolean; override;
    procedure SetDataSource(const Value: TDataSource); override;
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
