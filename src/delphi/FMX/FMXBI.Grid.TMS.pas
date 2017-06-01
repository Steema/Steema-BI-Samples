{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid plugin for TMS Firemonkey Grid     }
{  http://www.tmssoftware.com                 }
{                                             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Grid.TMS;

interface

uses
  System.Classes, Data.DB, FMXBI.Grid, BI.DataItem, FMX.Controls,
  FMX.TMSLiveGrid, BI.DataSet, FMX.Grid,
  FMX.Bind.Editors, Data.Bind.Components,
  Data.Bind.DBScope, Data.Bind.Grid, FMX.Bind.Grid,
  FMX.TMSLiveGridDataBinding, BI.UI;

type
  TBITMSGrid=class(TTMSFMXLiveGrid)
  private
    FBindingEditor: TTMSFMXBindLiveGridEditor;
    FLastTitle : Integer;
    IDataSet : TBIDataSet;

    BindingList : TBindingsList;
    LinkGrid: TLinkGridToDataSource;

    FOnRowChanged : TNotifyEvent;

    {$IF CompilerVersion>25}
    //procedure ClickedHeader(Column:TColumn);
    {$ENDIF}

    procedure DataChange(Sender: TObject; Field: TField);
    //procedure SetHeaderStyle(const AStyle:TFontStyles);
  protected
    BindSource : TBindSourceDB;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;
  end;

  TBITMSGridPlugin=class(TBIGridPlugin)
  private
    IGrid : TBITMSGrid;

    IItems : TDataColorizers;

    {
    OldGetCellColor : TTMSGridGetCellColor;

    procedure GetCellColor(Sender: TObject; ARow,ACol: Integer;
                AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
    }
  protected
    function GetDataSource: TDataSource; override;
    function GetTotals:Boolean; override;
    procedure SetDataSource(const Value: TDataSource); override;
    procedure SetTotals(const Value:Boolean); override;
  public
    Constructor Create(const AOwner:TComponent); override;

    procedure BindTo(const ADataSet:TDataSet); override;
    procedure Colorize(const AItems:TDataColorizers); override;
    procedure Duplicates(const AData:TDataItem; const Hide:Boolean); override;
    function GetObject:TObject; override;
  end;

implementation
