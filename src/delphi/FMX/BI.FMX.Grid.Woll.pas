{*********************************************}
{  TeeBI Software Library                     }
{  TBIGrid plugin for Woll2Woll FirePower     }
{  http://www.woll2woll.com                   }
{                                             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Grid.Woll;

interface

uses
  System.Classes, Data.DB, BI.FMX.Grid, BI.Data, FMX.Controls,
  FMX.wwDataGrid, FMX.wwLayouts, FMX.wwBaseGrid, BI.DataSet, FMX.Grid;

type
  TBIWollGrid=class(TwwDataGrid)
  private
    FLastTitle : Integer;
    IDataSet : TBIDataSet;

    FOnRowChanged : TNotifyEvent;

    {$IF CompilerVersion>25}
    //procedure ClickedHeader(Column:TColumn);
    {$ENDIF}

    procedure DataChange(Sender: TObject; Field: TField);
    //procedure SetHeaderStyle(const AStyle:TFontStyles);
  public
    Constructor Create(AOwner:TComponent); override;
  end;

  TBIWollGridPlugin=class(TBIGridPlugin)
  private
    IGrid : TBIWollGrid;

  protected
    function GetDataSource: TDataSource; override;
    function GetTotals:Boolean; override;
    procedure SetDataSource(const Value: TDataSource); override;
    procedure SetTotals(const Value:Boolean); override;
  public
    Constructor Create(const AOwner:TComponent); override;

    procedure BindTo(const ADataSet:TDataSet); override;
    procedure Duplicates(const AData:TDataItem; const Hide:Boolean); override;
    function GetObject:TObject; override;
  end;

implementation
