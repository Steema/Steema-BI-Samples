{*********************************************}
{  TeeBI Software Library                     }
{  TChart output                              }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Chart;
{$DEFINE FMX}

interface

uses
  System.Classes, System.SysUtils, Data.DB,

  {$IFDEF FMX}
  FMXTee.Constants, FMXTee.Procs, BI.FMX.DataControl, BI.FMX.Grid,
  {$ELSE}
  VCLTee.TeeConst, VCLTee.TeeProcs, BI.VCL.DataControl, BI.VCL.Grid,
  {$ENDIF}

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}
  {$ENDIF}

  {$IFDEF FMX}
  FMXTee.Chart, FMXTee.Engine, FMXTee.Series,
  {$ELSE}
  VCL.Controls, VCLTee.Chart, VCLTee.TeEngine, VCLTee.Series, VCLTee.BubbleCh,
  {$ENDIF}

  {$IFDEF TEEPRO}
  {$IFDEF FMX}
  FMXTee.Series.OHLC, FMXTee.Series.Candle,
  {$ELSE}
  VCLTee.OHLChart, VCLTee.CandleCh,
  {$ENDIF}
  {$ENDIF}

  BI.Data, BI.Arrays, BI.Summary, BI.DataSource;

type
  TBITChart=class(TChart)
  private
    procedure ClearTitles;

    {$IFDEF TEEPRO}
    function CreateFinancial(const AData:TDataArray; const Dimensions:Integer):TOHLCSeries;
    procedure CreateGrid3D(const AData:TDataArray; const ANoMeasure:TDataItem; const ByRows:Boolean);
    procedure CreateXYZ(const AData:TDataArray);
    {$ENDIF}

    procedure Fill(const AData:TDataArray; const ASeries: TDataItem);
    procedure FixDesignTime(const ASeries:TChartSeries);
    class function GetDateTime(const AData:TDataItem; const Index:TInteger; const Reverse:Boolean):TDateTime;
    class function GetValue(const AData:TDataItem; const Index:TInteger):TChartValue; static;
    procedure Init;
    function NewSeries(const AClass:TChartSeriesClass):TChartSeries; overload;
    function NewSeries(const Count:Integer):TChartSeries; overload;
    function NewSeries(const X,Y:String):TCustomSeries; overload;
  protected
  public
    SeriesClass : TChartSeriesClass;

    Constructor Create(AOwner:TComponent); override;
  published
    property Align default TUICommon.AlignClient;
    property BevelOuter default bvNone;
    property View3D default False;
  end;

  {$IFNDEF FPC}
  TGetText=TFunc<Integer,String>;
  {$ENDIF}

  TBIChartMode=(ByRows, ByColumns, XYZ);

  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IF CompilerVersion>=26}or pidAndroid{$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              )]
  {$ENDIF}
  TBIChart=class(TBIDataControl)
  private
    tmpNoMeasure : TDataItem;

    function AddCount(const AData:TDataItem):TChartSeries;
    procedure AddXY(const X,Y,AText:TDataItem; const AData:TDataArray; const Dimensions:Integer);
    procedure ApplyData(const AData:TDataItem);
    procedure CreateChart;
    function CreateSeries(const X,Y:TDataItem):TChartSeries;
    procedure FillSeries(const ASeries:TChartSeries; const X,Y,AText:TDataItem);
    function GetChart:TBITChart;
    function InitCountSeries(const ACount:TInteger):TChartSeries;

  protected
    Index : TCursorIndex;

    {$IFDEF TEEPRO}
    class function IsFinancial(const AData:TDataArray; const Dimensions:Integer):Boolean; static;
    {$ENDIF}

    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    procedure SetDataDirect(const Value: TDataItem); override;
  public
    Mode : TBIChartMode;

    Constructor Create(AOwner:TComponent); override;

    procedure Clear;

    function Fill(const AData:TInt32Array):TChartSeries; overload;
    function Fill(const AData:TInt64Array):TChartSeries; overload;
    function Fill(const AData:TDoubleArray):TChartSeries; overload;

    {$IFNDEF FPC}
    procedure Fill(const Map:TDataMap; const Text:TGetText=nil); overload;
    {$ENDIF}

    procedure Fill(const AData:TDataArray; const Dimensions:Integer=0); overload;
    procedure Fill(const AData:TDataArray; const ASeries:TDataItem); overload;
    procedure Fill(const AItems:TDataItems; const Dimensions:Integer=0); overload;
    procedure Fill(const AData:TDataSet; const ValueField:Integer; const TextField:Integer=-1); overload;
    function Fill(const AData:TDataItem):TChartSeries; overload;
    procedure Fill(const AHistogram:THistogram; const ASource:TDataItem); overload;
    procedure Fill(const ASummary:TSummary); overload;
    procedure Fill(const ACursor:TDataCursor; const AItems:TDataArray=nil;
                   const ADimensions:Integer=0); overload;
    function FillXY(const AData:TDataSet; const X,Y:Integer):TChartSeries; overload;
    function FillXY(const X,Y:TField):TChartSeries; overload;
  published
    property Chart:TBITChart read GetChart;

    {$IFNDEF FMX}
    property Height default 250;
    property Width default 400;
    {$ENDIF}
  end;

  // Converts data from a Chart or one or more Series to a TDataItem
  TChartData=record
  private
    class procedure InitNotMandatory(const ASeries:TChartSeries;
                                     const ACount:Integer); static;
  public
    class procedure AddSeries(const ASeries: TChartSeries; const ADest: TDataItem); static;
    class function From(const AData:TDataItem;
                        const AOwner:TComponent;
                        const AClass:TChartSeriesClass=nil):TChartSeries; overload; static;
    class function From(const ASeries:TChartSeries):TDataItem; overload; static;
    class function From(const AChart:TCustomChart):TDataItem; overload; static;
    class function From(const ASeries:Array of TChartSeries):TDataItem; overload; static;
  end;

implementation
