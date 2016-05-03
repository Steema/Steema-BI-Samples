{*********************************************}
{  TeeBI Software Library                     }
{  TChart output                              }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Chart;
{.$DEFINE FMX}

interface

uses
  System.Classes, System.SysUtils, Data.DB,

  {$IFDEF FMX}
  FMXTee.Constants, FMXTee.Procs,
  {$ELSE}
  VCLTee.TeeConst, VCLTee.TeeProcs,
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
  {$IFNDEF FPC}
  TGetText=TFunc<Integer,String>;
  {$ENDIF}

  TBIChartMode=(ByRows, ByColumns, XYZ);

  {$IFDEF FMX}
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IF CompilerVersion>=26}or pidAndroid{$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              )]
  {$ENDIF}
  {$ENDIF}
  TBIChart=class(TChart)
  private
    tmpX,
    tmpBool,
    tmpNoMeasure,
    tmpText : TDataItem;

    FData : TDataItem;

    procedure ClearTitles;

    {$IFDEF TEEPRO}
    function CreateFinancial(const AData:TDataArray; const Dimensions:Integer):TOHLCSeries;
    procedure CreateGrid3D(const AData:TDataArray);
    procedure CreateXYZ(const AData:TDataArray);
    {$ENDIF}

    function CreateSeries(const Y:TDataItem):TChartSeries;
    function DataOrigin:String;
    procedure FillSeries(const ASeries:TChartSeries; Y:TDataItem);
    procedure FixDesignTime(const ASeries:TChartSeries);
    class function GetValue(const AData:TDataItem; const Index:TInteger):TChartValue; static;
    function InitCountSeries(const ACount:TInteger):TChartSeries;

    {$IFDEF TEEPRO}
    class function ExistsAnyDateTime(const AData:TDataArray; const Dimensions:Integer; out IsReversed:Boolean):TDataItem; static;
    class function ExistsData(const AName:String; const AData:TDataArray; const Dimensions:Integer):TDataItem; static;
    class function GetDateTime(const AData:TDataItem; const Index:TInteger; const Reverse:Boolean):TDateTime; static;
    class function IsFinancial(const AData:TDataArray; const Dimensions:Integer):Boolean; static;
    {$ENDIF}

    function NewSeries(const AClass:TChartSeriesClass):TChartSeries; overload;
    function NewSeries(const Count:Integer):TChartSeries; overload;
    function NewSeriesXY(const X,Y:String):TCustomSeries;

    procedure ReadOrigin(Reader: TReader);
    procedure SetDataItem(const Value: TDataItem); // <-- do not rename to SetData (FMX conflict)
    procedure WriteOrigin(Writer: TWriter);
  protected
    Index : TCursorIndex;

    procedure DefineProperties(Filer: TFiler); override;
  public
    SeriesClass : TChartSeriesClass;
    Mode : TBIChartMode;

    Constructor Create(AOwner:TComponent); override;

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

    procedure Init;
  published
    property BevelOuter default bvNone;
    property Data:TDataItem read FData write SetDataItem;
    property View3D default False;
  end;

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
    class function From(const ASeries:array of TChartSeries):TDataItem; overload; static;
  end;

implementation
