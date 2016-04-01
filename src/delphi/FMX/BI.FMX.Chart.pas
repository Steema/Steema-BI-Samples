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
  VCL.Controls, VCLTee.Chart, VCLTee.TeEngine, VCLTee.Series,
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
    function CreateFinancial(const ADatas:TDataArray; const Dimensions:Integer):TOHLCSeries;
    procedure CreateGrid3D(const ADatas:TDataArray);
    procedure CreateXYZ(const ADatas:TDataArray);
    {$ENDIF}

    function CreateSeries(const Y:TDataItem):TChartSeries;
    class function ExistsAnyDateTime(const ADatas:TDataArray; const Dimensions:Integer; out IsReversed:Boolean):TDataItem; static;
    class function ExistsData(const AName:String; const ADatas:TDataArray; const Dimensions:Integer):TDataItem; static;
    procedure FillSeries(const ASeries:TChartSeries; Y:TDataItem);
    class function GetValue(const AData:TDataItem; const Index:TInteger):TChartValue; static;
    class function GetDateTime(const AData:TDataItem; const Index:TInteger; const Reverse:Boolean):TDateTime; static;
    function InitCountSeries(const ACount:TInteger):TChartSeries;
    class function IsFinancial(const ADatas:TDataArray; const Dimensions:Integer):Boolean; static;
    function NewSeries(const Count:Integer):TChartSeries;
    function NewSeriesXY(const X,Y:String):TPointSeries;
    procedure SetDataItem(const Value: TDataItem); // <-- do not rename to SetData (FMX conflict)

    procedure ReadOrigin(Reader: TReader);
    procedure WriteOrigin(Writer: TWriter);
  protected
    Index : TCursorIndex;

    procedure DefineProperties(Filer: TFiler); override;
  public
    SeriesClass : TChartSeriesClass;
    Mode : TBIChartMode;

    Constructor Create(AOwner:TComponent); override;

    procedure Fill(const Data:TInt32Array); overload;
    procedure Fill(const Data:TInt64Array); overload;

    {$IFNDEF FPC}
    procedure Fill(const Map:TDataMap; const Text:TGetText=nil); overload;
    {$ENDIF}

    procedure Fill(const ADatas:TDataArray; const Dimensions:Integer=0); overload;
    procedure Fill(const ADatas:TDataArray; const ASeries:TDataItem); overload;
    procedure Fill(const AItems:TDataItems; const Dimensions:Integer=0); overload;
    procedure Fill(const Data:TDataSet; const ValueField:Integer; const TextField:Integer=-1); overload;
    procedure Fill(const Data:TDataItem); overload;
    procedure Fill(const Histogram:THistogram; const Source:TDataItem); overload;
    procedure Fill(const Summary:TSummary); overload;
    procedure Fill(const ACursor:TDataCursor; const AItems:TDataArray=nil;
                   const ADimensions:Integer=0); overload;
    procedure FillXY(const Data:TDataSet; const X,Y:Integer);

    procedure Init;
  published
    property BevelOuter default bvNone;
    property Data:TDataItem read FData write SetDataItem;
    property View3D default False;
  end;

implementation
