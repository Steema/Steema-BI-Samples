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
  FMXTee.Constants,
  {$ELSE}
  VCLTee.TeeConst,
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
  VCLTee.Chart, VCLTee.TeEngine, VCLTee.Series,
  {$ENDIF}
  BI.Data, BI.Arrays, BI.Summary;

type
  {$IFNDEF FPC}
  TGetText=TFunc<Integer,String>;
  {$ENDIF}

  TBIChartMode=(ByRows, ByColumns, XYZ);

  {$IFDEF FMX}
  {$IFDEF VER230}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IFDEF VER250}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IFDEF VER260}or pidAndroid{$ENDIF}
              {$IFDEF VER290}or pidiOSDevice64{$ENDIF}
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
    procedure CreateGrid3D(const ADatas:TDataArray);
    procedure CreateXYZ(const ADatas:TDataArray);
    {$ENDIF}

    function CreateSeries(const tmpY:TDataItem):TChartSeries;
    procedure FillSeries(tmpS:TChartSeries; tmpY:TDataItem);
    class function GetValue(const AData:TDataItem; const Index:TInteger):TChartValue; static;
    function InitCountSeries(const ACount:TInteger):TChartSeries;
    function NewSeries(const Count:Integer):TChartSeries;
    function NewSeriesXY(const X,Y:String):TPointSeries;
    procedure SetDataItem(const Value: TDataItem); // <-- do not rename to SetData (FMX conflict)

    procedure ReadOrigin(Reader: TReader);
    procedure WriteOrigin(Writer: TWriter);
  protected
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
    procedure FillXY(const Data:TDataSet; const X,Y:Integer);

    procedure Init;
  published
    property Data:TDataItem read FData write SetDataItem;
    property View3D default False;
  end;

implementation
