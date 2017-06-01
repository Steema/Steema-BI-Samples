{*********************************************}
{  TeeBI Software Library                     }
{  TChart Financial Charts                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Chart.Financial;
{.$DEFINE FMX}

interface

uses
  BI.Arrays, BI.DataItem,

  {$IFDEF FMX}
  FMXTee.Constants, FMXTee.Procs,
  FMXTee.Engine, FMXTee.Chart,
  {$ELSE}
  VCL.Graphics, VCL.Controls,

  VCLTee.TeeConst, VCLTee.TeeProcs,
  VCLTee.TeeGDIPlus,
  VCLTee.TeEngine, VCLTee.Chart,
  {$ENDIF}

  {$IFDEF FMX}
  FMXBI.Chart.Plugin,
  {$ELSE}
  VCLBI.Chart.Plugin,
  {$ENDIF}

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}
  {$ENDIF}

  {$IFDEF TEEPRO}
  {$IFDEF FMX}
  FMXTee.Series.OHLC, FMXTee.Series.Candle
  {$ELSE}
  VCLTee.OHLChart, VCLTee.CandleCh
  {$ENDIF}
  {$ENDIF}

  ;

type
  TOHLCSeriesClass=class of TOHLCSeries;

  TFinancialChart=record
  private
    class function CreateFinancial(const AChart:TBITChart;
                             const ADate,AOpen,AClose,AHigh,ALow,AVolume:TDataItem;
                             const AReverseDate:Boolean):TOHLCSeries; static;

    class function GetDateTime(const AData:TDataItem; const Index:TInteger;
                               const Reverse:Boolean):TDateTime; static;
  public
    class var
      SeriesClass : TOHLCSeriesClass;

    class function CanReuse(const AChart:TBITChart):Boolean; static;

    class procedure Fill(const AChart:TBITChart;
                         const AValues:TDataArray;
                         const AX:TDataItem); static;

    class function Guess(const AValues:TDataArray):Boolean; static;
  end;

implementation
