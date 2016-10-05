{*********************************************}
{  TeeBI Software Library                     }
{  TChart 3D Charts                           }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Chart.ThreeD;
{$DEFINE FMX}

interface

uses
  BI.Data,

  {$IFDEF FMX}
  FMXTee.Constants, FMXTee.Procs, BI.FMX.DataControl, BI.FMX.Grid,
  FMXTee.Engine, FMXTee.Chart,
  BI.FMX.Chart.Plugin,
  {$ELSE}
  VCL.Graphics, VCL.Controls,

  VCLTee.TeeConst, VCLTee.TeeProcs, BI.VCL.DataControl, BI.VCL.Grid,
  VCLTee.TeeGDIPlus,
  VCLTee.TeEngine, VCLTee.Chart,
  BI.VCL.Chart.Plugin,
  {$ENDIF}

  {$IFDEF FMX}
  FMXTee.Series.Surface
  {$ELSE}
  VCLTee.TeeSurfa
  {$ENDIF}
  ;

type
  TCustom3DSeriesClass=class of TCustom3DSeries;

  TThreeDChart=record
  public
    class function CanReuse(const AChart:TBITChart):Boolean; static;

    class procedure CreateGrid3D(const AChart:TBITChart;
                                 const X,Y,Z:TDataItem;
                                 const ADirection:TBIChartDirection); static;

    class procedure CreateGridTable(const AChart:TBITChart;
                                    const AData:TDataArray;
                                    const ADirection:TBIChartDirection); static;

    class procedure CreateXYZ(const AChart:TBITChart;
                              const X,Y,Z:TDataItem); static;

    class function Create3DSeries(const AChart:TBITChart):TCustom3DSeries; static;

    class procedure FinishSeries(const AChart:TChart;
                                 const ASeries:TChartSeries;
                                 const AStacked:TBIChartStacked); static;

    class procedure FinishXYZ(const AChart:TChart); static;

    class function SetSeries3D(const AChart:TBITChart;
                               const AClass:TChartSeriesClass):Boolean; static;
  end;

implementation
