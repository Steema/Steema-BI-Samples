{*********************************************}
{  TeeBI Software Library                     }
{  Sub TChart plugin used by TBIChart         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Chart.Plugin;
{.$DEFINE FMX}

{$SCOPEDENUMS ON}

interface

uses
  BI.Arrays, BI.Data,
  System.Classes,
  Data.DB,

  {$IFDEF FMX}
  System.UITypes, FMX.Controls,

  {$IF CompilerVersion<26} // Cannot use FireMonkeyVersion<21 (or 21_0)
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  FMXTee.Constants, FMXTee.Procs, BI.FMX.DataControl, BI.FMX.Grid,
  FMXTee.Engine, FMXTee.Chart
  {$ELSE}
  VCL.Graphics, VCL.Controls,

  VCLTee.TeeConst, VCLTee.TeeProcs, BI.VCL.DataControl, BI.VCL.Grid,
  VCLTee.TeeGDIPlus,
  VCLTee.TeEngine, VCLTee.Chart
  {$ENDIF}

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}
  {$ENDIF}
  ;

// XE6 dcc32 BUG, workaround not available
{$IF CompilerVersion>27}
{$I BI.Chart.Options.inc} // <-- see .inc contents for reason/explanation
{$ENDIF}

const
  WhiteColor={$IFDEF FMX}TAlphaColors.White{$ELSE}clWhite{$ENDIF};

type
  TBIChartDirection=(Automatic, Rows, Columns);
  TBIChartStacked=(Automatic, No, Yes, Stacked100, Side, SideAll, SelfStack);

  TBITChart=class(TChart)
  private
    {$IFDEF TEEPRO}
    function TryAddUniqueTool(const AOwner:TComponent;
                              const AClass:TTeeCustomToolClass;
                              const AName:String):TTeeCustomTool;
    procedure TryDisableTool(const AClass:TTeeCustomToolClass; const ADisable:Boolean);
    {$ENDIF}

    function InitCountSeries(const ACount:TInteger):TChartSeries;
  protected
    FSeries2D : TChartSeriesClass;
    FSeries3D : TChartSeriesClass;

    DefaultXYSeries : TChartSeriesClass;

    LinePointer : Boolean;

    procedure ReadState(Reader: TReader); override;
  public

    class var
       FourItemSeries,
       Three3DSeries : TChartSeriesClass;

    Constructor Create(AOwner:TComponent); override;

    procedure AddLine(const Axis:TChartAxis);

    procedure ChangeSeries2D(const AClass: TChartSeriesClass;
                             const AHorizontal, ALinePointer: Boolean);

    procedure ClearTitles;

    procedure CreateMulti2D(const X,Y,Z:TDataItem; const ADirection:TBIChartDirection);

    procedure Fill(const AData:TDataArray; const ASeries: TDataItem); overload;
    function Fill(const AData:TInt32Array):TChartSeries; overload;
    function Fill(const AData:TInt64Array):TChartSeries; overload;
    function Fill(const AData:BI.Arrays.TDoubleArray):TChartSeries; overload;
    function Fill(const AData:TDataItem):TChartSeries; overload;
    procedure Fill(const AData:TDataSet; const ValueField:Integer; const TextField:Integer=-1); overload;

    function FillXY(const X,Y:TField):TChartSeries; overload;
    function FillXY(const AData:TDataSet; const X,Y:Integer):TChartSeries; overload;

    procedure FinishViewDimensions;

    Function GetParentComponent: TComponent; override;

    class function GetValue(const AData:TDataItem; const Index:TInteger):TChartValue; static;

    Function HasParent:Boolean; override;
    procedure Init(const FreeSeries:Boolean);
    function NewSeries(const AClass:TChartSeriesClass):TChartSeries; overload;
    function NewSeries(const Count:Integer):TChartSeries; overload;
    function NewSeries(const X,Y:String):TChartSeries; overload;

    class procedure SetAxesTitles(const ASeries:TChartSeries; const X,Y:String); static;

    procedure SetParentComponent(AParent: TComponent); override;
    procedure ShowHideAxesWalls(const AVisible:Boolean);

    class procedure TrySetName(const AComponent:TComponent; const APrefix:String); static;

    property Series2D:TChartSeriesClass read FSeries2D write FSeries2D;
    property Series3D:TChartSeriesClass read FSeries3D write FSeries3D;
  published
    property Align default TUICommon.AlignClient;
    property BevelOuter default bvNone;
    property Color default WhiteColor;
    property View3D default False;
  end;

  // Converts data from a Chart or one or more Series to a TDataItem
  TChartData=record
  private
    class procedure InitNotMandatory(const ASeries:TChartSeries;
                                     const ACount:Integer); static;

    class function NewSeries(const AOwner:TComponent;
                             const AClass:TChartSeriesClass):TChartSeries; static;
  public
    class procedure AddSeries(const ASeries: TChartSeries; const ADest: TDataItem); static;

    class function From(const AData:TDataArray;
                        const AOwner:TComponent;
                        const AClass:TChartSeriesClass=nil):TChartSeries; overload; static;

    class function From(const AData:TDataItem;
                        const AOwner:TComponent;
                        const AClass:TChartSeriesClass=nil):TChartSeries; overload; static;

    class function From(const ASeries:TChartSeries):TDataItem; overload; static;
    class function From(const AChart:TCustomChart):TDataItem; overload; static;
    class function From(const ASeries:Array of TChartSeries):TDataItem; overload; static;
  end;

implementation
