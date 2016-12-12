{*********************************************}
{  TeeBI Software Library                     }
{  TChart output                              }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Chart;
{$DEFINE FMX}

{$SCOPEDENUMS ON}

interface

uses
  System.Classes, System.SysUtils, Data.DB,

  {$IFDEF FMX}
  FMX.Controls,
  FMXTee.Constants, FMXTee.Procs, BI.FMX.DataControl, BI.FMX.Grid,
  {$ELSE}
  Winapi.Messages,
  VCLTee.TeeConst, VCLTee.TeeProcs, BI.VCL.DataControl, BI.VCL.Grid,
  VCLTee.TeeGDIPlus,
  {$ENDIF}

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}
  {$ENDIF}

  {$IFDEF FMX}
  System.UITypes, FMXTee.Chart, FMXTee.Engine, FMXTee.Series,
  BI.FMX.Chart.Plugin,

  {$IFDEF TEEPRO}
  BI.FMX.Chart.Geo,
  {$ENDIF}

  {$ELSE}

  VCL.Graphics, VCL.Controls, VCLTee.Chart, VCLTee.TeEngine, VCLTee.Series,
  //VCLTee.BubbleCh,
  BI.VCL.Chart.Plugin,

  {$IFDEF TEEPRO}
  BI.VCL.Chart.Geo,
  {$ENDIF}

  {$ENDIF}

  BI.Data, BI.Arrays, BI.Summary, BI.DataSource, BI.Data.Info;

type
  TBIChartItems=class(TPersistent)
  private
    IX,
    IZ,
    IText,
    IGroup,
    IColors : String;

    IY : TTextArray;

    {$IFDEF TEEPRO}
    FGeoContext : TGeoContext;
    {$ENDIF}

    procedure ExchangeXY;
    procedure ExchangeXZ;
    procedure ExchangeYZ;
    procedure Guess(const AData:TDataArray);
    procedure GetXYZ(out AX,AY,AZ:TDataItem);
    procedure MoveXToY;

    procedure ReadColors(Reader: TReader);
    procedure ReadGroup(Reader: TReader);
    procedure ReadText(Reader: TReader);
    procedure ReadX(Reader: TReader);
    procedure ReadY(Reader: TReader);
    procedure ReadZ(Reader: TReader);
    procedure WriteColors(Writer: TWriter);
    procedure WriteGroup(Writer: TWriter);
    procedure WriteText(Writer: TWriter);
    procedure WriteX(Writer: TWriter);
    procedure WriteY(Writer: TWriter);
    procedure WriteZ(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded(const AData:TDataArray);
  public
    X,
    Z,
    Text,
    Colors,
    Group : TDataItem;

    Y : TDataArray;

    procedure Clear;

    function CanFinancial:Boolean;
    function CanGeoGraphic:Boolean;
    function CanThreeD:Boolean;
    function CanXYZ:Boolean;
  end;

  {$IFNDEF FPC}
  TGetText=TFunc<Integer,String>;
  {$ENDIF}

  TBIChartMode=(Automatic, XY, ThreeD, Financial, Geographic);
  TBIChart3DMode=(Automatic, Table, Grid, XYZ);
  TBIChartDimensions=(Automatic, View2D, Orthogonal, View3D);
  TBIChartLegend=(Automatic, Show, Hide);
  TBIChartMarks=(Automatic, Show, Hide);
  TBISeriesDirection=(Automatic, Horizontal, Vertical);

  TBIChart=class;

  TBIChartOptions=class(TPersistent)
  private
    FDimensions: TBIChartDimensions;
    FDirection : TBIChartDirection;
    FItems : TBIChartItems;
    FLegend: TBIChartLegend;
    FMarks: TBIChartMarks;
    FMode: TBIChartMode;
    FSeriesDirection: TBISeriesDirection;
    FStacked: TBIChartStacked;
    FXYZMode: TBIChart3DMode;

    IChart : TBIChart;

    procedure Finish;
    procedure FinishViewDimensions;
    function GetSeries2D: TChartSeriesClass;
    function GetSeries3D: TChartSeriesClass;
    procedure SetDimensions(const Value: TBIChartDimensions);
    procedure SetDirection(const Value: TBIChartDirection);
    procedure SetItems(const Value: TBIChartItems);
    procedure SetLegend(const Value: TBIChartLegend);
    procedure SetMarks(const Value: TBIChartMarks);
    procedure SetMode(const Value: TBIChartMode);
    procedure SetSeries2D(const Value: TChartSeriesClass);
    procedure SetSeries3D(const Value: TChartSeriesClass);
    procedure SetStacked(const Value: TBIChartStacked);
    procedure SetXYZMode(const Value: TBIChart3DMode);
    procedure SetSeriesDirection(const Value: TBISeriesDirection);
  public
    Constructor Create(const AChart:TBIChart);
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    procedure ChangeSeries2D(const AClass:TChartSeriesClass;
                             const AHorizontal,ALinePointer:Boolean);
  published
    property Dimensions:TBIChartDimensions read FDimensions write SetDimensions
                        default TBIChartDimensions.Automatic;

    property Direction:TBIChartDirection read FDirection write SetDirection default TBIChartDirection.Automatic;
    property Items:TBIChartItems read FItems write SetItems;
    property Legend:TBIChartLegend read FLegend write SetLegend
                        default TBIChartLegend.Automatic;
    property Marks:TBIChartMarks read FMarks write SetMarks
                        default TBIChartMarks.Automatic;
    property Mode:TBIChartMode read FMode write SetMode default TBIChartMode.Automatic;
    property Series2D:TChartSeriesClass read GetSeries2D write SetSeries2D;
    property Series3D:TChartSeriesClass read GetSeries3D write SetSeries3D;
    property SeriesDirection: TBISeriesDirection read FSeriesDirection write SetSeriesDirection
                          default TBISeriesDirection.Automatic;
    property Stacked:TBIChartStacked read FStacked write SetStacked
                        default TBIChartStacked.Automatic;
    property XYZMode:TBIChart3DMode read FXYZMode write SetXYZMode default TBIChart3DMode.Automatic;
  end;

  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IF CompilerVersion>=26}or pidAndroid{$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              )]
  {$ENDIF}
  TBIChart=class(TBIDataControl)
  private
    FOptions : TBIChartOptions;

    IDataInfo : TDataInfo;
    IDirtyData : TDataItem;

    procedure AddXY(const AItems:TBIChartItems);

    procedure ApplyData(const AData:TDataItem);
    procedure CreateChart;
    function CreateSeries(const X,Y:TDataItem):TChartSeries;
    function DirectChart:TBITChart;
    procedure FillSeries(const ASeries:TChartSeries; const X,Y,AText:TDataItem);
    function DoFreeSeries:Boolean;
    function GetChart:TBITChart;
    function GuessRealData(const AData:TDataArray):TDataArray;
    function IsDesigning:Boolean;
    procedure SetOptions(const Value: TBIChartOptions);
  protected
    Index : TCursorIndex;

    RealMode : TBIChartMode;
    RealMode3D : TBIChart3DMode;

    procedure ExchangeXY;
    procedure ExchangeYZ;
    procedure ExchangeXZ;

    procedure DirectRefresh;
    procedure Loaded; override;

    {$IFDEF FMX}
    procedure Paint; override;
    {$ELSE}
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    {$ENDIF}

    procedure ReadState(Reader: TReader); override;
    procedure ResetTheme;
    procedure SetDataDirect(const Value: TDataItem); override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Clear;

    {$IFNDEF FPC}
    procedure Fill(const Map:TDataMap; const Text:TGetText=nil); overload;
    {$ENDIF}

    procedure Fill(const AData:TDataArray); overload;
    procedure Fill(const AItems:TDataItems); overload;
    procedure Fill(const AHistogram:THistogram; const ASource:TDataItem); overload;
    procedure Fill(const ASummary:TSummary); overload;
    procedure Fill(const ACursor:TDataCursor; const AItems:TDataArray=nil); overload;

    function GetChildOwner: TComponent; override;
    Procedure GetChildren(Proc:TGetChildProc; Root:TComponent); override;

    procedure SetTwoAxes;
    procedure SplitAxes(const Vertical:Boolean);
  published
    property Chart:TBITChart read GetChart;

    {$IFNDEF FMX}
    property Height default 250;
    property Width default 400;
    {$ENDIF}

    property Options:TBIChartOptions read FOptions write SetOptions;
  end;

implementation
