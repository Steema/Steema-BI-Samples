{*********************************************}
{  TeeBI Software Library                     }
{  TChart output                              }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Chart;
{.$DEFINE FMX}

{$SCOPEDENUMS ON}

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
  System.UITypes, FMXTee.Chart, FMXTee.Engine, FMXTee.Series,
  {$ELSE}
  VCL.Graphics, VCL.Controls, VCLTee.Chart, VCLTee.TeEngine, VCLTee.Series, VCLTee.BubbleCh,
  {$ENDIF}

  {$IFDEF TEEPRO}
  {$IFDEF FMX}
  FMXTee.Series.OHLC, FMXTee.Series.Candle, FMXTee.Series.Surface,
  {$ELSE}
  VCLTee.OHLChart, VCLTee.CandleCh, VCLTee.TeeSurfa,
  {$ENDIF}
  {$ENDIF}

  BI.Data, BI.Arrays, BI.Summary, BI.DataSource;

const
  WhiteColor={$IFDEF FMX}TAlphaColors.White{$ELSE}clWhite{$ENDIF};

type
  {$IFDEF TEEPRO}
  TCustom3DSeriesClass=class of TCustom3DSeries;
  {$ENDIF}

  TBIChartDirection=(Automatic, Rows, Columns);

  TBITChart=class(TChart)
  private
    procedure ClearTitles;

    procedure CreateMulti2D(const X,Y,Z:TDataItem; const ADirection:TBIChartDirection);

    {$IFDEF TEEPRO}
    function CreateFinancial(const ADate,AOpen,AClose,AHigh,ALow,
                             AVolume:TDataItem;
                             const AReverseDate:Boolean):TOHLCSeries;
    procedure CreateGrid3D(const X,Y,Z:TDataItem; const ADirection:TBIChartDirection);
    function Create3DSeries:TCustom3DSeries;
    procedure CreateGridTable(const AData:TDataArray; const ADirection:TBIChartDirection);
    procedure CreateXYZ(const X,Y,Z:TDataItem);
    {$ENDIF}

    procedure FinishXYZ;
    class function GetDateTime(const AData:TDataItem; const Index:TInteger; const Reverse:Boolean):TDateTime;
    class function GetValue(const AData:TDataItem; const Index:TInteger):TChartValue; static;
    procedure Init;
    function InitCountSeries(const ACount:TInteger):TChartSeries;
    function NewSeries(const AClass:TChartSeriesClass):TChartSeries; overload;
    function NewSeries(const Count:Integer):TChartSeries; overload;
    function NewSeries(const X,Y:String):TChartSeries; overload;
    procedure TryAddUniqueTool(const AClass:TTeeCustomToolClass; const AName:String);
    procedure TryDisableTool(const AClass:TTeeCustomToolClass; const ADisable:Boolean);
  protected
    FSeries2D : TChartSeriesClass;

    {$IFDEF TEEPRO}
    FSeries3D : TCustom3DSeriesClass;
    {$ENDIF}

    DefaultXYSeries : TChartSeriesClass;

    LinePointer : Boolean;

    procedure ReadState(Reader: TReader); override;
  public
    Constructor Create(AOwner:TComponent); override;

    procedure Fill(const AData:TDataArray; const ASeries: TDataItem); overload;
    function Fill(const AData:TInt32Array):TChartSeries; overload;
    function Fill(const AData:TInt64Array):TChartSeries; overload;
    function Fill(const AData:TDoubleArray):TChartSeries; overload;
    function Fill(const AData:TDataItem):TChartSeries; overload;
    procedure Fill(const AData:TDataSet; const ValueField:Integer; const TextField:Integer=-1); overload;

    function FillXY(const X,Y:TField):TChartSeries; overload;
    function FillXY(const AData:TDataSet; const X,Y:Integer):TChartSeries; overload;

    Function GetParentComponent: TComponent; override;
    Function HasParent:Boolean; override;
//    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    procedure SetParentComponent(AParent: TComponent); override;
  published
    property Align default TUICommon.AlignClient;
    property BevelOuter default bvNone;
    property Color default WhiteColor;
    property View3D default False;
  end;

  TBIChartItems=class(TPersistent)
  private
    IX,
    IZ,
    IText,
    IGroup,
    IColors : String;

    IY : TTextArray;

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
  TBIChartStacked=(Automatic, No, Yes, Stacked100, Side, SideAll, SelfStack);
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
    function GetSeries2D: TChartSeriesClass;
    function GetSeries3D: TChartSeriesClass;
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

    procedure AddXY(const AItems:TBIChartItems);

    procedure ApplyData(const AData:TDataItem);
    procedure CreateChart;
    function CreateSeries(const X,Y:TDataItem):TChartSeries;
    function DirectChart:TBITChart;
    procedure FillSeries(const ASeries:TChartSeries; const X,Y,AText:TDataItem);
    function GetChart:TBITChart;
    function GuessRealData(const AData:TDataArray):TDataArray;
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
  published
    property Chart:TBITChart read GetChart;

    {$IFNDEF FMX}
    property Height default 250;
    property Width default 400;
    {$ENDIF}

    property Options:TBIChartOptions read FOptions write SetOptions;
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

    class function From(const AData:TDataItem;
                        const AOwner:TComponent;
                        const AClass:TChartSeriesClass=nil):TChartSeries; overload; static;

    class function From(const ASeries:TChartSeries):TDataItem; overload; static;
    class function From(const AChart:TCustomChart):TDataItem; overload; static;
    class function From(const ASeries:Array of TChartSeries):TDataItem; overload; static;
  end;

implementation
