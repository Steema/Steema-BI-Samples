{*********************************************}
{  TeeBI Software Library                     }
{  TeeChart Visualizer Control                }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Visualizer.Chart;
{$DEFINE FMX}
{$SCOPEDENUMS ON}

interface

uses
  System.Classes, System.Types,

  {$IFNDEF FPC}
  System.UITypes,
  {$ENDIF}

  BI.Arrays, BI.Data, BI.DataSource,

  {$IFDEF FMX}
  FMX.ListBox, FMX.StdCtrls, FMX.Controls, FMX.Platform, FMX.Types,
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
  FMXTee.Canvas, FMXTee.Engine, FMXTee.Chart,

  {$IFDEF TEEPRO}
  FMXTee.Tools.SubChart,
  {$ENDIF}

  BI.FMX.Visualizer, BI.FMX.Chart
  {$ELSE}

  VCL.Graphics, VCL.StdCtrls, VCL.Buttons,

  VCLTee.TeCanvas, VCLTee.TeEngine, VCLTee.Chart,

  {$IFDEF TEEPRO}
  VCLTee.TeeSubChart,
  {$ENDIF}

  BI.VCL.Visualizer, BI.VCL.Chart
  {$ENDIF}
  ;

type
  TGroupChart=class;

  TBIMultiAxis=(Automatic,Single,Two,Multiple);

  TGroupChartOptions=class(TPersistent)
  private
    FMultiAxes : TBIMultiAxis;
    FRender   : TCanvas3DClass;
    FSettings : Boolean;
    FTemplate : TChart;

    IParent : TGroupChart;

    function GetLegend: Boolean;
    function GetMarks: Boolean;
    function GetTemplate:TChart;
    procedure SetLegend(const Value: Boolean);
    procedure SetMarks(const Value: Boolean);
    procedure SetMultiAxes(const Value: TBIMultiAxis);
    procedure SetRender(const Value:TCanvas3DClass);
    procedure SetSettings(const Value: Boolean);
    procedure SetTemplate(const Value: TChart);
  public
    Constructor Create;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
  published
    property Settings:Boolean read FSettings write SetSettings default True;
    property Legend:Boolean read GetLegend write SetLegend default True;
    property Marks:Boolean read GetMarks write SetMarks;
    property MultiAxes:TBIMultiAxis read FMultiAxes write SetMultiAxes default TBIMultiAxis.Automatic;
    property Render:TCanvas3DClass read FRender write SetRender;
    property Template:TChart read GetTemplate write SetTemplate;
  end;

  TGroupChart=class(TGroup)
  private
    FChart : TBIChart; //TCustomChart;
    FNext : TGroup;
    FOptions : TGroupChartOptions;

    IParent : TGroupChart;

    procedure ClickedSettings(Sender: TObject);
    procedure EnteredChart(Sender:TObject);
    function FirstZSeries:TChartSeries;
    class procedure InitChart(const AChart:TChart); static;
    procedure LeavedChart(Sender:TObject);
    function NewChart:TBIChart;
    procedure ShowSettingsButton(const Sender:TObject; const AShow:Boolean);
    procedure TryMultipleAxes(const AMulti:TBIMultiAxis);
    procedure SetCanvas(const AChart:TCustomChart);
  protected
    function AddItem:TComponent; virtual;
    procedure ApplyTemplate(const AChart,ATemplate:TCustomChart); overload; virtual;

    procedure ChartResized(Sender:TObject); virtual;

    procedure Init; override;
    procedure Finished; override;

    class procedure SetAxisTitle(const ASeries:TChartSeries; const AData:TDataItem); static;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    Destructor Destroy; override;

    procedure Add(const AIndex:TInteger); override;
    procedure ApplyTemplate; overload;

    procedure Assign(Source:TPersistent); override;

    property Chart:TBIChart read FChart;
    property Group:TGroupChart read IParent;
  published
    property Options:TGroupChartOptions read FOptions; // write SetOptions
  end;

  TGroupSeries=class;

  TAutoStackSeries=(Automatic,Yes,Yes100,No);

  TGroupSeriesStyle=(Automatic,Series2D,Series3D,Geographic);

  TGroupSeriesOptions=class(TPersistent)
  private
    FAddNulls : Boolean;
    FAutoStack : TAutoStackSeries;
    FSeries2D : TChartSeriesClass;
    FSeries3D : TChartSeriesClass;
    FStyle : TGroupSeriesStyle;

    IParent : TGroupSeries;

    function NewSeries(const AName:String; const AOptions:TGroupChartOptions):TChartSeries;
    procedure SetAutoStack(const Value: TAutoStackSeries);
    procedure SetSeries2D(const Value: TChartSeriesClass);
    procedure SetSeries3D(const Value: TChartSeriesClass);
    procedure SetStyle(const Value: TGroupSeriesStyle);
    procedure SetAddNulls(const Value: Boolean);
  public
    Constructor Create(const AParent:TGroupSeries);

    procedure Assign(Source:TPersistent); override;
  published
    property AddNulls:Boolean read FAddNulls write SetAddNulls default True;
    property AutoStack:TAutoStackSeries read FAutoStack write SetAutoStack default TAutoStackSeries.Automatic;

    property Series2D:TChartSeriesClass read FSeries2D write SetSeries2D;
    property Series3D:TChartSeriesClass read FSeries3D write SetSeries3D;

    property Style:TGroupSeriesStyle read FStyle write SetStyle default TGroupSeriesStyle.Automatic;
  end;

  TGroupSeries=class(TGroupChart)
  private
    type
      TSaveParams=record
        Component:TComponent;
        Values:TVisualizerItems;
        Group:TVisualizerItem;
        Rows:TCursorIndex;
      end;

    var
    FSeriesOptions : TGroupSeriesOptions;

    SingleSeries : TChartSeries;

    ISave : TSaveParams;

    function AddSeries(const AName:String):TChartSeries;
    function AddSingleSeries:TChartSeries;
    procedure CheckAutoStack;
    procedure DisableStack(const ASeries:TChartSeries);
    procedure EnableStack(const AStack:TAutoStackSeries; const AChart:TCustomChart; const ASeries:TChartSeries);
    function GetParentDataString:String;
    function MultipleNormalColumns(const Values:TVisualizerItems):Boolean;
    procedure Reset;
    procedure SetOptions(const Value: TGroupSeriesOptions);
    procedure TrySetTitles(const AGroup:TVisualizerItem; const Values:TVisualizerItems);
  protected
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;
    Destructor Destroy; override;

    procedure AddValues(const AComponent:TComponent;
                        const Values:TVisualizerItems;
                        const AGroup:TVisualizerItem;
                        const ARows:TCursorIndex); override;

    procedure Assign(Source:TPersistent); override;

    class function BestControl(const AIndex,ATotal,AValues:Integer):TGroupClass; override;
  published
    property Options:TGroupSeriesOptions read FSeriesOptions write SetOptions;
  end;

  {$IFDEF TEEPRO}
  // Uses TSubChart tool items, one for each item in the group dimension
  TGroupSubChart=class(TGroupChart)
  private
    FColumns : Integer;
    FSameAxisRange : Boolean;
    FTool : TSubChartTool;

    procedure HideChartParts;
    procedure RecalcAxisRange;
    procedure SetColumns(const Value: Integer);
    procedure SetSameAxisRange(const Value: Boolean);
  protected
    function AddItem:TComponent; override;
    procedure ApplyTemplate(const AChart,ATemplate:TCustomChart); override;
    procedure ChartResized(Sender:TObject); override;
    procedure Finished; override;
  public
    Constructor CreateData(const AItem:TVisualizerItem; const AParent:TGroup); override;

    procedure Assign(Source:TPersistent); override;

    property SubChart:TSubChartTool read FTool;
  published
    property Columns:Integer read FColumns write SetColumns default 0;
    property SameAxisRange:Boolean read FSameAxisRange write SetSameAxisRange default True;
  end;
  {$ENDIF}

  {
  // Uses a TChartScroller Tool as a way to group a dimension
  TGroupChartScroller=class(TGroup)
  public
    Constructor Create(const AParent:TWinControl); override;
    function Add(const AName:String):TComponent; override;
  end;
  }

  TBIChartComposerUI=record
  public
    Viz : TBIComposer;

    class function CurrentSeriesClass(const AItems:TComboBox):TChartSeriesClass; static;
    class procedure Fill2DSeries(const AItems:TStrings); static;
    class procedure Fill3DSeries(const AItems:TStrings); static;

    class function FindSeries(const AItems:TStrings; const AClass:TChartSeriesClass):Integer; static;

    function GetChart(const AIndex:Integer):TGroupChart;
    function GetSeries(const AIndex:Integer):TGroupSeries;

    {$IFDEF TEEPRO}
    function GetSubChart(const AIndex:Integer):TGroupSubChart;
    {$ENDIF}
  end;

implementation
