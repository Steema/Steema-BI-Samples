{*********************************************}
{  TeeBI Software Library                     }
{  TeeChart Visualizer Control                }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Visualizer.Chart;
{.$DEFINE FMX}
{$SCOPEDENUMS ON}

interface

uses
  System.Classes, System.Types,

  {$IFNDEF FPC}
  System.UITypes,
  {$ENDIF}

  BI.Arrays, BI.DataItem, BI.DataSource,

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

  FMXBI.Visualizer, FMXBI.Chart
  {$ELSE}

  VCL.Graphics, VCL.StdCtrls, VCL.Buttons,

  VCLTee.TeCanvas, VCLTee.TeEngine, VCLTee.Chart,

  {$IFDEF TEEPRO}
  VCLTee.TeeSubChart,
  {$ENDIF}

  VCLBI.Visualizer, VCLBI.Chart
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

    // Cannot be published ( C++ limitation )
    property Series2D:TChartSeriesClass read FSeries2D write SetSeries2D;
    property Series3D:TChartSeriesClass read FSeries3D write SetSeries3D;

  published
    property AddNulls:Boolean read FAddNulls write SetAddNulls default True;
    property AutoStack:TAutoStackSeries read FAutoStack write SetAutoStack default TAutoStackSeries.Automatic;
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

uses
  {$IFDEF FMX}

  FMXBI.Chart.Plugin,

  {$IFDEF TEEPRO}
  FMXTee.Themes, FMXTee.Series.Surface, FMXTee.Editor.Pro,
  FMXTee.Series.Donut, FMXTee.Series.Polar, FMXTee.Series.PolarGrid,
  FMXTee.Series.Point3D, FMXTee.Series.Map, FMXTee.Series.World,
  {$ENDIF}

  FMXTee.Series, FMXTee.Procs, FMXTee.Editor.Chart

  {$ELSE}

  {$IFDEF MSWINDOWS}
  WinAPI.Windows,
  {$ENDIF}

  VCL.Controls, VCL.ExtCtrls,
  VCLBI.Editor.Chart,

  VCLBI.Chart.Plugin,

  {$IFDEF TEEPRO}
  VCLTee.TeeThemes,

  VCLTee.TeeSurfa, VCLTee.TeeSurfEdit, VCLTee.TeeTowerEdit,
  VCLTee.TeeDonut, VCLTee.TeePolar, VCLTee.TeePolarGrid,
  VCLTee.TeePoin3,

  {$IFNDEF FPC}
  VCLTee.TeeGLCanvas, VCLTee.TeeGLEditor,
  {$ENDIF}

  VCLTee.TeeWorldSeries, VCLTee.TeeMapSeries, VCLTee.TeeWorldSeriesEditor,
  {$ENDIF}

  VCLTee.EditChar,

  VCLTee.Series,

  {$IFDEF MSWINDOWS}
  VCLTee.TeeGDIPlus,
  {$ENDIF}

  VCLTee.TeeProcs
  {$ENDIF}
  ;

{ TGroupChart }

type
  TChartProc={$IFNDEF FPC}reference to{$ENDIF} procedure(const AChart:TCustomChart);

procedure TraverseChart(const AChart:TCustomChart; const AProc:TChartProc);
{$IFDEF TEEPRO}
var tmpTool : TTeeCustomTool;
    t : Integer;
{$ENDIF}
begin
  AProc(AChart);

  {$IFDEF TEEPRO}
  for tmpTool in AChart.Tools do
      if tmpTool is TSubChartTool then
         for t:=0 to TSubChartTool(tmpTool).Charts.Count-1 do
             AProc(TSubChartTool(tmpTool).Charts[t].Chart);
  {$ENDIF}
end;

Constructor TGroupChart.CreateData(const AItem:TVisualizerItem; const AParent:TGroup);
begin
  inherited;

  if AParent is TGroupChart then
  begin
    IParent:=TGroupChart(AParent);
    FChart:=IParent.FChart;
    FOptions:=IParent.FOptions;
  end
  else
  begin
    IParent:=Self;

    FOptions:=TGroupChartOptions.Create;
    FOptions.IParent:=Self;

    FChart:=NewChart;

    if (Parent<>nil) and (Parent.Data<>nil) then
       FChart.Chart.Title.Caption:=Parent.MapToString;

    FChart.OnResize:=ChartResized;
  end;

  Control:=FChart;
end;

Destructor TGroupChart.Destroy;
begin
  if IParent=Self then
     FOptions.Free;

  inherited;
end;

procedure TGroupChart.EnteredChart(Sender: TObject);
begin
  if Options.FSettings then
     ShowSettingsButton(Sender,True);
end;

procedure TGroupChart.ApplyTemplate;
begin
  if FOptions.FTemplate<>nil then
     {$IFNDEF FPC}
     Traverse<TGroupChart>(procedure(const AGroup:TGroupChart)
     begin
       TraverseChart(AGroup.Chart.Chart,procedure(const AChart:TCustomChart)
       begin
         AGroup.ApplyTemplate(AChart,FOptions.FTemplate);
       end);
     end);
     {$ENDIF}
end;

procedure TGroupChart.Assign(Source: TPersistent);
begin
  if Source is TGroupChart then
     FOptions.Assign(TGroupChart(Source).FOptions)
  else
     inherited;
end;

procedure TGroupChart.ChartResized(Sender: TObject);
begin
  if Options.Legend then
     FChart.Chart.Legend.Visible:=FChart.Width>Round(1.5*FChart.Chart.Legend.Width);
end;

procedure TGroupChart.ClickedSettings(Sender: TObject);
var tmp : TCustomChart;
    tmpButton : TSpeedButton;
begin
  if Sender is TSpeedButton then
  begin
    tmpButton:=TSpeedButton(Sender);

    if tmpButton.Parent is TCustomChart then
    begin
      tmp:=TCustomChart(tmpButton.Parent);

      {$IFDEF FMX}
      TChartEditForm.Edit(tmp.Owner,tmp);
      {$ELSE}

      TBIChartEditor.Edit(Self,tmp.Parent as TBIChart);
      //EditChart(tmp.Owner,tmp);
      {$ENDIF}
    end;
  end;
end;

{$IFDEF FMX}
type
  TWinControl=TControl;
{$ENDIF}

function AddSettingsButton(const AControl:TWinControl):TSpeedButton;
begin
  result:=TSpeedButton.Create(AControl);

  {$IFDEF FMX}
  result.Position.X:=4;
  result.Position.Y:=4;
  result.Text:='*';
  {$ELSE}
  result.Flat:=True;
  result.Left:=4;
  result.Top:=4;

  result.Caption:='*'; // <-- pending to replace with bitmap
  {$ENDIF}

  result.Cursor:=crHandPoint;

  result.Visible:=False;

  result.Parent:=AControl;
end;

procedure TGroupChart.ApplyTemplate(const AChart,ATemplate:TCustomChart);
begin
  {$IFDEF TEEPRO}
  TThemesList.Apply(AChart,ATemplate);
  {$ENDIF}

  // Properties not applied by Themes:
  AChart.View3D:=ATemplate.View3D;
  AChart.Legend.Alignment:=ATemplate.Legend.Alignment;
end;

procedure TGroupChart.SetCanvas(const AChart:TCustomChart);
var tmpCanvas : TCanvas3DClass;
begin
  tmpCanvas:=FOptions.Render;

  //if tmpCanvas=nil then
   //  tmpCanvas:=GlobalOptions.Render;

  if tmpCanvas=nil then
  begin
    {$IFNDEF FMX}
    {$IFDEF MSWINDOWS}
    if not (AChart.Canvas is TGDIPlusCanvas) then
       AChart.Canvas:=TGDIPlusCanvas.Create;
    {$ENDIF}
    {$ENDIF}
  end
  else
  begin
    AChart.Canvas:=tmpCanvas.Create;

    {$IFNDEF FMX}
    {$IFNDEF FPC}
    {$IFDEF TEEPRO}
    if tmpCanvas=TGLCanvas then
    begin
      AChart.View3DOptions.Orthogonal:=False;
      AChart.View3DOptions.Zoom:=75;

      // In 3D mode, better enable right-mouse button to pan the whole chart
      // instead of the axis contents.
      //AChart.AllowPanning:=TPanningMode.pmBoth;

      AChart.ScrollMouseButton:=TMouseButton.mbRight;
      AChart.ZoomWheel:=TMouseWheelStyle.pmwNormal;
    end;
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
  end;

  // Experiment:
  //if AChart.Canvas is TGDIPlusCanvas then
  //   TGDIPlusCanvas(AChart.Canvas).Scale:=1.4;
end;

function TGroupChart.NewChart:TBIChart;
begin
  result:=TBIChart.Create(Self);

  if FOptions.FTemplate=nil then
     InitChart(result.Chart)
  else
     ApplyTemplate(result.Chart,FOptions.FTemplate);

  SetCanvas(result.Chart);

  if FOptions.FSettings then
     AddSettingsButton(result.Chart).OnClick:=ClickedSettings;

  {$IFNDEF FPC} // ????
  result.Chart.OnMouseEnter:=EnteredChart;
  result.Chart.OnMouseLeave:=LeavedChart;
  {$ENDIF}
end;

class procedure TGroupChart.InitChart(const AChart:TChart);
var tmp : TChartAxis;
begin
  AChart.Title.Font.Size:=12;

  AChart.BevelOuter:=bvNone;
  AChart.Color:=WhiteColor;

  AChart.ColorPaletteIndex:=9;
  AChart.View3D:=False;

  // Disable mouse and wheel:
  AChart.Zoom.Allow:=False;

  {$IFDEF TEEPRO}
  AChart.Panning.MouseWheel:=TMouseWheelStyle.pmwNone;
  AChart.ZoomWheel:=TMouseWheelStyle.pmwNone;
  {$ENDIF}

  AChart.AllowPanning:=TPanningMode.pmNone;

  for tmp in AChart.Axes do
      tmp.Title.Font.Style:=[TFontStyle.fsBold];
end;

procedure TGroupChart.LeavedChart(Sender: TObject);
begin
  ShowSettingsButton(Sender,False);
end;

function NotMandatoryAxis(const ASeries:TChartSeries):TChartAxis;
begin
  if ASeries.YMandatory then
     result:=ASeries.GetHorizAxis
  else
     result:=ASeries.GetVertAxis;
end;

function MandatoryAxis(const ASeries:TChartSeries):TChartAxis;
begin
  if ASeries.YMandatory then
     result:=ASeries.GetVertAxis
  else
     result:=ASeries.GetHorizAxis;
end;

procedure TGroupChart.Init;
begin
  inherited;
  Chart.Chart.AutoRepaint:=False;
end;

procedure SetColorEach(const ASeries:TChartSeries);
begin
  if ASeries.HasZValues then
     ASeries.ColorEachPoint:=False
  else
  if ASeries.Count>1 then
     if not ASeries.DrawBetweenPoints then
        ASeries.ColorEachPoint:=True;
end;

{$IFDEF TEEPRO}
type
  TCustomStackSeriesAccess=class(TCustomStackSeries);
{$ENDIF}

procedure TGroupChart.Finished;

  function ShouldView3D(const ASeries:TChartSeries):Boolean;
  begin
    result:=ASeries.HasZValues;

    {$IFDEF TEEPRO}
    if result then
    begin
      result:=(not (ASeries is TColorGridSeries)) and
              (not (ASeries is TContourSeries)) and
              (not (ASeries is TWorldSeries));
    end;
    {$ENDIF}
  end;

  procedure FinishChart(const AParent:TGroup; const Chart:TCustomChart);

    {$IFDEF TEEPRO}
    procedure CheckAreaTransparency;
    var tmpSeries : TChartSeries;
    begin
      // Semi-transparency for non-stacked Area series, for better visibility:
      if Chart[0] is TAreaSeries then
         if TAreaSeries(Chart[0]).MultiArea=TMultiArea.maNone then
         begin
           for tmpSeries in Chart.SeriesList do
               if tmpSeries is TAreaSeries then
                  tmpSeries.Transparency:=50;
         end;
    end;
    {$ENDIF}

  var tmp : TChartSeries;
  begin
    if Chart.SeriesCount=1 then
    begin
      tmp:=Chart[0];

      SetColorEach(tmp);

      if (not tmp.HasZValues) and (tmp.Count<=1) then
         if NotMandatoryAxis(tmp).Title.Caption='' then
            NotMandatoryAxis(tmp).Hide;

      {
      MandatoryAxis(tmp).Title.Caption:=tmp.Title;
      Chart.Legend.Title.Caption:=tmp.Title;
      }
    end
    else
    begin
      if Chart.SeriesCount>0 then
      begin
        {$IFDEF TEEPRO}
        CheckAreaTransparency;
        {$ENDIF}

        if Chart.Legend.Visible and (Chart.Legend.Title.Caption<>'') then
        begin
          // Try to wordwrap Legend Title if its much wider than Legend items
        end;
      end;
    end;
  end;

begin
  {$IFNDEF FMX}
  if Chart.Parent is TPanel then
     Chart.Chart.Title.Caption:=Chart.Chart.Title.Caption+' '+TPanel(Chart.Parent).Caption;
  {$ENDIF}

  if FNext is TGroupSeries then
     TGroupSeries(FNext).CheckAutoStack;

  FinishChart(Parent,Chart.Chart);

  TryMultipleAxes(Options.MultiAxes);

  {
  if Chart.SeriesCount=1 then
  begin
    if Data<>nil then
       Chart.Title.Caption:=Data.Name;
  end
  else
  }
  if Data<>nil then
     Chart.Chart.Legend.Title.Caption:=Data.Name;

  if Chart.Chart.SeriesCount>0 then
    if ShouldView3D(Chart.Chart[0]) then
    begin
      Chart.Chart.View3D:=True;
      Chart.Chart.Axes.Depth.Visible:=True;
      Chart.Chart.Axes.Bottom.Increment:=1;
    end;

  Chart.Chart.AutoRepaint:=True;
  Chart.Chart.Invalidate;

  inherited;
end;

function TGroupChart.FirstZSeries:TChartSeries;
var tmp : TChartSeries;
begin
  result:=nil;

  if Chart.Chart.SeriesCount=1 then
  begin
    tmp:=Chart.Chart[0];

    if tmp.HasZValues then
    begin
      if tmp.Title<>'' then
         tmp.Title:='';

      result:=tmp;
    end;
  end;
end;

function TGroupChart.AddItem:TComponent;
begin
  result:=FirstZSeries;
end;

type
  TComposerAccess=class(TBIComposer);

procedure TGroupChart.Add(const AIndex:TInteger);
var tmp : TComponent;
begin
  inherited;

  tmp:=AddItem;

  FItems[AIndex].Control:=tmp;
  FNext:=TComposerAccess(FVisualizer).AddGroup(Self,FItems[AIndex].Next,tmp,FItems[AIndex].Rows);
end;

class procedure TGroupChart.SetAxisTitle(const ASeries:TChartSeries;
   const AData:TDataItem);
begin
  MandatoryAxis(ASeries).Title.Caption:=AData.Name;
end;

{$DEFINE D17}

{$IFDEF FMX}
function TeeBIGetCursorPos:TPoint;
{$IFDEF D17}
var MouseService : IFMXMouseService;
{$ENDIF}
Begin
  {$IFDEF D17}
  if TPlatformServices.Current.SupportsPlatformService(IFMXMouseService, IInterface(MouseService)) then
     result:=MouseService.GetMousePos
  else
     result:=TeeZeroPoint;
  {$ELSE}
  result:=Platform.GetMousePos;
  {$ENDIF}
end;
{$ENDIF}


procedure TGroupChart.ShowSettingsButton(const Sender: TObject; const AShow: Boolean);

  function HasCursor(const AButton:TSpeedButton):Boolean;
  var P : {$IFDEF FMX}System.Types.TPointF{$ELSE}TPoint{$ENDIF};
  begin
    {$IFDEF FMX}
    P:=TeeBIGetCursorPos;
    result:=AButton.BoundsRect.Contains(P);

    {$ELSE}
    {$IFDEF MSWINDOWS}
    result:=GetCursorPos(P);
    {$ELSE}
    result:=True;
    P:=Mouse.CursorPos;
    {$ENDIF}
    {$ENDIF}

    if result then
    begin
      {$IFDEF FPC}
      result:=PointInRect(AButton.BoundsRect,AButton.ScreenToClient(P));
      {$ELSE}

      {$IFNDEF HASFMX20}
      {$IFNDEF FMX}
      P:=AButton.ScreenToClient(P);
      {$ENDIF}
      {$ENDIF}

      result:=AButton.BoundsRect.Contains(P);
      {$ENDIF}
    end;
  end;

var tmp : TWinControl;
    tmpButton : TSpeedButton;
begin
  if Sender is TWinControl then
  begin
    tmp:=TWinControl(Sender);

    if tmp.{$IFDEF FMX}Controls.Count{$ELSE}ControlCount{$ENDIF}>0 then
       if tmp.Controls[0] is TSpeedButton then
       begin
         tmpButton:=TSpeedButton(tmp.Controls[0]);

         if (tmpButton.Visible<>AShow) and (not HasCursor(tmpButton)) then
            tmpButton.Visible:=AShow;
       end;
  end;
end;

procedure TGroupChart.TryMultipleAxes(const AMulti:TBIMultiAxis);
var tmpMulti : TBIMultiAxis;
begin
  if AMulti<>TBIMultiAxis.Single then
  begin
    if Chart.Chart.SeriesCount>1 then
    begin
      tmpMulti:=AMulti;

      {
      if AMulti=TBIMultiAxis.Automatic then
         if Chart.SeriesCount>2 then
            tmpMulti:=TBIMultiAxis.Multiple
         else
            tmpMulti:=TBIMultiAxis.Two;
      }

      if tmpMulti=TBIMultiAxis.Multiple then
         Chart.SplitAxes(Chart.Chart[0].YMandatory)
      else
      if tmpMulti=TBIMultiAxis.Two then
         Chart.SetTwoAxes;
    end;
  end;
end;

{ TGroupSeries }

Constructor TGroupSeries.CreateData(const AItem:TVisualizerItem; const AParent: TGroup);
begin
  inherited;
  CanAddValues:=True;

  {$IFDEF TEEPRO}
  if AParent is TGroupSubChart then
  begin
    FChart:=AParent.Control as TBIChart;
    Control:=FChart;
  end;
  {$ENDIF}

  FSeriesOptions:=TGroupSeriesOptions.Create(Self);
end;

procedure TGroupSeries.CheckAutoStack;

  function CanAutoStack(const AChart:TCustomChart):Boolean;
  begin
    result:=(AChart.SeriesCount>0) and
            (
              (AChart[0] is TCustomSeries) or
              (AChart[0] is TCustomBarSeries)
            );
  end;

  function TotalPoints:Integer;
  var tmp : TChartSeries;
  begin
    result:=0;

    for tmp in Chart.Chart.SeriesList do
        Inc(result,tmp.Count);
  end;

var tmp : TAutoStackSeries;
    tmpSeries : TChartSeries;
begin
  tmp:=FSeriesOptions.AutoStack;

  if CanAutoStack(Chart.Chart) then
  begin
    tmpSeries:=Chart.Chart[0];

    if tmp<>TAutoStackSeries.No then
    begin
      if (tmp=TAutoStackSeries.Yes) or
         (tmp=TAutoStackSeries.Yes100) or
         (TotalPoints>32) or
         ( (tmp=TAutoStackSeries.Automatic) and (tmpSeries is TAreaSeries) ) then
            EnableStack(tmp,Chart.Chart,tmpSeries);
    end
    else
      DisableStack(tmpSeries);
  end;
end;

procedure TGroupSeries.EnableStack(const AStack:TAutoStackSeries;
                                   const AChart:TCustomChart;
                                   const ASeries:TChartSeries);
var tmp : TChartSeries;
begin
  if ASeries is TCustomBarSeries then
  begin
    if AChart.SeriesCount=1 then
       TCustomBarSeries(ASeries).MultiBar:=TMultiBar.mbSelfStack
    else
    if AStack=TAutoStackSeries.Yes100 then
       TCustomBarSeries(ASeries).MultiBar:=TMultiBar.mbStacked100
    else
       TCustomBarSeries(ASeries).MultiBar:=TMultiBar.mbStacked
  end
  else
  if ASeries is TAreaSeries then
     if AStack=TAutoStackSeries.Yes100 then
        TAreaSeries(ASeries).MultiArea:=TMultiArea.maStacked100
     else
        TAreaSeries(ASeries).MultiArea:=TMultiArea.maStacked
  else
  if ASeries is TCustomSeries then
  begin
    {$IFDEF TEEPRO}
    if AStack=TAutoStackSeries.Yes100 then
       TCustomStackSeriesAccess(ASeries).Stacked:=TCustomSeriesStack.cssStack100
    else
       TCustomStackSeriesAccess(ASeries).Stacked:=TCustomSeriesStack.cssStack;
    {$ENDIF}
  end;

  for tmp in AChart.SeriesList do
      tmp.Marks.Hide;
end;

procedure TGroupSeries.DisableStack(const ASeries:TChartSeries);
var tmp : TChartSeries;
begin
  if ASeries is TCustomBarSeries then
     TCustomBarSeries(ASeries).MultiBar:=TMultiBar.mbSide
  else
  if ASeries is TAreaSeries then
     TAreaSeries(ASeries).MultiArea:=TMultiArea.maNone
  else
  if ASeries is TCustomSeries then
  begin
    {$IFDEF TEEPRO}
    TCustomStackSeriesAccess(ASeries).Stacked:=TCustomSeriesStack.cssNone;
    {$ENDIF}
  end;

  if IParent.Options.Marks then
     for tmp in Chart.Chart.SeriesList do
         tmp.Marks.Show;
end;

type
  TGroupAccess=class(TGroup);

function TGroupSeries.GetParentDataString:String;
begin
  if (Parent<>nil) and (Parent.Data<>nil) then
     result:=TGroupAccess(Parent).GetDataString
  else
     result:='';
end;

type
  TSeriesData=record
  public
    AddNulls : Boolean;

    Series : TChartSeries;
    Data : TDataItem;

    procedure AddPoint(const APos:TInteger; const AText:String; const XPos,ZPos:Integer);
  end;

procedure TSeriesData.AddPoint(const APos:TInteger; const AText:String; const XPos,ZPos:Integer);
{$IFDEF TEEPRO}
var tmp3D : TCustom3DSeries;
    tmpX,
    tmpZ : Integer;
    tmpPoly : TTeePolygon;
{$ENDIF}
begin
  {$IFDEF TEEPRO}
  if Series is TWorldSeries then
  begin
    if not Data.Missing[APos] then
    begin
      tmpPoly:=TWorldSeries(Series).Shapes.ByName[AText];

      if tmpPoly=nil then
         raise EBIException.Create('Error Geo entity not found: '+AText)
      else
         tmpPoly.Z:=TBITChart.GetValue(Data,APos);
    end;
  end
  else
  if Series.HasZValues then
  begin
    tmp3D:=TCustom3DSeries(Series);

    tmpX:=XPos;
    tmpZ:=ZPos;

    if Data.Missing[APos] then
    begin
      if AddNulls then
         tmp3D.AddXYZ(tmpX,0,tmpZ,AText,clNone)
    end
    else
       case Data.Kind of
         dkInt32: tmp3D.AddXYZ(tmpX,Data.Int32Data[APos],tmpZ,AText,clTeeColor);
         dkInt64: tmp3D.AddXYZ(tmpX,Data.Int64Data[APos],tmpZ,AText,clTeeColor);
        dkSingle: tmp3D.AddXYZ(tmpX,Data.SingleData[APos],tmpZ,AText,clTeeColor);
        dkDouble: tmp3D.AddXYZ(tmpX,Data.DoubleData[APos],tmpZ,AText,clTeeColor);
      dkExtended: tmp3D.AddXYZ(tmpX,Data.ExtendedData[APos],tmpZ,AText,clTeeColor);
          dkText: ;
      dkDateTime: tmp3D.AddXYZ(tmpX,Data.DateTimeData[APos],tmpZ,AText,clTeeColor);
       end;
  end
  else
  {$ENDIF}
  begin
    if Data.Missing[APos] then
    begin
      if AddNulls then
         Series.AddNull(AText)
    end
    else
       case Data.Kind of
         dkInt32: Series.Add(Data.Int32Data[APos],AText);
         dkInt64: Series.Add(Data.Int64Data[APos],AText);
        dkSingle: Series.Add(Data.SingleData[APos],AText);
        dkDouble: Series.Add(Data.DoubleData[APos],AText);
      dkExtended: Series.Add(Data.ExtendedData[APos],AText);
          dkText: ;
      dkDateTime: Series.Add(Data.DateTimeData[APos],AText);
       end;
  end;
end;

function IsNumericOrDateTime(const AData:TDataItem):Boolean;
begin
  result:=AData.Kind.IsNumeric or (AData.Kind=TDataKind.dkDateTime);
end;

procedure TryChangeSeriesStyle(var ASeries:TChartSeries);
var tmpNew : TChartSeriesClass;
begin
  if not ASeries.HasZValues then
  begin
    tmpNew:=TChartSeriesClass(ASeries.ClassType);

    if (ASeries.Count>1000) and (not (ASeries is TFastLineSeries)) then
       tmpNew:=TFastLineSeries
    else
    if (ASeries.Count>32) and
       (not (ASeries is TFastLineSeries)) and
       (not (ASeries is TCustomSeries)) then
       tmpNew:=TLineSeries;

    if tmpNew<>ASeries.ClassType then
    begin
      ChangeSeriesType(ASeries,tmpNew);
      ASeries.Marks.Hide;

      if tmpNew=TFastLineSeries then
         ASeries.ParentChart.AllowPanning:=pmBoth;
    end;
  end;
end;

function TGroupSeries.AddSeries(const AName:String):TChartSeries;
{$IFDEF TEEPRO}
var tmpGeo : Boolean;
{$ENDIF}
begin
  result:=FSeriesOptions.NewSeries(AName,FOptions);
  result.ParentChart:=(Control as TBIChart).Chart;

  {$IFDEF TEEPRO}
  tmpGeo:=(result is TWorldSeries);

  result.ParentChart.Axes.Visible:=not tmpGeo;
  (result.ParentChart as TCustomChart).Walls.Visible:=not tmpGeo;

  if tmpGeo then
  begin
    TWorldSeries(result).Entities.Antarctica.Visible:=False;
    result.Pen.Color:=clDkGray;
    result.Marks.Show;
  end;

  {$ENDIF}
end;

function TGroupSeries.MultipleNormalColumns(const Values:TVisualizerItems):Boolean;
var tmp : TVisualizerItem;
    t,
    tmpCount : Integer;
begin
  tmpCount:=0;

  for t:=0 to Values.Count-1 do
  begin
    tmp:=Values[t];

    if tmp.Enabled then
       if tmp.Data.AsTable or (tmp.Data.Kind=TDataKind.dkUnknown) then
          Exit(False)
       else
          Inc(tmpCount);
  end;

  result:=tmpCount>1;
end;

procedure TGroupSeries.Reset;
var tmp : TChartAxis;
begin
  if ISave.Values<>nil then
  begin
    Chart.Chart.FreeAllSeries;

    Chart.Chart.Legend.Title.Caption:='';

    for tmp in Chart.Chart.Axes do
        tmp.Title.Caption:='';

    SingleSeries:=nil;

    AddValues(ISave.Component,ISave.Values,ISave.Group,ISave.Rows);

    Finished;
  end;
end;

procedure TGroupSeries.SetOptions(const Value: TGroupSeriesOptions);
begin
  FSeriesOptions.Assign(Value);
end;

procedure TGroupSeries.TrySetTitles(const AGroup:TVisualizerItem; const Values:TVisualizerItems);
var tmp : TVisualizerItem;
    tmpS : String;
    t : Integer;
begin
  if FChart<>nil then
     if FChart.Chart.SeriesCount=1 then
     if not MultipleNormalColumns(Values) then
     begin
       for t:=0 to Values.Count-1 do
       begin
         tmp:=Values[t];

         if tmp.Enabled then
         begin
           MandatoryAxis(FChart.Chart[0]).Title.Caption:=tmp.Data.Name;

           if AGroup=nil then
           begin
             tmpS:=GetParentDataString;

             if tmpS<>'' then
                FChart.Chart.Title.Caption:=tmpS;

             if Parent<>nil then
                NotMandatoryAxis(FChart.Chart[0]).Title.Caption:=Parent.Data.Name;
           end
           else
             NotMandatoryAxis(FChart.Chart[0]).Title.Caption:=AGroup.Data.Name;

           break;
         end;
       end;
     end;
end;

function TGroupSeries.AddSingleSeries:TChartSeries;
var tmpName : String;
begin
  if (Parent<>nil) and (Parent.Data<>nil) then
     tmpName:=Parent.MapToString
  else
     tmpName:='';

  result:=AddSeries(tmpName);
end;

procedure TGroupSeries.AddValues(const AComponent: TComponent;
                                 const Values:TVisualizerItems;
                                 const AGroup:TVisualizerItem;
                                 const ARows: TCursorIndex);

  procedure LoopRows(var ASeriesData:TSeriesData);
  var
    tmpGroup : TDataItem;
    tmpX : Integer;
    tmpLabel : String;

    procedure DoLoopRows;

      procedure DoAddPoint(const AIndex:TInteger);
      begin
        if tmpGroup<>nil then
           if not tmpGroup.AsTable then
              tmpLabel:=tmpGroup.DataToString(AIndex);

        ASeriesData.AddPoint(AIndex,tmpLabel,tmpX,AIndex);
      end;

    var t : TLoopInteger;
    begin
      ASeriesData.Data.Load;

      if ASeriesData.Data.Count=1 then
         tmpLabel:=ASeriesData.Data.Name
      else
      if tmpGroup=nil then
         tmpLabel:=GetParentDataString;

      if ARows=nil then
         for t:=0 to ASeriesData.Data.Count-1 do
             DoAddPoint(t)
      else
         for t:=0 to High(ARows) do
             DoAddPoint(ARows[t]);
    end;

  var t : TLoopInteger;
      tmp : TDataItem;
  begin
    if Parent=nil then
       tmpX:=0
    else
       tmpX:=TGroupAccess(Parent).Position;

    ASeriesData.AddNulls:=Options.AddNulls;

    if AGroup=nil then
       tmpGroup:=nil
    else
       tmpGroup:=AGroup.Data;

    if ASeriesData.Data.AsTable then
    begin
      tmp:=ASeriesData.Data;

      while (tmp.Items.Count>0) and tmp.Items[0].AsTable do
           tmp:=tmp.Items[0];

      // Row->Single Column
      if (Parent<>nil) and (Parent.Data=tmp) then
      begin
        ASeriesData.Data:=tmp.Items[tmpX];
        DoLoopRows;
      end
      else
      for t:=0 to tmp.Items.Count-1 do // No Row, just Columns
      begin
        ASeriesData.Data:=tmp.Items[t];

        tmpLabel:=ASeriesData.Data.Name;

        DoLoopRows;

        {
        if ARows=nil then
           ASeriesData.AddPoint(0,ASeriesData.Data.Name,tmpX,t)
        else
           ASeriesData.AddPoint(ARows[0],ASeriesData.Data.Name,tmpX,t);
        }

        if ASeriesData.Data.NumericValues=TNumericData.Percentages then
           ASeriesData.Series.ValueFormat:='0.##%';
      end;
    end
    else // Single Column
      DoLoopRows;
  end;

  procedure AddGroupAsSeries(const AGroup:TDataItem; const AValue:TVisualizerItem);
  var t : Integer;
      tmpSeries : TChartSeries;
      tmp : TDataItem;
      tmpSeriesData : TSeriesData;
  begin
    if AGroup.AsTable then
    begin
      for t:=0 to AGroup.Items.Count-1 do
      begin
        tmp:=AGroup.Items[t];

        if IsNumericOrDateTime(tmp) then
        begin
          tmpSeries:=AddSeries(tmp.Name);

          tmpSeriesData.Data:=tmp;
          tmpSeriesData.Series:=tmpSeries;
          tmpSeriesData.AddNulls:=Options.AddNulls;

          LoopRows(tmpSeriesData);

          TryChangeSeriesStyle(tmpSeries);
        end;
      end;

      if Control is TChart then
      begin
        TChart(Control).Legend.Title.Caption:=AGroup.Name;

        CheckAutoStack;

        //TGroupChart.FinishChart(Parent,Control as TChart);

        // Pending: Try here to add multiple non-mandatory axis sub-axis,
        // with labels for each parent<-parent.. group
      end;
    end;
  end;

var tmp : TVisualizerItem;
    tmpSeries : TChartSeries;
    t : Integer;
    tmpIsSingle : Boolean;
    tmpSeriesData : TSeriesData;
begin
  ISave.Component:=AComponent;
  ISave.Values:=Values;
  ISave.Group:=AGroup;
  ISave.Rows:=ARows;

  // All values are simple columns
  if (AGroup<>nil) and MultipleNormalColumns(Values) then
  begin
    tmpSeries:=nil;

    for t:=0 to Values.Count-1 do
    begin
      tmp:=Values[t];

      if tmp.Enabled then
      begin
        tmp.Data.Load;

        tmpSeries:=AddSeries(tmp.Data.Name);

        tmpSeriesData.AddNulls:=Options.AddNulls;
        tmpSeriesData.Data:=tmp.Data;
        tmpSeriesData.Series:=tmpSeries;

        LoopRows(tmpSeriesData);

        TryChangeSeriesStyle(tmpSeries);
      end;
    end;

    NotMandatoryAxis(tmpSeries).Title.Caption:=AGroup.Data.Name;

    CheckAutoStack;

    //TGroupChart.FinishChart(Parent,Control as TChart);

    //TryMultipleAxes(FVisualizer.MultiAxis);
  end
  else
  begin
    if AComponent is TChartSeries then
       tmpSeries:=TChartSeries(AComponent)
    else
       tmpSeries:=nil;

    // One normal column or tables:
    for t:=0 to Values.Count-1 do
    begin
      tmp:=Values[t];

      if tmp.Enabled then
      begin
        if (AGroup<>nil) and (Parent=nil) and (AGroup.Data.Parent=tmp.Data) then
           AddGroupAsSeries(AGroup.Data,tmp)
        else
        begin
          if tmpSeries=nil then
          begin
            if SingleSeries=nil then
               SingleSeries:=AddSingleSeries;

            tmpSeries:=SingleSeries;
          end;

          tmpSeriesData.AddNulls:=Options.AddNulls;
          tmpSeriesData.Data:=tmp.Data;
          tmpSeriesData.Series:=tmpSeries;

          LoopRows(tmpSeriesData);

          tmpIsSingle:=tmpSeries=SingleSeries;

          TryChangeSeriesStyle(tmpSeries);

          if tmpIsSingle then
             SingleSeries:=tmpSeries;
        end;
      end;
    end;

    TrySetTitles(AGroup,Values);
  end;
end;

procedure TGroupSeries.Assign(Source: TPersistent);
begin
  if Source is TGroupSeries then
     FSeriesOptions.Assign(TGroupSeries(Source).FSeriesOptions)
  else
    inherited;
end;

Destructor TGroupSeries.Destroy;
begin
  FSeriesOptions.Free;
  inherited;
end;

class function TGroupSeries.BestControl(const AIndex,ATotal,AValues:Integer):TGroupClass;
begin
  if AValues=0 then
     result:=nil
  else
  if AIndex=ATotal-1 then
     if AValues=1 then
        result:=TGroupSeries
     else
        result:=TGroupChart
  else
  if AIndex=ATotal-2 then
     result:=TGroupChart
  else
     result:=nil;
end;

procedure TGroupSeries.Finished;
var tmp : TCustomChart;
    tmpS : TChartSeries;
    tmpXName : String;
begin
  if Parent=nil then
  begin
    tmp:=(Control as TBIChart).Chart;

    if tmp.SeriesCount=1 then
    begin
      tmpS:=tmp[0];

      if Data<>nil then
      begin
        {$IFDEF TEEPRO}
        if not tmpS.HasZValues then
        {$ENDIF}
           tmp.Legend.Title.Caption:=Data.Name;

        if tmpS.HasZValues and (Parent<>nil) and (Parent.Data<>nil) then
           tmpXName:=Parent.Data.Name
        else
           tmpXName:=Data.Name;

        NotMandatoryAxis(tmpS).Title.Caption:=tmpXName;

        if tmpS.HasZValues then
           tmp.Axes.Depth.Title.Caption:=Data.Name;
      end;

      SetColorEach(tmpS);
    end;
  end;

  inherited;
end;

{ TGroupChartOptions }

Constructor TGroupChartOptions.Create;
begin
  inherited Create;
  FSettings:=True;
end;

destructor TGroupChartOptions.Destroy;
begin
  FTemplate.Free;
  inherited;
end;

procedure TGroupChartOptions.SetLegend(const Value: Boolean);
begin
  Template.Legend.Visible:=Value;
  IParent.FChart.Chart.Legend.Visible:=Value;

  {$IFNDEF FPC}
  IParent.Traverse<TGroupChart>(procedure(const AGroup:TGroupChart)
  begin
    if AGroup<>IParent then
       AGroup.Options.Legend:=Value;
  end);
  {$ENDIF}
end;

procedure TGroupChartOptions.SetMarks(const Value: Boolean);
begin
  if Template.SeriesCount=0 then
     Template.AddSeries(TChartSeries);

  Template[0].Marks.Visible:=Value;

  {$IFNDEF FPC}
  IParent.Traverse<TGroupChart>(procedure(const AGroup:TGroupChart)
  begin
    TraverseChart(AGroup.Chart.Chart,procedure(const AChart:TCustomChart)
    var tmpS : TChartSeries;
    begin
      for tmpS in AChart.SeriesList do
          tmpS.Marks.Visible:=Value;
    end);
  end);
  {$ENDIF}
end;

procedure TGroupChartOptions.SetMultiAxes(const Value: TBIMultiAxis);
begin
  {$IFNDEF FPC}
  IParent.Traverse<TGroupChart>(procedure(const AGroup:TGroupChart)
  begin
    TraverseChart(AGroup.Chart.Chart,procedure(const AChart:TCustomChart)
    begin
      if AGroup.FOptions.FMultiAxes<>Value then
      begin
        AGroup.FOptions.FMultiAxes:=Value;
        AGroup.TryMultipleAxes(AGroup.FOptions.FMultiAxes);
      end;
    end);
  end);
  {$ENDIF}
end;

procedure TGroupChartOptions.Assign(Source: TPersistent);
begin
  if Source is TGroupChartOptions then
  begin
    FMultiAxes:=TGroupChartOptions(Source).FMultiAxes;
    FRender:=TGroupChartOptions(Source).FRender;
    Template:=TGroupChartOptions(Source).FTemplate;
  end
  else
    inherited;
end;

function TGroupChartOptions.GetLegend: Boolean;
begin
  if FTemplate=nil then
     result:=True
  else
     result:=FTemplate.Legend.Visible
end;

function TGroupChartOptions.GetMarks: Boolean;
begin
  if (FTemplate=nil) or (FTemplate.SeriesCount=0) then
     result:=True
  else
     result:=FTemplate[0].Marks.Visible;
end;

function TGroupChartOptions.GetTemplate: TChart;
begin
  if FTemplate=nil then
  begin
    FTemplate:=TChart.Create(nil);
    IParent.InitChart(FTemplate);
  end;

  result:=FTemplate;
end;

procedure TGroupChartOptions.SetRender(const Value: TCanvas3DClass);
begin
  {$IFNDEF FPC}
  IParent.Traverse<TGroupChart>(procedure(const AGroup:TGroupChart)
  begin
    AGroup.Options.FRender:=Value;
    AGroup.SetCanvas(AGroup.Chart.Chart);
  end);
  {$ENDIF}
end;

procedure TGroupChartOptions.SetSettings(const Value: Boolean);
begin
  {$IFNDEF FPC}
  IParent.Traverse<TGroupChart>(procedure(const AGroup:TGroupChart)
  begin
    AGroup.Options.FSettings:=Value;
  end);
  {$ENDIF}
end;

procedure TGroupChartOptions.SetTemplate(const Value: TChart);
begin
  if Value=nil then
  begin
    FTemplate.Free;
    FTemplate:=nil;
  end
  else
     Template.Assign(Value);
end;

{ TBIChartComposerUI }

class procedure TBIChartComposerUI.Fill2DSeries(const AItems:TStrings);

  procedure Add(const AClass:TChartSeriesClass);
  var tmp : TTeeSeriesType;
  begin
    tmp:=TeeSeriesTypes.Find(AClass);

    if tmp<>nil then
       AItems.AddObject(tmp.Description^,tmp);
  end;

begin
  AItems.BeginUpdate;
  try
    AItems.Clear;

    AItems.Add('Automatic');

    Add(TBarSeries);
    Add(THorizBarSeries);
    Add(TAreaSeries);
    Add(THorizAreaSeries);
    Add(TLineSeries);
    Add(THorizLineSeries);
    Add(TPointSeries);
    Add(TFastLineSeries);
    Add(TPieSeries);

    {$IFDEF TEEPRO}
    Add(TDonutSeries);
    Add(TPolarSeries);
    Add(TRadarSeries);
    Add(TPolarGridSeries);
    {$ENDIF}
  finally
    AItems.EndUpdate;
  end;
end;

class procedure TBIChartComposerUI.Fill3DSeries(const AItems:TStrings);

  procedure Add(const AClass:TChartSeriesClass);
  var tmp : TTeeSeriesType;
  begin
    tmp:=TeeSeriesTypes.Find(AClass);

    if tmp<>nil then
       AItems.AddObject(tmp.Description^,tmp);
  end;

begin
  AItems.BeginUpdate;
  try
    AItems.Clear;

    AItems.Add('Automatic');

    {$IFDEF TEEPRO}
    Add(TTowerSeries);
    Add(TSurfaceSeries);
    Add(TColorGridSeries);
    Add(TContourSeries);
    Add(TPoint3DSeries);
    {$ENDIF}
  finally
    AItems.EndUpdate;
  end;
end;

function TBIChartComposerUI.GetSeries(const AIndex:Integer):TGroupSeries;
begin
  if AIndex=-1 then
  begin
    if Viz.Main is TGroupSeries then
       result:=TGroupSeries(Viz.Main)
    else
       result:=nil;
  end
  else
  if Viz.Groups[AIndex].Current is TGroupSeries then
     result:=TGroupSeries(Viz.Groups[AIndex].Current)
  else
     result:=nil;
end;

class function TBIChartComposerUI.CurrentSeriesClass(const AItems:TComboBox):TChartSeriesClass;
var tmp : TObject;
begin
  if AItems.ItemIndex=-1 then
     result:=nil
  else
  begin
    tmp:=AItems.Items.Objects[AItems.ItemIndex];

    if tmp=nil then
       result:=nil
    else
       result:=TTeeSeriesType(tmp).SeriesClass;
  end;
end;

class function TBIChartComposerUI.FindSeries(const AItems:TStrings; const AClass:TChartSeriesClass):Integer;
var tmp : TTeeSeriesType;
begin
  if AClass=nil then
     result:=AItems.IndexOf('Automatic')
  else
  begin
    tmp:=TeeSeriesTypes.Find(AClass);

    if tmp=nil then
       result:=-1
    else
       result:=AItems.IndexOfObject(tmp);
  end;
end;

{$IFDEF TEEPRO}
function TBIChartComposerUI.GetSubChart(const AIndex:Integer):TGroupSubChart;
begin
  if (AIndex<>-1) and (Viz.Groups[AIndex].Current is TGroupSubChart) then
     result:=TGroupSubChart(Viz.Groups[AIndex].Current)
  else
     result:=nil;
end;
{$ENDIF}

function TBIChartComposerUI.GetChart(const AIndex:Integer):TGroupChart;
begin
  if (AIndex<>-1) and (Viz.Groups[AIndex].Current is TGroupChart) then
     result:=TGroupChart(Viz.Groups[AIndex].Current)
  else
     result:=nil;
end;

{ TGroupSeriesOptions }

Constructor TGroupSeriesOptions.Create(const AParent:TGroupSeries);
begin
  inherited Create;
  IParent:=AParent;
  FAddNulls:=True;
end;

procedure TGroupSeriesOptions.Assign(Source: TPersistent);
begin
  if Source is TGroupSeriesOptions then
  begin
    FAddNulls:=TGroupSeriesOptions(Source).FAddNulls;
    FAutoStack:=TGroupSeriesOptions(Source).FAutoStack;
    FStyle:=TGroupSeriesOptions(Source).FStyle;
    FSeries2D:=TGroupSeriesOptions(Source).FSeries2D;
    FSeries3D:=TGroupSeriesOptions(Source).FSeries3D;
  end
  else
     inherited;
end;

function TGroupSeriesOptions.NewSeries(const AName: String; const AOptions:TGroupChartOptions): TChartSeries;
var tmpClass : TChartSeriesClass;
    tmpDefault : Boolean;
begin
  tmpDefault:=(AOptions.Template=nil) or (AOptions.Template.SeriesCount=0);

  if tmpDefault then
     {$IFDEF TEEPRO}
     if Style=TGroupSeriesStyle.Series3D then
     begin
       tmpClass:=Series3D;

       if tmpClass=nil then
          tmpClass:=TTowerSeries;
     end
     else
     if Style=TGroupSeriesStyle.Geographic then
        tmpClass:=TWorldSeries
     else
     {$ENDIF}
     begin
       tmpClass:=Series2D;

       if tmpClass=nil then
          tmpClass:=TBarSeries;
     end
  else
     tmpClass:=TChartSeriesClass(AOptions.Template[0].ClassType);

  result:=tmpClass.Create(AOptions.IParent);
  result.Marks.Style:=TSeriesMarksStyle.smsValue;

  if result is TCustomSeries then
  begin
    TCustomSeries(result).Pointer.Style:=psCircle;
  end;

  if result is TAreaSeries then
  begin
    TAreaSeries(result).AreaLinesPen.Hide;
  end;

  if not tmpDefault then
     result.Assign(AOptions.Template[0]);

  // Set Title after assigning Template !
  result.Title:=AName;
end;

procedure TGroupSeriesOptions.SetAddNulls(const Value: Boolean);
begin
  {$IFNDEF FPC}
  IParent.Traverse<TGroupSeries>(procedure(const AGroup:TGroupSeries)
  begin
    if AGroup.Options.FAddNulls<>Value then
    begin
      AGroup.Options.FAddNulls:=Value;
      AGroup.Reset;
    end;
  end);
  {$ENDIF}
end;

procedure TGroupSeriesOptions.SetAutoStack(const Value: TAutoStackSeries);
begin
  {$IFNDEF FPC}
  IParent.Traverse<TGroupSeries>(procedure(const AGroup:TGroupSeries)
  begin
    if AGroup.Options.FAutoStack<>Value then
    begin
      AGroup.Options.FAutoStack:=Value;
      AGroup.CheckAutoStack;
    end;
  end);
  {$ENDIF}
end;

procedure TGroupSeriesOptions.SetSeries2D(const Value: TChartSeriesClass);
begin
  {$IFNDEF FPC}
  IParent.Traverse<TGroupSeries>(procedure(const AGroup:TGroupSeries)
  begin
    if AGroup.Options.FSeries2D<>Value then
    begin
      AGroup.Options.FSeries2D:=Value;
      AGroup.Reset;
    end;
  end);
  {$ENDIF}
end;

procedure TGroupSeriesOptions.SetSeries3D(const Value: TChartSeriesClass);
begin
  {$IFNDEF FPC}
  IParent.Traverse<TGroupSeries>(procedure(const AGroup:TGroupSeries)
  begin
    if AGroup.Options.FSeries3D<>Value then
    begin
      AGroup.Options.FSeries3D:=Value;
      AGroup.Reset;
    end;
  end);
  {$ENDIF}
end;

procedure TGroupSeriesOptions.SetStyle(const Value: TGroupSeriesStyle);
begin
  {$IFNDEF FPC}
  IParent.Traverse<TGroupSeries>(procedure(const AGroup:TGroupSeries)
  begin
    if AGroup.Options.FStyle<>Value then
    begin
      AGroup.Options.FStyle:=Value;
      AGroup.Reset;
    end;
  end);
  {$ENDIF}
end;

{$IFDEF TEEPRO}

{ TGroupSubChart }

Constructor TGroupSubChart.CreateData(const AItem:TVisualizerItem; const AParent:TGroup);
begin
  inherited;
  FSameAxisRange:=True;
end;

procedure TGroupSubChart.Assign(Source:TPersistent);
begin
  if Source is TGroupSubChart then
  begin
    FColumns:=TGroupSubChart(Source).FColumns;
    FSameAxisRange:=TGroupSubChart(Source).FSameAxisRange;
  end;

  inherited;
end;

procedure TGroupSubChart.HideChartParts;
begin
  FChart.Chart.Axes.Hide;
  FChart.Chart.Walls.Hide;
end;

function TGroupSubChart.AddItem:TComponent;
var tmp : TChart;
begin
  if FTool=nil then
  begin
    FTool:=TSubChartTool.Create(Self);

    if Parent is TGroupSubChart then
    begin
      FChart:=nil; //TGroupSubChart(Parent).FTool.Charts[TGroupAccess(Parent).Position].Chart;
      Control:=FChart;
    end;

    FTool.ParentChart:=FChart.Chart;

    HideChartParts;
  end;

  tmp:=FTool.Charts.AddChart;

  TGroupChart.InitChart(tmp);

  tmp.Title.Caption:=AsString(Data);
  tmp.Title.Visible:=True;

  Control:=tmp;

  result:=Control;
end;

procedure TGroupSubChart.ApplyTemplate(const AChart,ATemplate:TCustomChart);
begin
  inherited;
  HideChartParts;
end;

procedure TGroupSubChart.SetColumns(const Value: Integer);
begin
  {$IFNDEF FPC}
  Traverse<TGroupSubChart>(procedure(const AGroup:TGroupSubChart)
  begin
    if AGroup.FColumns<>Value then
    begin
      AGroup.FColumns:=Value;
      AGroup.ChartResized(AGroup);
    end;
  end);
  {$ENDIF}
end;

procedure TGroupSubChart.RecalcAxisRange;

  function GetChartAxis(const AIndex:Integer; out Axis:TChartAxis):Boolean;
  var tmp : TCustomChart;
  begin
    tmp:=FTool.Charts[AIndex].Chart;

    result:=tmp.SeriesCount>0;

    if result then
       if tmp[0].YMandatory then
          Axis:=tmp[0].GetVertAxis
       else
          Axis:=tmp[0].GetHorizAxis;
  end;

  procedure CalcMinMax(out AMin,AMax:TChartValue);
  var tmp : TChartAxis;
      t : Integer;
  begin
    AMin:=0;
    AMax:=0;

    if FTool.Charts.Count>0 then
    begin
      if GetChartAxis(0,tmp) then
      begin
        tmp.AdjustMaxMin;
        AMin:=tmp.Minimum;
        AMax:=tmp.Maximum;

        for t:=1 to FTool.Charts.Count-1 do
            if GetChartAxis(t,tmp) then
            begin
              if tmp.Minimum<AMin then
                 AMin:=tmp.Minimum;

              if tmp.Maximum>AMax then
                 AMax:=tmp.Maximum;
            end;
      end;
    end
  end;

var tmpAxis : TChartAxis;
    t : Integer;
    tmpMin,
    tmpMax : TChartValue;
begin
  if FTool<>nil then
  begin
    if FSameAxisRange then
       CalcMinMax(tmpMin,tmpMax);

    for t:=0 to FTool.Charts.Count-1 do
        if GetChartAxis(t,tmpAxis) then
           if FSameAxisRange then
              tmpAxis.SetMinMax(tmpMin,tmpMax)
           else
              tmpAxis.Automatic:=True;
  end;
end;

procedure TGroupSubChart.SetSameAxisRange(const Value: Boolean);
begin
  {$IFNDEF FPC}
  Traverse<TGroupSubChart>(procedure(const AGroup:TGroupSubChart)
  begin
    if AGroup.FSameAxisRange<>Value then
    begin
      AGroup.FSameAxisRange:=Value;
      AGroup.RecalcAxisRange;
    end;
  end);
  {$ENDIF}
end;

procedure TGroupSubChart.Finished;
begin
  inherited;

  if FTool<>nil then
  begin
    if FSameAxisRange then
       RecalcAxisRange;

    ChartResized(Self);
  end;
end;

{$IFNDEF TEEPRO2016}
type
  TChartAccess=class(TCustomAxisPanel);

procedure GridLayout(const ATool:TSubChartTool; const AColumns: Integer);

  function TitleHeight(const ATitle:TChartTitle): {$IFDEF FMX}Single{$ELSE}Integer{$ENDIF};
  begin
    if ATitle.Visible and (ATitle.Caption<>'') then
    begin
      if ATitle.Height=0 then
         ATitle.ParentChart.Draw;

      result:=ATitle.TitleRect.Bottom-ATitle.TitleRect.Top;
    end
    else
      result:=0;
  end;

var t,
    tmp,
    x,y,
    tmpCols,
    tmpRows : Integer;

    tmpW,
    tmpH,
    tmpX,
    tmpY,
    tmpLeft,
    tmpTop,
    tmpMarginX,
    tmpMarginY,
    tmpTitle,
    tmpFooter,
    tmpRowHeight : TCoordinate;

    tmpChart : TCustomAxisPanel;
    tmpC : TCustomChart;
begin
  // Split sub charts
  tmp:=ATool.Charts.Count;

  if AColumns<=0 then
     tmpCols:=Round(Sqrt(tmp))
  else
     tmpCols:=AColumns;

  tmpRows:=(tmp div tmpCols);

  if tmp>(tmpCols*tmpRows) then
     Inc(tmpRows);

  tmpChart:=ATool.ParentChart;

  if tmpChart is TCustomChart then
  begin
    tmpTitle:=TitleHeight(TCustomChart(tmpChart).Title);
    tmpFooter:=TitleHeight(TCustomChart(tmpChart).Foot);
  end
  else
  begin
    tmpTitle:=0;
    tmpFooter:=0;
  end;

  tmpRowHeight:=(tmpChart.Height-tmpTitle-tmpFooter) {$IFDEF FMX}/{$ELSE}div{$ENDIF} tmpRows;

  tmpW:=Round(0.9*(tmpChart.Width {$IFDEF FMX}/{$ELSE}div{$ENDIF} tmpCols));
  tmpH:=Round(0.9*tmpRowHeight);

  x:=0;
  y:=0;

  tmpMarginX:=Round(0.05*tmpW);
  tmpMarginY:=Round(0.05*tmpH);

  if TChartAccess(tmpChart).GetParentComponent=ATool then
//  if tmpChart is TInnerChart then
  begin
    tmpLeft:=TChart(tmpChart).{$IFDEF FMX}Position.X{$ELSE}Left{$ENDIF};
    tmpTop:=TChart(tmpChart).{$IFDEF FMX}Position.Y{$ELSE}Top{$ENDIF};
  end
  else
  begin
    tmpLeft:=0;
    tmpTop:=0;
  end;

  for t:=0 to tmp-1 do
  begin
    tmpC:=ATool.Charts[t].Chart;

    tmpX:=tmpLeft + tmpMarginX + (x*(tmpChart.Width {$IFDEF FMX}/{$ELSE}div{$ENDIF} tmpCols));

    tmpC.{$IFDEF FMX}Position.X{$ELSE}Left{$ENDIF}:=tmpX;
    tmpC.Width:=tmpW;

    tmpY:=tmpTop + tmpTitle+ tmpMarginY + (y*tmpRowHeight);

    tmpC.{$IFDEF FMX}Position.Y{$ELSE}Top{$ENDIF}:=tmpY;
    tmpC.Height:=tmpH;

    if Assigned(tmpC.OnResize) then
       tmpC.OnResize(tmpC);

    Inc(x);

    if x>=tmpCols then
    begin
      x:=0;
      Inc(y);
    end;
  end;
end;
{$ENDIF}

procedure TGroupSubChart.ChartResized(Sender:TObject);
begin
  inherited;

  if FTool<>nil then
     {$IFDEF TEEPRO2016}
     FTool.GridLayout(FColumns);
     {$ELSE}
     GridLayout(FTool,FColumns); // <-- Can be FTool.GridLayout with latest TeeChart Pro
     {$ENDIF}
end;
{$ENDIF}

initialization
  TGroup.GroupClasses.Add(TGroupChart);
  TGroup.GroupClasses.Add(TGroupSeries);

  {$IFDEF TEEPRO}
  TGroup.GroupClasses.Add(TGroupSubChart);
  {$ENDIF}

  TBIComposer.ValuesGroupClass:=TGroupSeries;
finalization
  TBIComposer.ValuesGroupClass:=nil;
end.
