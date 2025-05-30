{*********************************************}
{  TeeBI Software Library                     }
{  Chart Visualizer Editor                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Visualizer.Chart;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, Vcl.Graphics,
  Vcl.Forms, Vcl.Dialogs,

  VCLBI.Visualizer, VCLBI.Visualizer.Chart, VCLBI.Editor.Visualizer,
  VCLBI.Editor.Chart, ComCtrls, StdCtrls, Controls, Classes;

type
  TChartVisualizerEditor = class(TForm)
    PageControl1: TPageControl;
    TabOptions: TTabSheet;
    Label7: TLabel;
    Label10: TLabel;
    BEditChart: TButton;
    CBRender: TComboBox;
    CBLegend: TCheckBox;
    CBMarks: TCheckBox;
    CBChartSettings: TCheckBox;
    CBMultiAxis: TComboBox;
    TabSeries: TTabSheet;
    Label4: TLabel;
    Label1: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label11: TLabel;
    CBAddNulls: TCheckBox;
    CBAutoStack: TComboBox;
    CB2D: TComboBox;
    CB3D: TComboBox;
    CBRenderSeries: TComboBox;
    CBStyle: TComboBox;
    TabSubChart: TTabSheet;
    Label15: TLabel;
    ESubColumns: TEdit;
    UDSubColumns: TUpDown;
    CBSubColumns: TCheckBox;
    CBSameAxisRange: TCheckBox;
    TabChart: TTabSheet;
    procedure CBAutoStackChange(Sender: TObject);
    procedure CBChartSettingsClick(Sender: TObject);
    procedure CBLegendClick(Sender: TObject);
    procedure CBMarksClick(Sender: TObject);
    procedure CBMultiAxisChange(Sender: TObject);
    procedure CBRenderChange(Sender: TObject);
    procedure CBRenderSeriesChange(Sender: TObject);
    procedure CBSameAxisRangeClick(Sender: TObject);
    procedure CBStyleChange(Sender: TObject);
    procedure CBSubColumnsClick(Sender: TObject);
    procedure CB2DChange(Sender: TObject);
    procedure CB3DChange(Sender: TObject);
    procedure CBAddNullsClick(Sender: TObject);
    procedure ESubColumnsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BEditChartClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;

    GroupIndex : Integer;
    VizUI : TBIChartComposerUI;

    IChartEditor : TBIChartEditor;
    VizEditor : TVisualizerEditor;

    procedure ChangeRender(const AIndex:Integer);
    // DEPRECATED: procedure ShowGroupSettings(const AGroup:TGroup);
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  // TeeConst before checking TeePro:
  VCLTee.TeeConst,

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}

  {$ENDIF}

  VCLTee.TeEngine, VCLTee.Series,

  {$IFDEF TEEPRO}
  VCLTee.TeeThemeEditor,

  {$IFNDEF FPC}
  VCLTee.TeeGLCanvas, VCLTee.TeeGLEditor,
  {$ENDIF}

  VCLTee.TeeEditPro, VCLTee.TeeWorldSeriesEditor,
  {$ENDIF}

  VCLTee.TeCanvas,

  {$IFDEF MSWINDOWS}
  VCLTee.TeeGDIPlus, VCLTee.TeeGDIPlusEditor,
  {$ENDIF}

  VCLTee.Chart, VCLTee.EditChar;

procedure TChartVisualizerEditor.BEditChartClick(Sender: TObject);
var tmp : TGroupChart;
begin
  tmp:=VizUI.GetChart(GroupIndex);

  if tmp<>nil then
  begin
    EditChart(Self,tmp.Options.Template);
    tmp.ApplyTemplate;
  end;
end;

procedure TChartVisualizerEditor.CB2DChange(Sender: TObject);
var tmp : TGroupSeries;
begin
  tmp:=VizUI.GetSeries(GroupIndex);

  if tmp<>nil then
     tmp.Options.Series2D:=TBIChartComposerUI.CurrentSeriesClass(CB2D);
end;

procedure TChartVisualizerEditor.CB3DChange(Sender: TObject);
var tmp : TGroupSeries;
begin
  tmp:=VizUI.GetSeries(GroupIndex);

  if tmp<>nil then
     tmp.Options.Series3D:=VizUI.CurrentSeriesClass(CB3D);
end;

procedure TChartVisualizerEditor.CBAddNullsClick(Sender: TObject);
var tmp : TGroupSeries;
begin
  if not IChanging then
  begin
    tmp:=VizUI.GetSeries(GroupIndex);

    if tmp<>nil then
       tmp.Options.AddNulls:=CBAddNulls.Checked;
  end;
end;

procedure TChartVisualizerEditor.CBAutoStackChange(Sender: TObject);
var tmp : TGroupSeries;
begin
  tmp:=VizUI.GetSeries(GroupIndex);

  if tmp<>nil then
     tmp.Options.AutoStack:=TAutoStackSeries(CBAutoStack.ItemIndex);
end;

procedure TChartVisualizerEditor.CBChartSettingsClick(Sender: TObject);
var tmp : TGroupChart;
begin
  tmp:=VizUI.GetChart(GroupIndex);

  if tmp<>nil then
     tmp.Options.Settings:=CBChartSettings.Checked;
end;

procedure TChartVisualizerEditor.CBLegendClick(Sender: TObject);
var tmp : TGroupChart;
    tmpSeries : TGroupSeries;
begin
  tmp:=VizUI.GetChart(GroupIndex);

  if tmp=nil then
  begin
    tmpSeries:=VizUI.GetSeries(GroupIndex);

    if tmpSeries<>nil then
       tmp:=tmpSeries.Group;
  end;

  if tmp<>nil then
     tmp.Options.Legend:=CBLegend.Checked;
end;

procedure TChartVisualizerEditor.CBMarksClick(Sender: TObject);
var tmp : TGroupChart;
begin
  tmp:=VizUI.GetChart(GroupIndex);

  if tmp<>nil then
     tmp.Options.Marks:=CBMarks.Checked;
end;

procedure TChartVisualizerEditor.CBMultiAxisChange(Sender: TObject);
var tmp : TGroupChart;
begin
  tmp:=VizUI.GetChart(GroupIndex);

  if tmp<>nil then
     tmp.Options.MultiAxes:=TBIMultiAxis(CBMultiAxis.ItemIndex);
end;

procedure TChartVisualizerEditor.CBRenderChange(Sender: TObject);
var tmp : TGroupChart;
begin
  tmp:=VizUI.GetChart(GroupIndex);

  if tmp<>nil then
     ChangeRender(CBRender.ItemIndex);
end;

procedure TChartVisualizerEditor.CBRenderSeriesChange(Sender: TObject);
var tmp : TGroupSeries;
begin
  tmp:=VizUI.GetSeries(GroupIndex);

  if tmp<>nil then
     ChangeRender(CBRenderSeries.ItemIndex);
end;

procedure TChartVisualizerEditor.CBSameAxisRangeClick(Sender: TObject);
{$IFDEF TEEPRO}
var tmp : TGroupSubChart;
{$ENDIF}
begin
  {$IFDEF TEEPRO}
  tmp:=VizUI.GetSubChart(GroupIndex);

  if tmp<>nil then
     tmp.SameAxisRange:=CBSameAxisRange.Checked;
  {$ENDIF}
end;

procedure TChartVisualizerEditor.CBStyleChange(Sender: TObject);
var tmp : TGroupSeries;
begin
  tmp:=VizUI.GetSeries(GroupIndex);

  if tmp<>nil then
     tmp.Options.Style:=TGroupSeriesStyle(CBStyle.ItemIndex);
end;

procedure TChartVisualizerEditor.CBSubColumnsClick(Sender: TObject);
begin
  if CBSubColumns.Checked then
     UDSubColumns.Position:=0;
end;

procedure TChartVisualizerEditor.ESubColumnsChange(Sender: TObject);
{$IFDEF TEEPRO}
var tmp : TGroupSubChart;
{$ENDIF}
begin
  {$IFDEF TEEPRO}
  if not IChanging then
  begin
    CBSubColumns.Checked:=UDSubColumns.Position=0;

    tmp:=VizUI.GetSubChart(GroupIndex);

    if tmp<>nil then
       tmp.Columns:=UDSubColumns.Position;
  end;
  {$ENDIF}
end;

procedure TChartVisualizerEditor.FormCreate(Sender: TObject);
begin
  TBIChartComposerUI.Fill2DSeries(CB2D.Items);
  TBIChartComposerUI.Fill3DSeries(CB3D.Items);
end;

procedure TChartVisualizerEditor.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabChart then
     if IChartEditor=nil then
//        IChartEditor:=TBIChartEditor.Embedd(Self,TabChart,VizUI.GetChart(GroupIndex).Chart)
end;

// DEPRECATED:
(*
type
  TVizAccess=class(TVisualizerEditor);

procedure TChartVisualizerEditor.ShowGroupSettings(const AGroup:TGroup);

  procedure SetCBRender(const AGroup:TGroupChart; const ACombo:TComboBox);
  begin
    if AGroup.Options.Render=nil then
       ACombo.ItemIndex:=0
    else
    if AGroup.Options.Render=TTeeCanvas3D then
       ACombo.ItemIndex:=1
    else
       ACombo.ItemIndex:=2;
  end;

  procedure SetChartSettings(const AGroup:TGroupChart);
  begin
    SetCBRender(AGroup,CBRender);
    CBLegend.Checked:=AGroup.Options.Legend;
    CBMarks.Checked:=AGroup.Options.Marks;
    CBChartSettings.Checked:=AGroup.Options.Settings;
    CBMultiAxis.ItemIndex:=Ord(AGroup.Options.MultiAxes);
  end;

var
  tmpSeries : TGroupSeries;

  {$IFDEF TEEPRO}
  tmpSubChart : TGroupSubChart;
  {$ENDIF}
begin
  if AGroup is TGroupSeries then // <-- before "...is TGroupChart"
  begin
    tmpSeries:=TGroupSeries(AGroup);

    CBAutoStack.ItemIndex:=Ord(tmpSeries.Options.AutoStack);
    CBStyle.ItemIndex:=Ord(tmpSeries.Options.Style);

    CB2D.ItemIndex:=TBIChartComposerUI.FindSeries(CB2D.Items,tmpSeries.Options.Series2D);
    CB3D.ItemIndex:=TBIChartComposerUI.FindSeries(CB3D.Items,tmpSeries.Options.Series3D);

    CBAddNulls.Checked:=tmpSeries.Options.AddNulls;

    SetCBRender(tmpSeries.Group,CBRenderSeries);

    TVizAccess(VizEditor).ShowSettings(TabSeries);

    if tmpSeries.Group<>AGroup then
    begin
      SetChartSettings(tmpSeries.Group);
      TabOptions.TabVisible:=True;
    end;
  end
  else
  {$IFDEF TEEPRO}
  if AGroup is TGroupSubChart then // <-- before "...is TGroupChart"
  begin
    tmpSubChart:=(AGroup as TGroupSubChart);

    UDSubColumns.Position:=Ord(tmpSubChart.Columns);
    CBSubColumns.Checked:=UDSubColumns.Position=0;
    CBSameAxisRange.Checked:=tmpSubChart.SameAxisRange;

    TVizAccess(VizEditor).ShowSettings(TabSubChart);
  end
  else
  {$ENDIF}
  if AGroup is TGroupChart then
  begin
    SetChartSettings(TGroupChart(AGroup));

    TVizAccess(VizEditor).ShowSettings(TabOptions);
  end
  else
    // ???
end;
*)

procedure TChartVisualizerEditor.ChangeRender(const AIndex:Integer);
var tmp : TGroupChart;
begin
  tmp:=VizUI.GetChart(GroupIndex);

  if tmp<>nil then
  begin
    case AIndex of
      0: tmp.Options.Render:=nil;
      1: tmp.Options.Render:=TTeeCanvas3D;

      {$IFDEF TEEPRO}
      {$IFNDEF FPC}
      2: tmp.Options.Render:=TGLCanvas;
      {$ENDIF}
      {$ENDIF}
    end;
  end;
end;

end.
