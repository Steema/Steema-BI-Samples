{*********************************************}
{  TeeBI Software Library                     }
{  Chart Visualizer Editor                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXBI.Editor.Visualizer.Chart;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation,
  {$ENDIF}

  FMX.StdCtrls, FMX.TabControl, FMX.ListBox,
  FMXBI.Visualizer, FMXBI.Visualizer.Chart, FMXBI.Editor.Visualizer;

type
  TChartVisualizerEditor = class(TForm)
    TabControl1: TTabControl;
    TabChart: TTabItem;
    Button2: TButton;
    TabSeries: TTabItem;
    CBAddNulls: TCheckBox;
    Label6: TLabel;
    CBAutoStack: TComboBox;
    Label9: TLabel;
    CBSeriesStyle: TComboBox;
    Label7: TLabel;
    CB2D: TComboBox;
    Label8: TLabel;
    CB3D: TComboBox;
    procedure Button2Click(Sender: TObject);
    procedure CBAddNullsChange(Sender: TObject);
    procedure CBAutoStackChange(Sender: TObject);
    procedure CBSeriesStyleChange(Sender: TObject);
    procedure CB2DChange(Sender: TObject);
    procedure CB3DChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    VizEditor : TVisualizerEditor;
    VizUI : TBIChartComposerUI;

    GroupIndex : Integer;

    // DEPRECATED: procedure ShowGroupSettings(const AGroup:TGroup);
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  // FMXTee.Constants before checking TeePro:
  FMXTee.Constants,

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}

  FMXTee.Engine, FMXTee.Series,

  {$IFDEF TEEPRO}
  FMXTee.Editor.Themes, FMXTee.Editor.Pro, FMXTee.Editor.Series.WorldMap,
  {$ENDIF}

  FMXTee.Canvas, FMXTee.Chart, FMXTee.Editor.Chart;

procedure TChartVisualizerEditor.Button2Click(Sender: TObject);
var tmp : TGroupChart;
begin
  tmp:=VizUI.GetChart(GroupIndex);

  if tmp<>nil then
  begin
    TChartEditForm.Edit(Self,tmp.Options.Template);
    tmp.ApplyTemplate;
  end;
end;

procedure TChartVisualizerEditor.CB2DChange(Sender: TObject);
var tmp : TGroupSeries;
begin
  tmp:=VizUI.GetSeries(GroupIndex);

  if tmp<>nil then
     tmp.Options.Series2D:=VizUI.CurrentSeriesClass(CB2D);
end;

procedure TChartVisualizerEditor.CB3DChange(Sender: TObject);
var tmp : TGroupSeries;
begin
  tmp:=VizUI.GetSeries(GroupIndex);

  if tmp<>nil then
     tmp.Options.Series3D:=VizUI.CurrentSeriesClass(CB3D);
end;

procedure TChartVisualizerEditor.CBAddNullsChange(Sender: TObject);
var tmp : TGroupSeries;
begin
  tmp:=VizUI.GetSeries(GroupIndex);

  if tmp<>nil then
     tmp.Options.AddNulls:=CBAddNulls.IsChecked;
end;

procedure TChartVisualizerEditor.CBAutoStackChange(Sender: TObject);
var tmp : TGroupSeries;
begin
  tmp:=VizUI.GetSeries(GroupIndex);

  if tmp<>nil then
     tmp.Options.AutoStack:=TAutoStackSeries(CBAutoStack.ItemIndex);
end;

procedure TChartVisualizerEditor.CBSeriesStyleChange(Sender: TObject);
var tmp : TGroupSeries;
begin
  tmp:=VizUI.GetSeries(GroupIndex);

  if tmp<>nil then
     tmp.Options.Style:=TGroupSeriesStyle(CBSeriesStyle.ItemIndex);
end;

procedure TChartVisualizerEditor.FormCreate(Sender: TObject);
begin
  TBIChartComposerUI.Fill2DSeries(CB2D.Items);
  TBIChartComposerUI.Fill3DSeries(CB3D.Items);
end;

(*
// DEPRECATED:
type
  TVizAccess=class(TVisualizerEditor);

procedure TChartVisualizerEditor.ShowGroupSettings(const AGroup:TGroup);
begin
  if AGroup is TGroupChart then
  begin
    //SetCBRender(CBRender);
    TVizAccess(VizEditor).ShowSettings(TabChart);
  end
  else
  if AGroup is TGroupSeries then
  begin
    CBAutoStack.ItemIndex:=Ord(TGroupSeries(AGroup).Options.AutoStack);
    CBSeriesStyle.ItemIndex:=Ord(TGroupSeries(AGroup).Options.Style);

    CB2D.ItemIndex:=TBIChartComposerUI.FindSeries(CB2D.Items,TGroupSeries(AGroup).Options.Series2D);
    CB3D.ItemIndex:=TBIChartComposerUI.FindSeries(CB3D.Items,TGroupSeries(AGroup).Options.Series3D);

    CBAddNulls.IsChecked:=(AGroup as TGroupSeries).Options.AddNulls;

    //SetCBRender(CBRenderSeries);

    TVizAccess(VizEditor).ShowSettings(TabSeries);
  end;
end;
*)

end.
