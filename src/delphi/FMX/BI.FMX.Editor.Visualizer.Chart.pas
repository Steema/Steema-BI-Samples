{*********************************************}
{  TeeBI Software Library                     }
{  Chart Visualizer Editor                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FMX.Editor.Visualizer.Chart;

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
  BI.FMX.Visualizer, BI.FMX.Visualizer.Chart, BI.FMX.Editor.Visualizer;

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
    VizUI : TBIChartVisualizerUI;

    GroupIndex : Integer;

    procedure ShowGroupSettings(const AGroup:TGroup);
  public
    { Public declarations }
  end;

implementation
