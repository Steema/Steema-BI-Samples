{*********************************************}
{  TeeBI Software Library                     }
{  Chart Visualizer Editor                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.Visualizer.Chart;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,

  BI.VCL.Visualizer, BI.VCL.Visualizer.Chart, BI.VCL.Editor.Visualizer;

type
  TChartVisualizerEditor = class(TForm)
    PageControl1: TPageControl;
    TabChart: TTabSheet;
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
  private
    { Private declarations }

    IChanging : Boolean;

    GroupIndex : Integer;
    VizUI : TBIChartComposerUI;

    VizEditor : TVisualizerEditor;

    procedure ChangeRender(const AIndex:Integer);
    procedure ShowGroupSettings(const AGroup:TGroup);
  public
    { Public declarations }
  end;

implementation
