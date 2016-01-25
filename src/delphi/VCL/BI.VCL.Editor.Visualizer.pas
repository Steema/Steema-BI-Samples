{*********************************************}
{  TeeBI Software Library                     }
{  Data Visualizer Editor                     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.Visualizer;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.VCL.Visualizer, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, Vcl.CheckLst, Vcl.ComCtrls, BI.VCL.Grid,
  BI.Summary;

type
  TVisualizerEditor = class(TForm)
    LBGroups: TCheckListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    BUp: TSpeedButton;
    BDown: TSpeedButton;
    PageItems: TPageControl;
    PageSettings: TPageControl;
    TabCombo: TTabSheet;
    TabMulti: TTabSheet;
    LMultiScroll: TLabel;
    CBMultiScroll: TComboBox;
    Label3: TLabel;
    EColumns: TEdit;
    UDColumns: TUpDown;
    TabChart: TTabSheet;
    BEditChart: TButton;
    TabList: TTabSheet;
    TabSeries: TTabSheet;
    CBAddNulls: TCheckBox;
    Button1: TButton;
    Label7: TLabel;
    CBRender: TComboBox;
    TabValues: TTabSheet;
    LBValues: TCheckListBox;
    Label4: TLabel;
    CBAutoStack: TComboBox;
    Label1: TLabel;
    CB2D: TComboBox;
    Label6: TLabel;
    CB3D: TComboBox;
    Label8: TLabel;
    CBRenderSeries: TComboBox;
    Panel5: TPanel;
    Panel6: TPanel;
    SBUpValue: TSpeedButton;
    SBDownValue: TSpeedButton;
    Panel7: TPanel;
    CBLegend: TCheckBox;
    CBMarks: TCheckBox;
    Button2: TButton;
    Label11: TLabel;
    CBStyle: TComboBox;
    TabPage: TTabSheet;
    CBExpandLast: TCheckBox;
    Group: TTabSheet;
    Panel4: TPanel;
    LCurrentHeader: TLabel;
    LCurrent: TLabel;
    Label9: TLabel;
    CBGroupBy: TComboBox;
    RGClass: TRadioGroup;
    CBCheckBoxes: TCheckBox;
    TabTrack: TTabSheet;
    Label14: TLabel;
    ERowHeight: TEdit;
    UDRowHeight: TUpDown;
    CBSplitters: TCheckBox;
    CBChartSettings: TCheckBox;
    TabSelected: TTabSheet;
    CBSelected: TCheckListBox;
    TabSubChart: TTabSheet;
    Label15: TLabel;
    ESubColumns: TEdit;
    UDSubColumns: TUpDown;
    CBSubColumns: TCheckBox;
    CBSameAxisRange: TCheckBox;
    TabControl: TTabSheet;
    Label12: TLabel;
    CBControlAlign: TComboBox;
    CBControlLabel: TCheckBox;
    Label10: TLabel;
    CBMultiAxis: TComboBox;
    TabGrid: TTabSheet;
    Panel8: TPanel;
    SpeedButton2: TSpeedButton;
    CBDuplicates: TCheckBox;
    CBColorize: TCheckBox;
    BIGrid1: TBIGrid;
    procedure LBGroupsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BEditChartClick(Sender: TObject);
    procedure CBMultiScrollChange(Sender: TObject);
    procedure BUpClick(Sender: TObject);
    procedure BDownClick(Sender: TObject);
    procedure LBGroupsClickCheck(Sender: TObject);
    procedure CBAutoStackChange(Sender: TObject);
    procedure EColumnsChange(Sender: TObject);
    procedure CBAddNullsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CB2DChange(Sender: TObject);
    procedure CB3DChange(Sender: TObject);
    procedure CBRenderChange(Sender: TObject);
    procedure LBValuesClickCheck(Sender: TObject);
    procedure CBRenderSeriesChange(Sender: TObject);
    procedure SBUpValueClick(Sender: TObject);
    procedure SBDownValueClick(Sender: TObject);
    procedure LBValuesClick(Sender: TObject);
    procedure CBGroupByChange(Sender: TObject);
    procedure CBMultiAxisChange(Sender: TObject);
    procedure CBLegendClick(Sender: TObject);
    procedure CBMarksClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CBStyleChange(Sender: TObject);
    procedure CBControlAlignChange(Sender: TObject);
    procedure CBExpandLastClick(Sender: TObject);
    procedure RGClassClick(Sender: TObject);
    procedure CBCheckBoxesClick(Sender: TObject);
    procedure ERowHeightChange(Sender: TObject);
    procedure CBSplittersClick(Sender: TObject);
    procedure CBChartSettingsClick(Sender: TObject);
    procedure PageSettingsChange(Sender: TObject);
    procedure CBSelectedClickCheck(Sender: TObject);
    procedure ESubColumnsChange(Sender: TObject);
    procedure CBSubColumnsClick(Sender: TObject);
    procedure CBSameAxisRangeClick(Sender: TObject);
    procedure CBControlLabelClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure CBColorizeClick(Sender: TObject);
    procedure CBDuplicatesClick(Sender: TObject);
  private
    { Private declarations }
    Viz : TBIVisualizer;

    VizUI : TBIVisualizerUI;

    IChanging : Boolean;

    procedure ChangeAlign(const AIndex:Integer);
    procedure ChangeRender(const AIndex:Integer);

    procedure FillSelected(const AItem:TVisualizerItem);
    procedure MoveGroup(const ADelta:Integer);
    procedure MoveValue(const ADelta:Integer);
    procedure ShowGroupSettings(const AGroup:TGroup);
  public
    { Public declarations }

    procedure CheckDuplicates(const ASummary:TSummary);

    class procedure Edit(const AOwner:TComponent; const AVisualizer:TBIVisualizer); static;
    class function Embedd(const AOwner:TComponent; const AParent:TWinControl):TVisualizerEditor; static;

    procedure Refresh(const AVisualizer:TBIVisualizer);
  end;

implementation
