{*********************************************}
{  TeeBI Software Library                     }
{  Data Visualizer Editor                     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.Visualizer;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages,
  {$ENDIF}

  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLBI.Visualizer, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, Vcl.CheckLst, Vcl.ComCtrls, VCLBI.Grid,
  BI.Summary, VCLBI.DataControl;

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
    TabList: TTabSheet;
    Button1: TButton;
    TabValues: TTabSheet;
    LBValues: TCheckListBox;
    Panel5: TPanel;
    Panel6: TPanel;
    SBUpValue: TSpeedButton;
    SBDownValue: TSpeedButton;
    Panel7: TPanel;
    Button2: TButton;
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
    TabSelected: TTabSheet;
    CBSelected: TCheckListBox;
    TabControl: TTabSheet;
    Label12: TLabel;
    CBControlAlign: TComboBox;
    CBControlLabel: TCheckBox;
    TabGrid: TTabSheet;
    Panel8: TPanel;
    SpeedButton2: TSpeedButton;
    CBDuplicates: TCheckBox;
    CBColorize: TCheckBox;
    BIGrid1: TBIGrid;
    procedure LBGroupsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBMultiScrollChange(Sender: TObject);
    procedure BUpClick(Sender: TObject);
    procedure BDownClick(Sender: TObject);
    procedure LBGroupsClickCheck(Sender: TObject);
    procedure EColumnsChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure LBValuesClickCheck(Sender: TObject);
    procedure SBUpValueClick(Sender: TObject);
    procedure SBDownValueClick(Sender: TObject);
    procedure LBValuesClick(Sender: TObject);
    procedure CBGroupByChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CBControlAlignChange(Sender: TObject);
    procedure CBExpandLastClick(Sender: TObject);
    procedure RGClassClick(Sender: TObject);
    procedure CBCheckBoxesClick(Sender: TObject);
    procedure ERowHeightChange(Sender: TObject);
    procedure CBSplittersClick(Sender: TObject);
    procedure PageSettingsChange(Sender: TObject);
    procedure CBSelectedClickCheck(Sender: TObject);
    procedure CBControlLabelClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure CBColorizeClick(Sender: TObject);
    procedure CBDuplicatesClick(Sender: TObject);
  private
    { Private declarations }
    Viz : TBIComposer;

    VizUI : TBIVisualizerUI;

    IChanging : Boolean;

    procedure ChangeAlign(const AIndex:Integer);

    procedure FillSelected(const AItem:TVisualizerItem);
    procedure MoveGroup(const ADelta:Integer);
    procedure MoveValue(const ADelta:Integer);
    procedure ShowGroupSettings(const AGroup:TGroup);
  protected
    procedure ShowSettings(const ATab:TTabSheet);
  public
    { Public declarations }

    procedure CheckDuplicates(const ASummary:TSummary);

    class procedure Edit(const AOwner:TComponent; const AVisualizer:TBIComposer); static;
    class function Embedd(const AOwner:TComponent; const AParent:TWinControl):TVisualizerEditor; static;

    procedure Refresh(const AVisualizer:TBIComposer);
  end;

implementation
