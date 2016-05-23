{*********************************************}
{  TeeBI Software Library                     }
{  BIChart Editor dialog                      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.Chart;

interface

{
  This dialog is used to edit BIChart controls.

  It offers the UI to change all properties that BIChart has in addition of the
  normal TChart properties, as well as allowing changing the Data to chart.
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  VclTee.TeeConst, VclTee.TeeEditCha, BI.VCL.Chart, BI.VCL.DataControl,
  BI.VCL.Grid, Vcl.Buttons, Vcl.CheckLst, BI.Data,

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}
  {$ENDIF}

  {$IFDEF TEEPRO}
  VCLTee.TeeEdit,
  {$ENDIF}

  BI.VCL.Editor.ListItems, VCLTee.TeeProcs;

type
  TBIChartEditor = class(TForm)
    PageControl1: TPageControl;
    PanelButtons: TPanel;
    Panel9: TPanel;
    BOK: TButton;
    TabOptions: TTabSheet;
    TabChart: TTabSheet;
    TabData: TTabSheet;
    Panel1: TPanel;
    Button1: TButton;
    BIGrid1: TBIGrid;
    PageMode: TPageControl;
    Tab2D: TTabSheet;
    Tab3D: TTabSheet;
    Panel3D: TPanel;
    BSwapXY: TSpeedButton;
    BSwapYZ: TSpeedButton;
    BSwapXZ: TSpeedButton;
    Shape1: TShape;
    Shape2: TShape;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    CBX: TComboBox;
    CBY: TComboBox;
    CBZ: TComboBox;
    LB3D: TListBox;
    LBXYZ: TListBox;
    RG3D: TRadioGroup;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    RBAuto: TRadioButton;
    RBXY: TRadioButton;
    RB3D: TRadioButton;
    RBFinancial: TRadioButton;
    RBGeo: TRadioButton;
    RGDirection: TRadioGroup;
    TabFinancial: TTabSheet;
    TabGeo: TTabSheet;
    Label7: TLabel;
    CBOpen: TComboBox;
    CBAutoFinancial: TCheckBox;
    CBClose: TComboBox;
    Label8: TLabel;
    Label9: TLabel;
    CBHigh: TComboBox;
    Label10: TLabel;
    CBLow: TComboBox;
    LBFinancial: TListBox;
    CB3D: TCheckBox;
    PanelYTable: TPanel;
    TabView: TTabSheet;
    RGView: TRadioGroup;
    CBOpenGL: TCheckBox;
    RGLegend: TRadioGroup;
    RGMarks: TRadioGroup;
    Label12: TLabel;
    CBVolume: TComboBox;
    Panel3: TPanel;
    GroupBox2: TLabel;
    LB2D: TListBox;
    Label11: TLabel;
    CBStacked: TComboBox;
    Panel4: TPanel;
    PanelItems: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CBX2D: TComboBox;
    CBText: TComboBox;
    CBColors: TComboBox;
    PanelY: TPanel;
    CBHoriz2D: TComboBox;
    procedure PageControl1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RBAutoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RBXYClick(Sender: TObject);
    procedure RB3DClick(Sender: TObject);
    procedure RBFinancialClick(Sender: TObject);
    procedure RBGeoClick(Sender: TObject);
    procedure RG3DClick(Sender: TObject);
    procedure LB2DClick(Sender: TObject);
    procedure BSwapXYClick(Sender: TObject);
    procedure BSwapYZClick(Sender: TObject);
    procedure BSwapXZClick(Sender: TObject);
    procedure CBTextChange(Sender: TObject);
    procedure LB3DClick(Sender: TObject);
    procedure CBX2DChange(Sender: TObject);
    procedure RGDirectionClick(Sender: TObject);
    procedure LBXYZClick(Sender: TObject);
    procedure CBXChange(Sender: TObject);
    procedure CBYChange(Sender: TObject);
    procedure CBZChange(Sender: TObject);
    procedure RGViewClick(Sender: TObject);
    procedure CBOpenGLClick(Sender: TObject);
    procedure RGLegendClick(Sender: TObject);
    procedure RGMarksClick(Sender: TObject);
    procedure CBOpenChange(Sender: TObject);
    procedure CBCloseChange(Sender: TObject);
    procedure CBHighChange(Sender: TObject);
    procedure CBLowChange(Sender: TObject);
    procedure CBVolumeChange(Sender: TObject);
    procedure CBStackedChange(Sender: TObject);
    procedure CBHoriz2DChange(Sender: TObject);
  private
    { Private declarations }

    FChart : TBIChart;

    IChanging : Boolean;

    IListY,
    IListYTable : TFormListItems;

    {$IFDEF TEEPRO}
    ChartEditor : TChartEditorPanel;
    {$ELSE}
    ChartEditor : TChartEditForm;
    {$ENDIF}

    procedure Changed3DItem;
    procedure ChangeY(const AIndex:Integer; const ACombo:TComboBox);
    function CurrentText:TDataItem;
    function DataOf(const ACombo:TComboBox):TDataItem;
    procedure DoExchange(const A,B:TComboBox);
    procedure Recreate;
    procedure RecreateY(Sender:TObject);
    procedure ResetEditor;
    procedure SetModeOptions;
    procedure SetPostSettings;
    procedure SetChart(const Value: TBIChart);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RefreshData(const AItems:TBIChartItems); overload;
  public
    { Public declarations }

    class procedure Edit(const AOwner:TComponent; const AChart:TBIChart); static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AChart:TBIChart):TBIChartEditor; static;

    procedure RefreshData; overload;

    property Chart:TBIChart read FChart write SetChart;
  end;

implementation
