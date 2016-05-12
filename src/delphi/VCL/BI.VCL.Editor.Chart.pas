unit BI.VCL.Editor.Chart;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  VclTee.TeeConst, VclTee.TeeEditCha, BI.VCL.Chart, BI.VCL.DataControl,
  BI.VCL.Grid;

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
    GroupBox1: TGroupBox;
    RBAuto: TRadioButton;
    RBXY: TRadioButton;
    RB3D: TRadioButton;
    RBFinancial: TRadioButton;
    RBGeo: TRadioButton;
    procedure PageControl1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RBAutoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RBXYClick(Sender: TObject);
    procedure RB3DClick(Sender: TObject);
    procedure RBFinancialClick(Sender: TObject);
    procedure RBGeoClick(Sender: TObject);
  private
    { Private declarations }

    Chart : TBIChart;

    IEditor : TChartEditForm;
  public
    { Public declarations }

    class procedure Edit(const AOwner:TComponent; const AChart:TBIChart); static;
  end;

implementation
