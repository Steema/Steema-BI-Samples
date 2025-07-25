unit Main_Unit;

interface

{
  Note: This demo requires the "Pro" version of TeeChart components.

  https://www.steema.com
}


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, VCLTee.TeEngine, Vcl.Grids,
  VCLTee.TeeProcs, VCLTee.Chart, VCLBI.Chart.Plugin, VCLBI.Chart,
  VCLBI.DataControl, VCLBI.Grid, Vcl.Imaging.pngimage, BI.DataItem, BI.Persist,
  BI.Query, VCLTee.TeeTools, Vcl.ComCtrls, Vcl.StdCtrls,
  VCLTee.TeeWorldSeries, VCLTee.TeCanvas, VCLTee.TeePenDlg;

type
  TUSADemo_Form = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    BIGrid1: TBIGrid;
    Splitter1: TSplitter;
    BITChart1: TBITChart;
    BIChart1: TBIChart;
    BIQuery1: TBIQuery;
    CheckBox1: TCheckBox;
    Education_Year: TListBox;
    Button1: TButton;
    Education_Title: TListBox;
    ColorPalette: TCheckBox;
    ChartTool1: TRepaintMonitor;
    CBSkia: TCheckBox;
    CBSpeed: TCheckBox;
    ButtonPen1: TButtonPen;
    procedure FormCreate(Sender: TObject);
    procedure BITChart1AddSeries(Sender: TCustomChartSeries);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox1Click(Sender: TObject);
    procedure Education_YearClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ColorPaletteClick(Sender: TObject);
    procedure CBSpeedClick(Sender: TObject);
    procedure CBSkiaClick(Sender: TObject);
  private
    { Private declarations }

    Education : TDataItem;

    function EducationField:TDataItem;
    procedure DoQuery;
    procedure SetupChart;
    function WorldSeries:TWorldSeries;
  public
    { Public declarations }
  end;

var
  USADemo_Form: TUSADemo_Form;

implementation

{$R *.dfm}

uses
  BI.Geographic, VCLTee.TeeSurfa,
  VCLTee.TeeSkia, VCLTee.TeeGDIPlus,
  VCLBI.Grid.DBGrid,
//  VCLBI.Grid.TeeGrid, // <-- use TeeGrid instead of standard TDBGrid
  VCLTee.Grid, VCLTee.EditChar;

procedure TUSADemo_Form.Button1Click(Sender: TObject);
begin
  EditChart(Self,BIChart1.Chart)
end;

procedure TUSADemo_Form.CBSkiaClick(Sender: TObject);
begin
  if CBSkia.Checked then
     BIChart1.Chart.Canvas:=TTeeSkiaCanvas.Create
  else
     BIChart1.Chart.Canvas:=TGDIPlusCanvas.Create;
end;

procedure TUSADemo_Form.CBSpeedClick(Sender: TObject);
begin
  ChartTool1.Visible:=CBSpeed.Checked;
end;

procedure TUSADemo_Form.CheckBox1Click(Sender: TObject);
begin
  BIChart1.Chart[0].Marks.Visible:=CheckBox1.Checked;
end;

function TUSADemo_Form.WorldSeries:TWorldSeries;
begin
  result:=(BIChart1.Chart[0] as TWorldSeries);
end;

procedure TUSADemo_Form.ColorPaletteClick(Sender: TObject);
var S : TWorldSeries;
begin
  S:=WorldSeries;

  S.UsePalette:=ColorPalette.Checked;
  S.UseColorRange:=not S.UsePalette;

  S.PaletteStyle:=psRainbow;
end;

function TUSADemo_Form.EducationField:TDataItem;
var tmp : Integer;
begin
  case Education_Year.ItemIndex of
    0: tmp:=12;
    1: tmp:=20;
    2: tmp:=28;
    3: tmp:=36;
  else
    tmp:=44;
  end;

  result:=Education[tmp + Education_Title.ItemIndex -1];
end;

procedure TUSADemo_Form.DoQuery;
begin
  BIGrid1.Data.Free;

  BIQuery1.Clear;

  BIQuery1.Dimensions.Add(Education['FIPS Code']);
  BIQuery1.Dimensions.Add(EducationField);

  BIGrid1.Data:=BIQuery1.Calculate;
  BIChart1.Data:=BIGrid1.Data;

  (BIGrid1.Plugin.GetObject as TBIDBGrid).Fields[1].DisplayLabel:='%';
//  (BIGrid1.Plugin.GetControl as TBITeeGrid).Columns[1].Width.Value:=90;

  BIChart1.Chart.Title.Caption:=EducationField.Name;
end;

procedure TUSADemo_Form.BITChart1AddSeries(Sender: TCustomChartSeries);
begin
  ColorPaletteClick(Self);

  ButtonPen1.LinkPen(WorldSeries.Pen);
end;

procedure TUSADemo_Form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  BIGrid1.Data.Free;
end;

procedure TUSADemo_Form.SetupChart;
begin
  BIChart1.Options.Legend:=TBIChartLegend.Hide;
  BIChart1.Chart.ClipPoints:=False;
  BIChart1.Chart.Gradient.Visible:=False;
  BIChart1.Chart.BackWall.Hide;
  BIChart1.Chart.ParentColor:=True;
end;

procedure TUSADemo_Form.FormCreate(Sender: TObject);
begin
  TGeo.Check;  // Init geographic database

  Education:=TStore.Load('BISamples','US Education 1970-2014 by counties')['USA_Counties_Education'];

  SetupChart;

  Education_Title.ItemIndex:=1;

  Education_Year.ItemIndex:=0;
  Education_YearClick(Self);
end;

procedure TUSADemo_Form.Education_YearClick(Sender: TObject);
begin
  DoQuery;
end;

end.
