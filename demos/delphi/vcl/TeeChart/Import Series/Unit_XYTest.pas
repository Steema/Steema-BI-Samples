unit Unit_XYTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VCLTee.TeEngine,
  VCLTee.Series, VCLBI.Grid, Vcl.ExtCtrls, VCLTee.TeeProcs,
  VCLTee.Chart, VCLBI.Chart, VCLBI.Chart.Plugin, VCLBI.DataControl,
  VCLTee.TeeTools;

type
  TFormXYTest = class(TForm)
    BIGrid1: TBIGrid;
    Panel1: TPanel;
    Splitter1: TSplitter;
    BIChart1: TBIChart;
    Chart1: TChart;
    BITChart1: TBITChart;
    Series1: TPointSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormXYTest.FormCreate(Sender: TObject);
var t : Integer;
begin
  // Fill series with random XY values
  Series1.Clear;

  Series1.XValues.Order:=TChartListOrder.loNone;

  for t:=1 to 100 do
      Series1.AddXY(Random(1000),Random(1000));

  // Convert Series1 to a TDataItem
  BIGrid1.Data:=TChartData.From(Series1);

  // Convert a TDataItem to a new Series
  Chart1.AddSeries(TChartData.From(BIGrid1.Data, Self, TPointSeries));

  // Cosmetic settings:
  (Chart1[0] as TPointSeries).Pointer.Style:=psCircle;
  Chart1[0].ColorEachPoint:=True;
  Chart1.View3D:=False;

  // Free Series1
  Series1.Free;

  // Test automatic charting of Grid data
  BIChart1.Data:=BIGrid1.Data;
end;

procedure TFormXYTest.FormDestroy(Sender: TObject);
begin
  BIGrid1.Data.Free;
end;

end.
