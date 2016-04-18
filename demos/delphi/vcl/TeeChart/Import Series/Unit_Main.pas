unit Unit_Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VCLTee.TeEngine,
  VCLTee.Series, BI.VCL.Grid, Vcl.ExtCtrls, VCLTee.TeeProcs, VCLTee.Chart,
  BI.VCL.Chart;

type
  TSeriesImport = class(TForm)
    BIChart1: TBIChart;
    BIGrid1: TBIGrid;
    Series1: TPieSeries;
    Chart1: TChart;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SeriesImport: TSeriesImport;

implementation

{$R *.dfm}

procedure TSeriesImport.FormCreate(Sender: TObject);
begin
  // Convert Series1 to a TDataItem
  BIGrid1.Data:=TChartData.From(Series1);

  // Convert a TDataItem to a new Series
  Chart1.AddSeries(TChartData.From(BIGrid1.Data, Self, TPieSeries));
end;

procedure TSeriesImport.FormDestroy(Sender: TObject);
begin
  BIGrid1.Data.Free;
end;

end.
