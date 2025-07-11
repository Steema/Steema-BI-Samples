unit Unit_StartHere;

interface

{
   Minimal example showing TeeBI features:

   1) Importing data from a file (csv in this example)

   2) Running a summary query

   3) Showing the query output in a Grid and Chart

   4) Dialogs to edit chart, grid and query properties
}

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,

  // VCL
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  // TeeBI
  BI.DataItem, BI.Persist, BI.Query,

  // VCL TeeBI
  VCLBI.Grid, VCLBI.DataControl, VCLBI.Chart, VCLBI.Chart.Plugin,
  VCLBI.Editor.BIGrid, VCLBI.Editor.Chart, VCLBI.Editor.Query,

  // TeeChart
  VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart, VCLTee.TeeTools;

type
  TMainForm = class(TForm)
    BITChart1: TBITChart;
    BIChart1: TBIChart;
    BIGrid1: TBIGrid;
    Panel1: TPanel;
    Label1: TLabel;
    Splitter1: TSplitter;
    BIQuery1: TBIQuery;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }

    procedure RefreshQuery;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  BI.CSV,
  Unit_SampleData;

var Data1, Data2 : TDataItem;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Load CSV data, or TBIXML, TBIJSON, TBIDB...
  Data1 := TBICSV.FromFile(SampleDataFile);

  BIQuery1.Parse(Data1, 'Sum(Stock) group by Category, Color');

  RefreshQuery;

  BIChart1.Chart.Axes.Bottom.Texts.Angle:=90;
  BIChart1.Chart.View3D:=False;
end;

// Cleanup memory
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Data2.Free;
  Data1.Free;
end;

procedure TMainForm.RefreshQuery;
begin
  Data2.Free;
  Data2 := BIQuery1.Calculate;  // Run the query

  BIGrid1.Data  := Data2;     // Show results at Grid
  BIChart1.Data := Data2;    // Show results at Chart
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  TBIGridEditor.Edit(Self,BIGrid1); // Show the grid editor
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  TBIChartEditor.Edit(Self,BIChart1); // Show the chart editor
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  if TBIQueryEditor.Edit(Self,BIQuery1) then // Show the query editor
     RefreshQuery;
end;

end.
