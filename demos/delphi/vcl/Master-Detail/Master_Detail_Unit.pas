unit Master_Detail_Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.ExtCtrls, Vcl.Grids,
  Vcl.DBGrids, BI.Data, BI.DataSet, BI.DataSource, Vcl.StdCtrls, BI.VCL.Grid,
  VclTee.TeeGDIPlus, VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart,
  BI.VCL.Chart;

// Note: This example uses TBIGrid controls to show the extra features of TBIGrid.
//       Standard TDBGrid controls can also be used.

type
  TMasterDetailForm = class(TForm)
    BIGrid1: TBIGrid;
    BIGrid2: TBIGrid;
    Splitter1: TSplitter;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    Panel1: TPanel;
    Panel2: TPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Master: TBIDataset;
    Detail: TBIDataset;
    BIChart1: TBIChart;
    procedure FormShow(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure DetailAfterRefresh(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MasterDetailForm: TMasterDetailForm;

implementation

{$R *.dfm}

uses
  BI.Persist;

procedure TMasterDetailForm.CheckBox1Click(Sender: TObject);
begin
  Master.Active:=CheckBox1.Checked;
end;

procedure TMasterDetailForm.CheckBox2Click(Sender: TObject);
begin
  Detail.Active:=CheckBox2.Checked;
end;

procedure TMasterDetailForm.DetailAfterRefresh(DataSet: TDataSet);
begin
  BIChart1.Init;
  BIChart1.Fill(Detail.Cursor);
end;

procedure TMasterDetailForm.FormShow(Sender: TObject);
var Data : TDataItem;
begin
  // Note: This code is not necessary.
  // The "Data" property of Master and Detail datasets can be
  // selected at design-time.

  Data:=TStore.Load({'Steema',}'SQLite_Demo');

  Master.Data:=Data['Products'];
  Master.Open;

  Detail.Data:=Data['"Order Details"'];
  Detail.Open;
end;

end.
