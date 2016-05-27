unit Unit_Summary;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  BI.FMX.Grid, BI.Data, BI.Summary, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm17 = class(TForm)
    BIGrid1: TBIGrid;
    Layout1: TLayout;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    Demo : TDataItem;
  public
    { Public declarations }
  end;

var
  Form17: TForm17;

implementation

{$R *.fmx}

uses
  BI.Persist, BI.DataSource, Unit_Histogram_Text;

procedure TForm17.Button1Click(Sender: TObject);
begin
  with TFormHistogramText.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TForm17.FormCreate(Sender: TObject);
var Summary : TSummary;
begin
  Demo:=TStore.Load('SQLite_Demo');

  Summary:=TSummary.Create;
  Summary.AddMeasure(Demo['"Order Details"']['Quantity'],TAggregate.Sum);

  Summary.AddGroupBy(Demo['Orders']['EmployeeID']).Layout:=TGroupByLayout.Rows;
  Summary.AddGroupBy(Demo['Shippers']['CompanyName']).Layout:=TGroupByLayout.Rows;

  BIGrid1.Data:=TDataItem.Create(Summary);

  BIGrid1.Duplicates(BIGrid1.Data['EmployeeID'],True);
end;

end.
