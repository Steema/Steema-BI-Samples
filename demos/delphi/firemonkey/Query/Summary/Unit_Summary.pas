unit Unit_Summary;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, FMX.Layouts, FMX.Grid,
  FMXBI.Grid, BI.DataItem, BI.Summary, FMX.StdCtrls,
  FMXBI.DataControl;

type
  TMain_Form = class(TForm)
    BIGrid1: TBIGrid;
    Layout1: TLayout;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Demo : TDataItem;

    function Grid:TGrid;
  public
    { Public declarations }
  end;

var
  Main_Form: TMain_Form;

implementation

{$R *.fmx}

uses
  BI.Persist, BI.DataSource, Unit_Histogram_Text, FMXBI.Grid.Grid;

procedure TMain_Form.Button1Click(Sender: TObject);
begin
  with TFormHistogramText.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMain_Form.FormCreate(Sender: TObject);
var Summary : TSummary;
    ByCustomer : TGroupBy;
    ByCompany : TGroupBy;
begin
  // Load data
  Demo:=TStore.Load('SQLite_Demo');

  // Create Summary
  Summary:=TSummary.Create(Self);

  // Add Sum of Quantity
  Summary.AddMeasure(Demo['"Order Details"']['Quantity'],TAggregate.Sum);

  // Assign a name for Field,which will appear at the Column Header
  Summary.Measures[0].Name := 'Sum of Quantity of Orders';

  // Add "By CustomerID"
  ByCustomer:=Summary.AddGroupBy(Demo['Orders']['CustomerID']);
  ByCustomer.Layout:=TGroupByLayout.Rows; // <-- optional

  // Add "By CompanyName" to show grouped by Shippers Company
  ByCompany:=Summary.AddGroupBy(Demo['Shippers']['CompanyName']);
  ByCompany.Name := 'Shippers';

  // NOTE: Delphi 10.1 Berlin and up Firemonkey Grid is capable of displaying
  // sub-tables, thus TGroupByLayout.Columns is supported

  ByCompany.Layout:=TGroupByLayout.Rows; // <-- optional

  // Show Summary in BIGrid
  BIGrid1.Data:=Summary.NewData;

  // Tell grid to not display cells with duplicate content
  BIGrid1.Duplicates(BIGrid1.Data['CustomerID'],True);

  // Accessing Grid properties and methods
  Grid.Columns[0].Width := Grid.Columns[0].Width + 25;
  Grid.Columns[1].Width := Grid.Columns[0].Width + 25;
  Grid.Columns[2].Width := 150;
end;

procedure TMain_Form.FormDestroy(Sender: TObject);
begin
  BIGrid1.Data.Free;
end;

function TMain_Form.Grid: TGrid;
begin
  if BIGrid1.Plugin.GetObject is TBIFMXGrid then
     result:=(BIGrid1.Plugin.GetObject as TBIFMXGrid).Grid
  else
     result:=nil;
end;

end.
