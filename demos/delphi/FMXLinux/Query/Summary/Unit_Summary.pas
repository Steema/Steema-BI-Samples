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

  FMXBI.Grid.TeeGrid, // <-- Use TeeGrid !!

  FMX.Dialogs, FMX.Layouts,
  FMXBI.Grid, BI.DataItem, BI.Summary, FMX.StdCtrls,
  FMXBI.DataControl;

type
  TForm17 = class(TForm)
    BIGrid1: TBIGrid;
    Layout1: TLayout;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Demo : TDataItem;

    Summary : TSummary;
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
var ByEmployee : TGroupBy;
    ByCompany : TGroupBy;
begin
  // Load data
  Demo:=TStore.Load('SQLite_Demo');

  // Create Summary
  Summary:=TSummary.Create(Self);

  // Add Sum of Quantity
  Summary.AddMeasure(Demo['"Order Details"']['Quantity'],TAggregate.Sum);

  // Add "By EmployeeID"
  ByEmployee:=Summary.AddGroupBy(Demo['Orders']['EmployeeID']);
  ByEmployee.Layout:=TGroupByLayout.Rows; // <-- optional

  // Add "By CompanyName"
  ByCompany:=Summary.AddGroupBy(Demo['Shippers']['CompanyName']);

  // NOTE: Delphi 10.1 Berlin Firemonkey Grid is capable of displaying
  // sub-tables, thus TGroupByLayout.Columns is supported

//  ByCompany.Layout:=TGroupByLayout.Rows; // <-- optional

  // Show Summary in BIGrid
  BIGrid1.Data:=Summary.NewData;

  // Tell grid to not display cells with duplicate content
  BIGrid1.Duplicates(BIGrid1.Data['EmployeeID'],True);
end;

procedure TForm17.FormDestroy(Sender: TObject);
begin
  {$IFNDEF AUTOREFCOUNT}
  BIGrid1.Data.Free;
  {$ENDIF}
end;

end.
