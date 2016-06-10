unit Import_ByCode;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, BI.VCL.Grid,
  Vcl.ComCtrls, BI.VCL.DataViewer, BI.Data, Data.DB, Datasnap.DBClient,
  Vcl.Grids, Vcl.DBGrids, BI.VCL.DataControl;

type
  TByCode = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button4: TButton;
    Panel3: TPanel;
    PageControl1: TPageControl;
    TabBIGrid: TTabSheet;
    BIGrid1: TBIGrid;
    TabStructure: TTabSheet;
    LBFormat: TListBox;
    Splitter1: TSplitter;
    TabSource: TTabSheet;
    PageExamples: TPageControl;
    TabCSV: TTabSheet;
    MemoCSV: TMemo;
    TabJSON: TTabSheet;
    MemoJSON: TMemo;
    TabXML: TTabSheet;
    MemoXML: TMemo;
    TabDatabase: TTabSheet;
    DBGrid1: TDBGrid;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    procedure LBFormatClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    DataViewer : TDataViewer;

    // Just a helper method
    function CreateData(const AName:String; const AData:TDataArray):TDataItem;

    procedure ImportCSV;
    procedure ImportDatabase;
    procedure ImportExcel;
    procedure ImportJSON;
    procedure ImportXML;
  public
    { Public declarations }

    class procedure Show(const AOwner:TComponent); static;
  end;

implementation

{$R *.dfm}

uses
  BI.Data.CSV, BI.Data.JSON, BI.Data.XML, BI.Data.Excel, BI.Data.DB,
  BI.DataSource,
  FindSampleData;

{ TByCode }

// Helper method to create a TDataItem from an array of TDataItem
function TByCode.CreateData(const AName:String; const AData:TDataArray):TDataItem;
begin
  result:=TBISource.FromData(AData);

  if result<>nil then
     result.Name:=AName;
end;

procedure TByCode.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabBIGrid;
  PageExamples.ActivePage:=TabCSV;

  // Create DataViewer
  DataViewer:=TDataViewer.Embedd(Self,TabStructure,nil);
end;

procedure TByCode.FormDestroy(Sender: TObject);
begin
  // Destroy data (to avoid memory leaks)
  BIGrid1.DestroyData;
end;

procedure TByCode.ImportCSV;
var CSV : TBICSV;
begin
  CSV:=TBICSV.Create;
  try
    CSV.Delimiter:=',';   // <-- Field separator, default is comma
    CSV.Quote:='"';       // <-- Text fields quotes, default is "

    CSV.Header.Headers:=TTextHeaders.Auto;  // <-- Detect first row as headers

    // CSV.Header.Count:=1;  // <-- Number of header lines when Headers=Yes

    CSV.MissingValue:='NA';     // <-- Content that should be considered a "null" value
    CSV.ZeroOneAsBoolean:=True; // <-- When True, CSV 0 and 1 are considered Booleans instead of Integers

    // Class methods for one-line of code importing :
    {
      Data:=TBICSV.FromText('A,B,C'+#13#10+'1,2,3');
      Data:=TBICSV.FromFile('test.csv');
      Data:=TBICSV.FromStrings(Memo1.Lines);
    }

    // Choose the origin of the CSV content:
    {
      CSV.Import('c:\myfolder', True);  // <-- recursive import from a folder files
      CSV.Import(Memo1.Lines);          // <-- import text from TStrings
      CSV.ImportFile('test.csv');       // <-- import from a single file
      CSV.ImportText('A,B'+#13#10+'1,2');  // <-- import from a string
      CSV.ImportURL('http://acme.com/data.csv');  // <-- load from a web URL
    }

    BIGrid1.Data:=CSV.ImportText(MemoCSV.Text);
  finally
    CSV.Free;
  end;
end;

procedure TByCode.ImportDatabase;
var DB : TBIDB;
begin
  DB:=TBIDB.Create; // CreateEngine allows specifiying a different database (FireDAC, UniDAC, etc, etc)
  try
    // DB.Import(FDConnection1);  // <-- Import all datasets from a database Connection

    // Data:=TBIDB.FromDataSet( FDQuery1 );  <-- class method from any TDataset

    DB.ExcludePattern:='SYS*'; // <-- To exclude datasets that match pattern

    BIGrid1.Data:=DB.Import(ClientDataSet1);
  finally
    DB.Free;
  end;
end;

procedure TByCode.ImportExcel;
var Excel : TBIExcel;
begin
  Excel:=TBIExcel.Create;
  try
    Excel.HeaderCount:=2; // <-- Number of Rows that contain column headers

    // Excel.Range:='A1:B14'; // <-- Optional, define portion of Excel sheet

    // Excel.WorkSheet:='Another Sheet'; // <-- Optional, select a single sheet to import

    BIGrid1.Data:=CreateData('Excel Book1', Excel.ImportFile(BISamplesFolder+'\Other\Excel Book1.xls'));

    // Other methods:
    {
      Data:=Excel.Import('c:\myexcel',True); // <-- recursive import of Excel files
      Data:=Excel.ImportURL('http://acme.com/myexcel.xlsx');
      Data:=Excel.ImportFile('mysheet.xls');

      // Class method
      Data:=TBIExcel.FromFile('mysheet.xls');
    }
  finally
    Excel.Free;
  end;
end;

procedure TByCode.ImportJSON;
var JSON : TBIJSON;
begin
  JSON:=TBIJSON.Create;
  try
    JSON.Format:=TBIJSONFormat.Normal;  // <-- "Array" format for MongoDB special row-by-row json

    JSON.MissingValue:='NA';  // <-- Content that should be considered a "null" value
    JSON.ZeroOneAsBoolean:=True;   // <-- When True, CSV 0 and 1 are considered Booleans instead of Integers

    BIGrid1.Data:=CreateData('JSON Data',JSON.Import(MemoJSON.Lines));
  finally
    JSON.Free;
  end;
end;

procedure TByCode.ImportXML;
var XML : TBIXML;
begin
  XML:=TBIXML.Create; // <-- CreatEngine to pass a different XML DOM library
  try
    XML.ExcludePattern:='name';

    XML.MissingValue:='NA';  // <-- Content that should be considered a "null" value
    XML.ZeroOneAsBoolean:=True;   // <-- When True, CSV 0 and 1 are considered Booleans instead of Integers

    BIGrid1.Data:=XML.ImportText(MemoXML.Text);
  finally
    XML.Free;
  end;
end;

procedure TByCode.LBFormatClick(Sender: TObject);
begin
  if LBFormat.ItemIndex<>-1 then
  begin
    // Destroy previous data (to avoid memory leak)
    BIGrid1.DestroyData;

    case LBFormat.ItemIndex of
      0: begin
           PageExamples.ActivePage:=TabCSV;
           ImportCSV;
         end;

      1: begin
           PageExamples.ActivePage:=TabJSON;
           ImportJSON;
         end;

      2: begin
           PageExamples.ActivePage:=TabXML;
           ImportXML;
         end;

      3: begin
           PageExamples.ActivePage:=TabDatabase;
           ImportDatabase;
         end;
    else
      ImportExcel;
    end;

    // Show new data structure at data viewer
    if DataViewer<>nil then
       DataViewer.Select(BIGrid1.Data);
  end;
end;

// Show this form modal
class procedure TByCode.Show(const AOwner: TComponent);
begin
  with TByCode.Create(AOwner) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

end.
