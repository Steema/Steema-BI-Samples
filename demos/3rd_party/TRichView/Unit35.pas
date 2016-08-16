unit Unit35;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.Data, BI.VCL.DataControl,
  BI.VCL.Grid, RVScroll, RichView, RVStyle, Vcl.ExtCtrls, Vcl.StdCtrls,
  RVTable;

type
  TFormRichView = class(TForm)
    RichView1: TRichView;
    BIGrid1: TBIGrid;
    RVStyle1: TRVStyle;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Label1: TLabel;
    ComboMode: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboModeChange(Sender: TObject);
  private
    { Private declarations }

    function FromDataCursor:TRVTableItemInfo;
  public
    { Public declarations }
  end;

var
  FormRichView: TFormRichView;

implementation

{$R *.dfm}

uses
  BI.VCL.Export.RichView, BI.DataSource, BI.Arrays;

procedure TFormRichView.ComboModeChange(Sender: TObject);
var Table : TRVTableItemInfo;
begin
  RichView1.Clear;

  case ComboMode.ItemIndex of
    0: // Add full Grid Data
       Table:=TDataRichView.Add(BIGrid1.Data,RichView1);

    1: // Add a single field
       Table:=TDataRichView.Add(BIGrid1.Data['ProductName'],RichView1);

    2: // Add several fields only
       Table:=TDataRichView.Add([
                                  BIGrid1.Data['ProductName'],
                                  BIGrid1.Data['UnitPrice'],
                                  BIGrid1.Data['CategoryID']
                                ],
                                RichView1);
  else
    // Selected rows and fields, in custom sort order
    Table:=FromDataCursor;
  end;

  // Cosmetic Table borders, colors and spacing
  TDataRichView.DefaultStyle(Table);

  // Show
  RichView1.Format;
end;

procedure TFormRichView.FormCreate(Sender: TObject);
begin
  // Cosmetic text styles
  RVStyle1.TextStyles[0].Size:=8;
  RVStyle1.TextStyles[1].Size:=8;

  // Enable horiz scroll
  RichView1.WordWrap:=False;

  ComboModeChange(Self);
end;

// Advanced Example:
//
// Use a TDataCursor to customize which rows and fields should be exported,
// and in which order (optionally)

function TFormRichView.FromDataCursor:TRVTableItemInfo;
var tmp : TDataCursor;
begin
  // Create a temporary "Cursor"
  tmp:=TDataCursor.Create(nil);
  try
    // Set main Data
    tmp.Data:=BIGrid1.Data;

    // Optional: Add some custom Fields only
    tmp.Add(tmp.Data['ProductName']);
    tmp.Add(tmp.Data['SupplierID']);

    // Optional: Sort data by ProductName (ascending, case-sensitive)
    tmp.SortBy.Add(tmp.Data['ProductName']);

    // Obtain the cursor index
    tmp.PrepareIndex;

    // Example, optional: select the first 10 rows only:
    tmp.Index:=tmp.Index.Copy(0,10);

    // Create a RichView table using our "Cursor"
    result:=TDataRichView.Add(tmp,RichView1);
  finally
    tmp.Free;
  end;
end;

end.
