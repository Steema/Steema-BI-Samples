unit Unit35;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.Data, BI.VCL.DataControl,
  BI.VCL.Grid, RVScroll, RichView, RVStyle, Vcl.ExtCtrls, Vcl.StdCtrls;

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
  public
    { Public declarations }
  end;

var
  FormRichView: TFormRichView;

implementation

{$R *.dfm}

uses
  BI.VCL.Export.RichView, RVTable;

procedure TFormRichView.ComboModeChange(Sender: TObject);
var Table : TRVTableItemInfo;
begin
  RichView1.Clear;

  case ComboMode.ItemIndex of
    0: // Add full Grid Data
       Table:=TDataRichView.Add(BIGrid1.Data,RichView1);

    1: // Add a single field
       Table:=TDataRichView.Add(BIGrid1.Data['ProductName'],RichView1);

  else
    // Add several fields
    Table:=TDataRichView.Add([
                                BIGrid1.Data['ProductName'],
                                BIGrid1.Data['UnitPrice'],
                                BIGrid1.Data['CategoryID']
                             ],
                             RichView1);
  end;

  // Cosmetic Table borders
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

end.
