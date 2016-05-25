unit Import_FromComponents;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, BI.VCL.Grid,
  Vcl.ComCtrls, Data.DB, Datasnap.DBClient, BI.VCL.DataControl;

type
  TFromComponents = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button4: TButton;
    PageControl1: TPageControl;
    TabMemo: TTabSheet;
    Memo1: TMemo;
    BIGrid1: TBIGrid;
    TabDataset: TTabSheet;
    ClientDataSet1: TClientDataSet;
    BIGrid2: TBIGrid;
    procedure Button4Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    procedure ImportDataset;
  public
    { Public declarations }

    class procedure Show(const AOwner: TComponent); static;
  end;

implementation

{$R *.dfm}

uses
  BI.DataSource, BI.Data.DB, BI.Data.CSV;

procedure TFromComponents.Button4Click(Sender: TObject);
begin
  Close;
end;

procedure TFromComponents.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabMemo;

  // Start demo importing Memo1 contents
  Memo1Change(Self);
end;

procedure TFromComponents.FormDestroy(Sender: TObject);
begin
  BIGrid1.DestroyData;
  BIGrid2.DestroyData;
end;

procedure TFromComponents.ImportDataset;
begin
  BIGrid2.DestroyData;

  BIGrid2.Data:=TBIDB.From(ClientDataSet1);
end;

procedure TFromComponents.Memo1Change(Sender: TObject);
begin
  BIGrid1.DestroyData;

  BIGrid1.Data:=TBICSV.FromStrings(Memo1.Lines);
end;

procedure TFromComponents.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabMemo then
     Memo1Change(Self)
  else
  if PageControl1.ActivePage=TabDataset then
     ImportDataset;
end;

class procedure TFromComponents.Show(const AOwner: TComponent);
begin
  with TFromComponents.Create(AOwner) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

end.
