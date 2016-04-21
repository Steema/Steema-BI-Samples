unit ImportData;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TImportDataMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Panel1: TPanel;
    Button4: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Button5: TButton;
    Label6: TLabel;
    Button6: TButton;
    Label7: TLabel;
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ImportDataMain: TImportDataMain;

implementation

{$R *.dfm}

uses Automatic_File_Import, Import_ByCode, Import_FromComponents, Import_Store;

procedure TImportDataMain.Button1Click(Sender: TObject);
begin
  TImportDemoForm.Show(Self);
end;

procedure TImportDataMain.Button2Click(Sender: TObject);
begin
  TByCode.Show(Self);
end;

procedure TImportDataMain.Button3Click(Sender: TObject);
begin
  TFromComponents.Show(Self);
end;

procedure TImportDataMain.Button4Click(Sender: TObject);
begin
  Close;
end;

procedure TImportDataMain.Button5Click(Sender: TObject);
begin
  TFromBIStore.Show(Self);
end;

procedure TImportDataMain.Button6Click(Sender: TObject);
begin
  TFromRTTI.Show(Self);
end;

end.
