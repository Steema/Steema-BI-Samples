unit Unit_BIWeb_Tests;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.VCL.DataControl, BI.VCL.Grid;

type
  TFormBIWebTests = class(TForm)
    Panel1: TPanel;
    BRun: TButton;
    LBTest: TListBox;
    Button1: TButton;
    SaveDialog1: TSaveDialog;
    CBHost: TComboBox;
    BIGrid1: TBIGrid;
    Panel2: TPanel;
    EURL: TEdit;
    BOpenURL: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BRunClick(Sender: TObject);
    procedure LBTestClick(Sender: TObject);
    procedure BOpenURLClick(Sender: TObject);
    procedure CBHostChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormBIWebTests: TFormBIWebTests;

implementation

{$R *.dfm}

uses
  BI.Web.Tests, System.Diagnostics;

procedure TFormBIWebTests.BRunClick(Sender: TObject);
var t1 : TStopwatch;
begin
  t1:=TStopwatch.StartNew;
  TBIWebTests.Run(CBHost.Text);

  Caption:='Time: '+t1.ElapsedMilliseconds.ToString+' msec';
end;

procedure TFormBIWebTests.Button1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
     TBIWebTests.SaveToHTML(SaveDialog1.FileName,CBHost.Text);
end;

procedure TFormBIWebTests.CBHostChange(Sender: TObject);
begin
  LBTest.ItemIndex:=-1;
end;

procedure TFormBIWebTests.BOpenURLClick(Sender: TObject);
begin
  TUICommon.GotoURL(Self,EURL.Text);
end;

procedure TFormBIWebTests.FormCreate(Sender: TObject);
var tmp : TBIWebTest;
begin
  CBHost.ItemIndex:=0;

  for tmp in TBIWebTests.Tests do
      LBTest.Items.Add(tmp.Description);
end;

procedure TFormBIWebTests.LBTestClick(Sender: TObject);
var tmp : TBIWebTest;
begin
  if LBTest.ItemIndex<>-1 then
  begin
    tmp:=TBIWebTests.Tests[LBTest.ItemIndex];

    EURL.Text:=TBIWebTests.URL(tmp,CBHost.Text);

    BIGrid1.Data:=TBIWebTests.GetData(tmp.URL,CBHost.Text);

    BOpenURL.Enabled:=EURL.Text<>'';
  end;
end;

end.
