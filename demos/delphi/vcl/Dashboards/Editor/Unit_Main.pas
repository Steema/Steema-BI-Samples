unit Unit_Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  BI.VCL.Editor.Template, BI.Dashboard.HTML,
  BI.Dashboard, BI.VCL.Dashboard, BI.VCL.Dashboard.Chart, BI.VCL.Editor.Chart;

type
  TForm34 = class(TForm)
    BIVisual1: TBIVisual;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }

    procedure LoadTemplate(const AFileName:String);
  public
    { Public declarations }
  end;

var
  Form34: TForm34;

implementation

{$R *.dfm}

uses
  System.IOUtils, BI.Dashboard.Loader;

function SampleTemplates:String;
var tmp : String;
begin
  tmp:=GetCurrentDir;

  repeat
    result:=TPath.Combine(tmp,'Templates');

    if TDirectory.Exists(result) then
       Exit
    else
       tmp:=TDirectory.GetParent(tmp);
  until tmp='';
end;

procedure TForm34.Button1Click(Sender: TObject);
begin
  TTemplateEditor.Edit(Self,BIVisual1.Template);
end;

procedure TForm34.LoadTemplate(const AFileName:String);
begin
  TTemplateLoader.FromJSONFile(AFileName,BIVisual1.Template);

  if BIVisual1.Dashboards.Count>0 then
     BIVisual1.Dashboard:=BIVisual1.Dashboards[0];
end;

procedure TForm34.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
     LoadTemplate(OpenDialog1.FileName);
end;

procedure TForm34.FormCreate(Sender: TObject);
begin
  OpenDialog1.InitialDir:=SampleTemplates;

  LoadTemplate(TPath.Combine(OpenDialog1.InitialDir,'customers.json'));
end;

end.
