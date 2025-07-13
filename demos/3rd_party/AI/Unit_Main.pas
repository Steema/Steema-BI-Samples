unit Unit_Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Edit, FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMXTee.Engine, FMXTee.Procs, FMXTee.Chart, FMXBI.Chart.Plugin,
  FMXBI.Chart, FMXBI.DataControl, FMXBI.Grid, FMX.Layouts,

  BI.DataItem, BI.AI, BI.XMLData, BI.CSV, BI.JSON, FMX.Objects, FMXTee.Tools;

type
  TForm_AI_Demo = class(TForm)
    Layout1: TLayout;
    BIGrid1: TBIGrid;
    Splitter1: TSplitter;
    BITChart1: TBITChart;
    BIChart1: TBIChart;
    Layout2: TLayout;
    Memo1: TMemo;
    Label1: TLabel;
    CBAgent: TComboBox;
    Label2: TLabel;
    EditAPIKey: TEdit;
    Button1: TButton;
    Text1: TText;
    Text2: TText;
    ComboExample: TComboBox;
    procedure EditAPIKeyChangeTracking(Sender: TObject);
    procedure CBAgentChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Text2Click(Sender: TObject);
    procedure ComboExampleChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

    APIKey : String;
  public
    { Public declarations }
  end;

var
  Form_AI_Demo: TForm_AI_Demo;

implementation

{$R *.fmx}

uses
  System.IOUtils;

procedure TForm_AI_Demo.Button1Click(Sender: TObject);
var Data1 : TDataItem;
begin
  TeeChangeCursor(Self,crHourGlass);
  try
    // One line of code, call Gemini and get a data structure:
    Data1:=TBIAI.From(EditAPIKey.Text, Memo1.Text.Trim, TBIAI.GoogleGemini);

    // Visualize it:
    BIGrid1.Data:=Data1;
    BIChart1.Data:=Data1;
  finally
    TeeChangeCursor(Self,crDefault);
  end;
end;

procedure TForm_AI_Demo.CBAgentChange(Sender: TObject);
begin
  EditAPIKey.Text:='';
end;

procedure TForm_AI_Demo.ComboExampleChange(Sender: TObject);
const
  Example1 = 'Give me the list of the highest 10 mountains by elevation in csv format, just the list';
  Example2 = 'I want a list of car sales worldwide this year, by month and by brand (top 10 brands), in csv format, just the list';

begin
  case ComboExample.ItemIndex of
    0: Memo1.Text:=Example1;
    1: Memo1.Text:=Example2;
  else
    Memo1.Text:='';
  end;
end;

procedure TForm_AI_Demo.EditAPIKeyChangeTracking(Sender: TObject);
begin
  APIKey:=EditAPIKey.Text.Trim;

  Button1.Enabled:=(APIKey<>'') and (Memo1.Text.Trim<>'');
end;

// Just a handy trick, put your key on this file to avoid always typing it:
procedure TForm_AI_Demo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  BIGrid1.Data:=nil; // <-- just to skip a FMX probable AV bug at TModel Cache

  BIChart1.Data.Free;
end;

procedure TForm_AI_Demo.FormCreate(Sender: TObject);
var KeyFile : String;
begin
  KeyFile:=TPath.Combine(TPath.GetDocumentsPath,'Gemini_Key.txt');

  if TFile.Exists(KeyFile) then
     EditAPIKey.Text:=TFile.ReadAllText(KeyFile);
end;

procedure TForm_AI_Demo.Text2Click(Sender: TObject);
begin
  TeeGotoURL(0,Text2.Text);
end;

end.
