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
    Label2: TLabel;
    EditAPIKey: TEdit;
    Button1: TButton;
    Text1: TText;
    TextKeyLink: TText;
    ComboExample: TComboBox;
    ListAgents: TListBox;
    Label1: TLabel;
    procedure EditAPIKeyChangeTracking(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TextKeyLinkClick(Sender: TObject);
    procedure ComboExampleChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListAgentsChange(Sender: TObject);
  private
    { Private declarations }

    function AI_Agent:TBIAI.TAgent;
    procedure ReadPrivateAPIKeys;
  public
    { Public declarations }
  end;

var
  Form_AI_Demo: TForm_AI_Demo;

implementation

{$R *.fmx}

uses
  System.IOUtils;

function TForm_AI_Demo.AI_Agent:TBIAI.TAgent;
begin
  case ListAgents.ItemIndex of
    0 : result:=TBIAI.TAgent.Gemini;
    1 : result:=TBIAI.TAgent.Copilot;
    2 : result:=TBIAI.TAgent.ChatGPT;
  else // 3 :
    result:=TBIAI.TAgent.Grok;
  end;
end;

procedure TForm_AI_Demo.Button1Click(Sender: TObject);
var Data1 : TDataItem;
begin
  TeeChangeCursor(Self,crHourGlass);
  try
    // One line of code, call Gemini and get a data structure:
    Data1:=TBIAI.From(Memo1.Text.Trim, AI_Agent,EditAPIKey.Text);

    // Visualize it:
    BIGrid1.Data:=Data1;
    BIChart1.Data:=Data1;
  finally
    TeeChangeCursor(Self,crDefault);
  end;
end;

procedure TForm_AI_Demo.ComboExampleChange(Sender: TObject);
const
  Example1 = 'Give me the list of the highest 10 mountains by elevation in csv format, mountain name and height, just the list';
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
  Button1.Enabled:=(EditAPIKey.Text.Trim<>'') and (Memo1.Text.Trim<>'');
end;

// Just a handy trick, put your key on this file to avoid always typing it:
procedure TForm_AI_Demo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  BIGrid1.Data:=nil; // <-- just to skip a FMX probable AV bug at TModel Cache

  BIChart1.Data.Free;
end;

var
  APIKeys:Array[0..3] of String;

// Just a convenient method to read keys from your Documents folder, to avoid
// typing the API key always at the edit box
procedure TForm_AI_Demo.ReadPrivateAPIKeys;
var t : Integer;
    KeyFile : String;
begin
  for t:=0 to ListAgents.Count-1 do
  begin
    KeyFile:=TPath.Combine(TPath.GetDocumentsPath,ListAgents.Items[t]+'_Key.txt');

    if TFile.Exists(KeyFile) then
       APIKeys[t]:=TFile.ReadAllText(KeyFile);
  end;
end;

procedure TForm_AI_Demo.FormCreate(Sender: TObject);
begin
  ReadPrivateAPIKeys;

  ListAgentsChange(Self);
  ComboExampleChange(Self);
end;

const
  APILinks:Array[0..3] of String=(
    'http://aistudio.google.com/app/apikey',  // Gemini
    '',  // Copilot
    'https://platform.openai.com/welcome?step=create',  // ChatGPT
    'https://accounts.x.ai/sign-in?redirect=cloud-console' // Grok
    );

procedure TForm_AI_Demo.ListAgentsChange(Sender: TObject);
begin
  EditAPIKey.Text:=APIKeys[ListAgents.ItemIndex];
  TextKeyLink.Text:=APILinks[ListAgents.ItemIndex];
end;

procedure TForm_AI_Demo.TextKeyLinkClick(Sender: TObject);
begin
  TeeGotoURL(0,TextKeyLink.Text);
end;

end.
