unit Unit_Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion>25}
  FMX.Graphics,
  {$ENDIF}

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Controls.Presentation, FMX.ComboEdit,
  {$ENDIF}

  FMX.Layouts, FMX.Dialogs, FMX.TabControl, FMX.StdCtrls, FMX.Edit,

  // TeeBI
  BI.DataItem, BI.Web, BI.JSON, BI.HTML, BI.UI,
  FMXBI.DataControl, FMXBI.Grid,

  // TeeChart and BIChart
  FMXTee.Engine, FMXTee.Procs, FMXTee.Chart, FMXBI.Chart.Plugin, FMXBI.Chart;

type
  TFormWiki = class(TForm)
    TabControl1: TTabControl;
    Layout1: TLayout;
    Label1: TLabel;
    ComboEdit1: TComboEdit;
    Button1: TButton;
    TabItem1: TTabItem;
    BIGrid1: TBIGrid;
    Layout2: TLayout;
    LabelURL: TLabel;
    TabChart: TTabItem;
    BITChart1: TBITChart;
    BIChart1: TBIChart;
    procedure Button1Click(Sender: TObject);
    procedure ComboEdit1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure BIGrid1DataChange(Sender: TObject);
    procedure LabelURLClick(Sender: TObject);
  private
    { Private declarations }

    procedure Beautify(const Data:TDataItem);
    procedure LoadData(const URL:String);
    procedure Search(const Query:String);
  public
    { Public declarations }
  end;

var
  FormWiki: TFormWiki;

implementation

{$R *.fmx}

procedure TFormWiki.ComboEdit1KeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key=vkReturn then
     Button1Click(Self);
end;

procedure TFormWiki.FormDestroy(Sender: TObject);
begin
  BIGrid1.Data.Free;
end;

// Return the first URL of the Wikipedia search results
function FindURL(const AData:TDataItem):String;
var tmp : TDataItem;
begin
  result:='';

  if (AData<>nil) and (AData.Count>0) then
     if AData.Items.Count>3 then
     begin
       tmp:=AData[3];

       if tmp.Items.Count>0 then
       begin
         tmp:=tmp[0];

         if (tmp.Count>0) and (tmp.Kind=TDataKind.dkText) then
            result:=tmp.TextData[0];
       end;
     end;
end;

// Import data from an URL, with automatic content guessing (xml, html, csv, json, etc)
function DataFrom(const URL:String):TDataItem;
begin
  result:=TBIURLSource.From(URL);
end;

// Call Wikipedia online search
procedure TFormWiki.Search(const Query:String);
var tmp : String;
    tmpList : TDataItem;
    tmpURL : String;
begin
  tmp:='https://en.wikipedia.org/w/api.php?action=opensearch';
  tmp:=tmp+'&search='+Query;
  tmp:=tmp+'&limit=1';
  tmp:=tmp+'&namespace=0';
  tmp:=tmp+'&format=json';

  // Search and download results
  tmpList:=TBIURLSource.From(tmp);
  try
    // Try to find a search result URL
    tmpURL:=FindURL(tmpList);

    // Load URL
    if tmpURL<>'' then
       LoadData(tmpURL);
  finally
    tmpList.Free;
  end;
end;

procedure TFormWiki.Beautify(const Data: TDataItem);
var t : Integer;
    tmp : TDataItem;
begin
  if not Data.AsTable then
     for t:=0 to Data.Items.Count-1 do
     begin
       tmp:=Data.Items[t];

       tmp.Name:=IntToStr(t+1)+') '+IntToStr(tmp.Count)+' rows';
     end;
end;

procedure TFormWiki.BIGrid1DataChange(Sender: TObject);
var tmp : TDataItem;
begin
  if BIGrid1.CurrentRow=-1 then
     tmp:=nil
  else
     tmp:=BIGrid1.Data.Items[BIGrid1.CurrentRow];

  BIChart1.Data:=tmp;
end;

procedure TFormWiki.Button1Click(Sender: TObject);
var tmp : String;
begin
  tmp:=Trim(ComboEdit1.Text);

  if tmp<>'' then
  begin
    // Add text to combo for later reuse
    if ComboEdit1.Items.IndexOf(tmp)=-1 then
       ComboEdit1.Items.Add(tmp);

    LabelURL.Text:='';

    if TCommonUI.IsURL(tmp) then
       LoadData(tmp)  // <-- direct load
    else
       Search(tmp);   // <-- Search Wikipedia !
  end;
end;

procedure TFormWiki.LabelURLClick(Sender: TObject);
begin
  if LabelURL.Text<>'' then
     TUICommon.GotoURL(LabelURL,LabelURL.Text);
end;

procedure TFormWiki.LoadData(const URL:String);
var Data : TDataItem;
begin
  Data:=DataFrom(URL);

  // Beautify data a little bit, showing number of rows for each table
  Beautify(Data);

  BIGrid1.Data:=Data;

  LabelURL.Text:=URL;
end;

end.
