unit Unit_Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  BI.FMX.DataControl, BI.FMX.Grid, FMX.TabControl, FMX.StdCtrls, FMX.Edit,
  FMX.ComboEdit, FMX.Controls.Presentation, FMX.Layouts,

  BI.Data, BI.Web, BI.Data.JSON, BI.Data.HTML;

type
  TForm35 = class(TForm)
    TabControl1: TTabControl;
    Layout1: TLayout;
    Label1: TLabel;
    ComboEdit1: TComboEdit;
    Button1: TButton;
    TabItem1: TTabItem;
    BIGrid1: TBIGrid;
    Layout2: TLayout;
    LabelURL: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure ComboEdit1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    procedure Search(const Query:String);
  public
    { Public declarations }
  end;

var
  Form35: TForm35;

implementation

{$R *.fmx}

procedure TForm35.ComboEdit1KeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key=vkReturn then
     Button1Click(Self);
end;

procedure TForm35.FormDestroy(Sender: TObject);
begin
  BIGrid1.Data.Free;
end;

function FindURL(const AData:TDataItem):String;
var tmp : TDataItem;
begin
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

function DataFrom(const URL:String):TDataItem;
begin
  result:=TBIURLSource.From(URL);
end;

procedure TForm35.Search(const Query:String);
var tmp : String;
    tmpList : TDataItem;
    tmpURL : String;
begin
  tmp:='https://en.wikipedia.org/w/api.php?action=opensearch';
  tmp:=tmp+'&search='+Query;
  tmp:=tmp+'&limit=1';
  tmp:=tmp+'&namespace=0';
  tmp:=tmp+'&format=json';

  tmpList:=TBIURLSource.From(tmp);
  try
    tmpURL:=FindURL(tmpList);

    if tmpURL<>'' then
    begin
      BIGrid1.Data:=DataFrom(tmpURL);
      LabelURL.Text:=tmpURL;
    end;

  finally
    tmpList.Free;
  end;
end;

procedure TForm35.Button1Click(Sender: TObject);
var tmp : String;
begin
  tmp:=Trim(ComboEdit1.Text);

  if tmp<>'' then
  begin
    if ComboEdit1.Items.IndexOf(tmp)=-1 then
       ComboEdit1.Items.Add(tmp);

    LabelURL.Text:='';

    Search(tmp);
  end;
end;

end.
