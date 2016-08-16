unit Main_Unit_Ranks;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.VCL.DataControl, BI.VCL.Grid;

type
  TFormDataRank = class(TForm)
    BIGrid1: TBIGrid;
    Panel1: TPanel;
    CreateRank: TButton;
    Button2: TButton;
    CBHideYears: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CreateRankClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CBHideYearsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    procedure RefreshGrid;
    procedure TryColorizeRanks;
  public
    { Public declarations }
  end;

var
  FormDataRank: TFormDataRank;

implementation

{$R *.dfm}

uses
  BI.Data, BI.Data.Rank, BI.Data.Expressions, BI.UI;

// Try to find an item named "Rank" to destroy it
procedure RemoveItem(const AData:TDataItem);
var tmp : TDataItem;
begin
  tmp:=AData.Items.Find('Rank');

  if tmp<>nil then
     tmp.Free;
end;

procedure TFormDataRank.CreateRankClick(Sender: TObject);
var tmp : TDataItem;
    tmpRank : TDataItem;
begin
  tmp:=BIGrid1.Data;

  // Remove "Rank" item, if it exists
  RemoveItem(tmp);

  // Create a new "Rank" item

  tmpRank:=TDataRank.From(tmp,
                         [tmp['Year']],
                         tmp['Happiness']);

  tmpRank.Name:='Rank';

  // Add "Rank" to table
  tmp.Items.Add(tmpRank);

  RefreshGrid;
end;

function BigRandomTable:TDataItem;
const
  PersonNames:Array[0..4] of String=('Johnny','Mike','Angela','Julia','Claire');

  Max_Year=2000;

var Year,
    Person,
    Happiness : TDataItem;

    tmpRow,
    t,
    p : Integer;
begin
  result:=TDataItem.Create(True);

  Year:=result.Items.Add('Year',TDataKind.dkInt32);
  Person:=result.Items.Add('Person',TDataKind.dkText);
  Happiness:=result.Items.Add('Happiness',TDataKind.dkSingle);

  result.Resize((Max_Year+1)*5);

  for t:=0 to Max_Year do
      for p:=0 to 4 do
      begin
        tmpRow:=(t*5)+p;

        Year.Int32Data[tmpRow]:=2000+t;
        Person.TextData[tmpRow]:=PersonNames[p];
        Happiness.SingleData[tmpRow]:=Random(1000)*0.1;
      end;
end;

// Sort data by two fields (Year and Happiness)
procedure TFormDataRank.Button2Click(Sender: TObject);
var Sort : TSortItems;
begin
  Sort.Clear;
  Sort.Add(BIGrid1.Data['Year']);
  Sort.Add(BIGrid1.Data['Happiness'],False);

  Sort.SortData(BIGrid1.Data);

  RefreshGrid;
end;

procedure TFormDataRank.CBHideYearsClick(Sender: TObject);
begin
  RefreshGrid;
end;

procedure TFormDataRank.FormCreate(Sender: TObject);
begin
  BIGrid1.Data:=BigRandomTable;

  RefreshGrid;
end;

procedure TFormDataRank.FormDestroy(Sender: TObject);
begin
  BIGrid1.Data.Free; // remove used memory
end;

procedure TFormDataRank.TryColorizeRanks;
var tmp : TDataColorizers;
    tmpRank : TDataItem;
begin
  tmp.Clear;

  tmpRank:=BIGrid1.Data.Items.Find('Rank');

  if tmpRank<>nil then
     tmp.Add(tmpRank);

  BIGrid1.Colorize(tmp);
end;

procedure TFormDataRank.RefreshGrid;
begin
  BIGrid1.RefreshData;

  if CBHideYears.Checked then
     BIGrid1.Duplicates(BIGrid1.Data['Year'],True);

  TryColorizeRanks;
end;

end.
