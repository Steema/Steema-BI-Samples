unit Unit_Main_Gridify;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, BI.VCL.DataControl,
  BI.VCL.Grid, Vcl.StdCtrls, System.Diagnostics;

type
  TFromGridify = class(TForm)
    BIGrid1: TBIGrid;
    BIGrid2: TBIGrid;
    Splitter1: TSplitter;
    LBTest: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure LBTestClick(Sender: TObject);
  private
    { Private declarations }

    procedure AddRank;
    procedure DoGridify;
    procedure TryColorizeRanks;
  public
    { Public declarations }
  end;

var
  FromGridify: TFromGridify;

implementation

{$R *.dfm}

uses
  BI.Data, BI.Data.Gridify, BI.Data.Rank, BI.UI;

function BigRandomTable:TDataItem;
const
  PersonNames:Array[0..4] of String=('Johnny','Mike','Angela','Julia','Claire');
  ColorNames:Array[0..3] of String=('Red','Blue','Yellow','Green');

  Max_Year=500;

var Year,
    Person,
    Colors,
    Happiness : TDataItem;

    tmpRow,
    t,
    p : Integer;
begin
  result:=TDataItem.Create(True);

  Year:=result.Items.Add('Year',TDataKind.dkInt32);
  Person:=result.Items.Add('Person',TDataKind.dkText);
  Happiness:=result.Items.Add('Happiness',TDataKind.dkSingle);
  Colors:=result.Items.Add('Color',TDataKind.dkText);

//  result.Resize((Max_Year+1)*5);

  for t:=0 to Max_Year do
      for p:=0 to 4 do
      if Random(1000)<500 then
      begin
        //tmpRow:=(t*5)+p;

        result.Resize(result.Count+1);
        tmpRow:=result.Count-1;

        Year.Int32Data[tmpRow]:=2016+t;
        Person.TextData[tmpRow]:=PersonNames[p];
        Happiness.SingleData[tmpRow]:=Random(1000)*0.1;
        Colors.TextData[tmpRow]:=ColorNames[Random(Length(ColorNames))];
      end;

  result.SortBy(Happiness);
end;

procedure TFromGridify.TryColorizeRanks;
var tmp : TDataColorizers;
    tmpRank : TDataItem;
begin
  tmp.Clear;

  tmpRank:=BIGrid2.Data.Items.Find('Rank');

  if tmpRank<>nil then
     tmp.Add(tmpRank);

  BIGrid2.Colorize(tmp);
end;

procedure TFromGridify.AddRank;
var tmp,
    tmpRank : TDataItem;
begin
  tmp:=BIGrid1.Data;

  tmpRank:=TDataRank.From(tmp,
                         nil {[tmp['Year']]},
                         tmp['Happiness']);

  tmpRank.Name:='Rank';

  // Add "Rank" to table
  tmp.Items.Add(tmpRank);
end;

procedure TFromGridify.DoGridify;
var Year,
    Person,
    Happiness : TDataItem;

    tmp : TDataItem;

    t1 : TStopWatch;
begin
  Year:=BIGrid1.Data['Year'];
  Person:=BIGrid1.Data['Person'];
  Happiness:=BIGrid1.Data['Happiness'];

  t1:=TStopwatch.StartNew;

  case LBTest.ItemIndex of
    0: tmp:=TGridify.From(Happiness,[Year],[Person]);
    1: tmp:=TGridify.FromItems(BIGrid1.Data,'Happiness',['Year'],['Person']);
    2: tmp:=TGridify.From(BIGrid1.Data,'Happiness','Year','Person');
    3: tmp:=TGridify.From(BIGrid1.Data,'Color','Year','Person');
    4: tmp:=TGridify.From(BIGrid1.Data,'Color','Person','Year');
    5: tmp:=TGridify.From(BIGrid1.Data,'Rank','Year','Person');
    6: tmp:=TGridify.From(BIGrid1.Data,'Rank','Person','Year');
    7: tmp:=TGridify.From(BIGrid1.Data,'Color','Rank','Person');
  end;

  BIGrid2.Data:=tmp;

  Caption:=t1.ElapsedMilliseconds.ToString;
end;

procedure TFromGridify.FormCreate(Sender: TObject);
begin
  BIGrid1.Data:=BigRandomTable;

  AddRank;
  BIGrid1.RefreshData;

  LBTest.ItemIndex:=4;
  LBTestClick(Self);

  TryColorizeRanks;
end;

procedure TFromGridify.LBTestClick(Sender: TObject);
begin
  DoGridify;
end;

end.
