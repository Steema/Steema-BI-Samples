unit Unit_Main_Gridify;

{
  Example of TDataRank and TGridify features.

  TDataRank creates a "ranking" of values with optional "group by" fields.
  A ranking is a sequential number: 1 2 3 4 ...

  (For example, ranking of "Salary" groupped by "Country")


  TGridify converts a flat table into a grid table, with one or more fields
  used for "rows", and for "columns".

  Another field is used to fill the grid cells.

}

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
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LBTestClick(Sender: TObject);
    procedure BIGrid2Resize(Sender: TObject);
  private
    { Private declarations }

    procedure AddRank;
    procedure DoGridify(const TestNumber:Integer);
    procedure TryColorizeRanks;
  public
    { Public declarations }
  end;

var
  FromGridify: TFromGridify;

implementation

{$R *.dfm}

uses
  BI.Data, BI.Data.Gridify, BI.Data.Rank, BI.UI, BI.Arrays.Strings,
  BI.Demos.RandomTable;

// Use the grid "Colorize" feature on "Rank" field (if available)
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

// Creates a new TDataItem using TDataRank, based on "Happiness" values
procedure TFromGridify.AddRank;
var tmp,
    tmpRank : TDataItem;
begin
  tmp:=BIGrid1.Data;

  tmpRank:=TDataRank.From(tmp,
                         nil {[tmp['Year']]},
                         tmp['Happiness']);

  tmpRank.Name:='Rank';

  // Add new "Rank" field to table
  tmp.Items.Add(tmpRank);
end;

{$IF CompilerVersion>27}
{$DEFINE XE7}
{$ENDIF}

// Execute TGridify tests
procedure TFromGridify.BIGrid2Resize(Sender: TObject);
begin
  // Cosmetic
  Label3.Left:=BIGrid2.Left;
end;

procedure TFromGridify.DoGridify(const TestNumber:Integer);
var Year,
    Person,
    Happiness : TDataItem;

    tmp : TDataItem;

    {$IFNDEF XE7}
    DataRows, DataColumns : TDataArray;
    RowNames, ColumnNames : TStringArray;
    {$ENDIF}
begin
  Year:=BIGrid1.Data['Year'];
  Person:=BIGrid1.Data['Person'];
  Happiness:=BIGrid1.Data['Happiness'];

  case TestNumber of

    {$IFDEF XE7}
    0: tmp:=TGridify.From(Happiness,[Year],[Person]);
    1: tmp:=TGridify.FromItems(BIGrid1.Data,'Year',['Color'],['Person']);

    {$ELSE}
    // Pre-XE7 limitation, cannot pass arrays inline

    0: begin
         SetLength(DataRows,1);
         DataRows[0]:=Year;

         SetLength(DataColumns,1);
         DataColumns[0]:=Person;

         tmp:=TGridify.From(Happiness,DataRows,DataColumns);
       end;

    1: begin
         SetLength(RowNames,1);
         RowNames[0]:='Color';

         SetLength(ColumnNames,1);
         ColumnNames[0]:='Person';

         tmp:=TGridify.FromItems(BIGrid1.Data,'Year',RowNames,ColumnNames);
       end;

    {$ENDIF}

    2: tmp:=TGridify.From(BIGrid1.Data,'Person','Year','Color');
    3: tmp:=TGridify.From(BIGrid1.Data,'Color','Year','Person');
    4: tmp:=TGridify.From(BIGrid1.Data,'Color','Person','Year');
    5: tmp:=TGridify.From(BIGrid1.Data,'Rank','Year','Person');
    6: tmp:=TGridify.From(BIGrid1.Data,'Rank','Person','Year');
    7: tmp:=TGridify.From(BIGrid1.Data,'Color','Rank','Person');
  else
    tmp:=nil;
  end;

  BIGrid2.Data:=tmp;
end;

procedure TFromGridify.FormCreate(Sender: TObject);
begin
  BIGrid1.Data:=BigRandomTable;

  AddRank;
  BIGrid1.RefreshData;

  // Start with test number 5
  LBTest.ItemIndex:=5;
  LBTestClick(Self);
end;

procedure TFromGridify.LBTestClick(Sender: TObject);
var t1 : TStopWatch;
begin
  t1:=TStopwatch.StartNew;

  DoGridify(LBTest.ItemIndex);

  Label1.Caption:='Time: '+t1.ElapsedMilliseconds.ToString+' msec';

  TryColorizeRanks;
end;

end.
