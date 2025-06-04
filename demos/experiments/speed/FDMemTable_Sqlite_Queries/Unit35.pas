unit Unit35;

interface

{
  Comparing the speed of FireDAC local SQL datasets
    vs
  in-memory TeeBI structures.
}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Phys.SQLiteVDataSet, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.UI.Intf, FireDAC.FMXUI.Wait, FireDAC.Comp.UI, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, System.Rtti, FMX.Grid.Style,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid, FMX.Memo, FMX.Memo.Types,
  FireDAC.Phys.SQLiteWrapper.Stat;

type
  TLocalSQLvsBI = class(TForm)
    Persons: TFDMemTable;
    PersonsID: TIntegerField;
    PersonsName: TWideStringField;
    PersonsSalary: TSingleField;
    FDLocalSQL1: TFDLocalSQL;
    FDQuery1: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDConnection1: TFDConnection;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    procedure TestFireDAC;
    procedure TestTeeBI;
  public
    { Public declarations }
  end;

var
  LocalSQLvsBI: TLocalSQLvsBI;

implementation

{$R *.fmx}

uses
  System.Diagnostics, BI.DataItem, BI.SQL;

const
  SampleNames:Array[0..5] of String=('Sam','Jane','Peter','Carla','Alex','Julie');

  Row_Quantity=1000000;

procedure TLocalSQLvsBI.TestFireDAC;
var t : Integer;
    t1 : TStopwatch;
begin
  // Testing fix for Mac OSX (no difference)
  // https://quality.embarcadero.com/browse/RSP-11827
  // FDConnection1.ResourceOptions.SilentMode:=True;

  Persons.Open;

  Persons.DisableControls;
  try
    t1:=TStopwatch.StartNew;

    // Add rows
    for t:=0 to Row_Quantity-1 do
    begin
      Persons.Insert;

      PersonsID.Value:=t;
      PersonsName.Text:=SampleNames[t mod High(SampleNames)];
      PersonsSalary.Value:=456.789;
      Persons.Post;
    end;

    Memo1.Lines.Add('Create: '+t1.ElapsedMilliseconds.ToString+' msec');
  finally
    Persons.EnableControls;
  end;

  FDLocalSQL1.UseTransactions:=False;

  FDLocalSQL1.Active:=True;

  t1:=TStopwatch.StartNew;

  // Query 10 times
  for t:=0 to 9 do
  begin
    FDQuery1.Close;
    FDQuery1.Open;
  end;

  Memo1.Lines.Add('Query 10 times: '+t1.ElapsedMilliseconds.ToString+' msec');

  FDQuery1.Close;
end;

procedure TLocalSQLvsBI.TestTeeBI;
var t : Integer;
    Persons : TDataItem;
    t1 : TStopwatch;
    Data : TDataItem;
begin
  Persons:=TDataItem.Create(True);
        Persons.Items.Add('ID',TDataKind.dkInt32);
        Persons.Items.Add('Name',TDataKind.dkText);
        Persons.Items.Add('Salary',TDataKind.dkSingle);

  t1:=TStopwatch.StartNew;

  // Prepare space
  Persons.Resize(Row_Quantity);

  // Add all rows
  for t:=0 to Row_Quantity-1 do
  begin
    // Fill row
    Persons[0].Int32Data[t]:=t;
    Persons[1].TextData[t]:=SampleNames[t mod High(SampleNames)];
    Persons[2].SingleData[t]:=456.789;
  end;

  Memo1.Lines.Add('Create: '+t1.ElapsedMilliseconds.ToString+' msec');

  t1:=TStopwatch.StartNew;

  // Query 10 times
  for t:=0 to 9 do
  begin
    Data:=TBISQL.From(Persons,'select * where Name="Alex"');
    try
      Data.Load;
    finally
      Data.Free;
    end;
  end;

  Memo1.Lines.Add('Query 10 times: '+t1.ElapsedMilliseconds.ToString+' msec');
end;

procedure TLocalSQLvsBI.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Add('FireDAC LocalSQL (SQLite)');

  TestFireDAC;

  Memo1.Lines.Add('');
  Memo1.Lines.Add('TeeBI (pure Pascal)');

  TestTeeBI;
end;

end.
