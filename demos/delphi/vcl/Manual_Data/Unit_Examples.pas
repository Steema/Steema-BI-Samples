unit Unit_Examples;

interface

// Several examples of different data structures that can be created using the
// TDataItem class

uses
  BI.DataItem;

function SimpleColumn:TDataItem;
function SimpleTable:TDataItem;
function GroupOfTables:TDataItem;
function NestedTable:TDataItem;

function CreateMaster:TDataItem;
function CreateDetail(const AMaster:TDataItem):TDataItem;

function CreateDetail_Embedded:TDataItem;

implementation

uses
  BI.Arrays;

function SimpleColumn:TDataItem;
begin
  result:=TDataItem.Create(TDataKind.dkText); // <-- "dkXXX" means result is a field (column)
  result.Name:='Simple Column';

  // Allocate space for 2 rows:
  result.Resize(2);

  // Fill these 2 rows:
  result.TextData[0]:='ABC';
  result.TextData[1]:='XYZ';

  // Append new rows:
  result.TextData.Append('Hello');
  result.TextData.Append('World');
end;

function SimpleTable:TDataItem;
begin
  result:=TDataItem.Create(True);  // <-- "True" means result is a table
  result.Name:='Simple Table';

  // Add 3 fields
  result.Items.Add('Field 1',TDataKind.dkInt32);
  result.Items.Add('Field 2',TDataKind.dkBoolean);
  result.Items.Add('Field 3',TDataKind.dkText);

  result.Resize(3);

  // Fill 3 rows, 3 fields

  result[0].Int32Data[0]:=123;
  result[1].BooleanData[0]:=True;
  result[2].TextData[0]:='Apples';

  result[0].Int32Data[1]:=456;
  result[1].BooleanData[1]:=False;
  result[2].TextData[1]:='Oranges';

  result[0].Int32Data[2]:=789;
  result[1].BooleanData[2]:=True;
  result[2].TextData[2]:='Bananas';
end;

function AnotherTable:TDataItem;
begin
  result:=TDataItem.Create(True);  // <-- "True" means result is a table
  result.Name:='Another Table';

  // Add 2 fields
  result.Items.Add('Field A',TDataKind.dkSingle);
  result.Items.Add('Field B',TDataKind.dkText);

  result.Resize(2);

  // Fill 2 rows, 2 fields

  result[0].SingleData[0]:=123.456;
  result[1].TextData[0]:='Car';

  result[0].SingleData[1]:=789.321;
  result[1].TextData[1]:='Bike';
end;

function GroupOfTables:TDataItem;
begin
  result:=TDataItem.Create;  // <-- No parameter means result is a group
  result.Name:='Group of Tables';

  // Add two items (they can be anything: tables, groups or normal fields)

  result.Items.Add(SimpleTable);
  result.Items.Add(AnotherTable);
end;

function NestedTable:TDataItem;
var SomeInt,
    SomeBool,
    SomeText,
    SomeNested : TDataItem;
begin
  result:=TDataItem.Create(True);  // <-- "True" means result is a table
  result.Name:='Nested Table';

  // Add 3 fields.
  // These "Somexxx" variables are just handy to use them later:

  SomeInt:=result.Items.Add('Field 1',TDataKind.dkInt32);
  SomeBool:=result.Items.Add('Field 2',TDataKind.dkBoolean);
  SomeText:=result.Items.Add('Field 3',TDataKind.dkText);

  // Nested tables are just normal TDataItem:
  SomeNested:=AnotherTable;
  result.Items.Add(SomeNested);

  result.Resize(3);

  // Just fill some values
  SomeInt.Int32Data[0]:=42;
  SomeBool.BooleanData[0]:=True;
  SomeText.TextData[0]:='America';

  SomeInt.Int32Data[1]:=99;
  SomeBool.BooleanData[1]:=True;
  SomeText.TextData[1]:='Africa';

  SomeInt.Int32Data[2]:=555;
  SomeBool.BooleanData[2]:=True;
  SomeText.TextData[2]:='Asia';

  // Fill 3rd rows in nested sub-table.

  SomeNested['Field A'].SingleData[2]:=7.654;
  SomeNested['Field B'].TextData[2]:='I''m third';
end;

function CreateMaster:TDataItem;
begin
  // Reuse the "SimpleTable" example, as it is a good example to be a "Master"
  // table
  result:=SimpleTable;
end;

function RandomText:String;
const
  Texts:Array[0..5] of String=
      ('Red','Green','Blue','Yellow','White','Black');
begin
  result:=Texts[Random(Length(Texts))];
end;

// When using different master and detail tables (not "embedded"),
// we must "link" one item in the detail table to another in the master table.
// (This is the concept of an "ID" field, like in relational databases)

function CreateDetail(const AMaster:TDataItem):TDataItem;
var ID : TDataItem;
    t : Integer;
    tmpID : Integer;
begin
  result:=TDataItem.Create(True);
  result.Name:='Detail Table';

  ID:=result.Items.Add('Sub ID',TDataKind.dkInt32);

  result.Items.Add('Sub Field',TDataKind.dkText);

  ID.Master:=AMaster['Field 1'];

  result.Resize(30);

  // Lets fill some random data rows

  for t:=0 to result.Count-1 do
  begin
    // Lets choose one ID in master, randomly:
    tmpID:=Random(AMaster.Count);

    // Link ID to master row
    ID.Int32Data[t]:=ID.Master.Int32Data[tmpID];

    // Just fill with random strings
    result[1].TextData[t]:=RandomText;
  end;
end;

// An embedded detail table does not need "ID" fields to link to a master table
function CreateDetail_Embedded:TDataItem;
var t : Integer;
begin
  result:=TDataItem.Create(True);
  result.Name:='Detail Table';

  result.Items.Add('Sub Field',TDataKind.dkText);
  result.Resize(30);

  // Lets fill rows with random text
  for t:=0 to result.Count-1 do
      result[0].TextData[t]:=RandomText;
end;

end.
