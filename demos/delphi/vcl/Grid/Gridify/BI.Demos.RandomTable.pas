unit BI.Demos.RandomTable;

interface

// Returns a random table filled with random values.

{
  The table structure is:

  Year           (Integer)
  Person         (Text)
  Happiness      (Single float)
  Color          (Text)

  After filling the table, it is sorted by "Happiness" in ascending order
}

uses
  BI.Data;

function BigRandomTable:TDataItem;

implementation

function BigRandomTable:TDataItem;
const
  PersonNames:Array[0..4] of String=('Johnny','Mike','Angela','Julia','Claire');
  ColorNames:Array[0..3] of String=('Red','Blue','Yellow','Green');

  Max_Year=500;

var Year,
    Person,
    Colors,
    Happiness : TDataItem;

    Row,
    t,
    p : Integer;
begin
  // Create table
  result:=TDataItem.Create(True);

  // Add fields (structure)
  Year:=result.Items.Add('Year',TDataKind.dkInt32);
  Person:=result.Items.Add('Person',TDataKind.dkText);
  Happiness:=result.Items.Add('Happiness',TDataKind.dkSingle);
  Colors:=result.Items.Add('Color',TDataKind.dkText);

  // Fill rows
  for t:=0 to Max_Year do
  begin
    for p:=0 to 4 do
    if Random(1000)<500 then // just to skip adding some rows
    begin

      // Resize table
      result.Resize(result.Count+1);
      Row:=result.Count-1;

      // Set field values for new rows

      Year.Int32Data[Row]:=2016+t;
      Person.TextData[Row]:=PersonNames[p];
      Happiness.SingleData[Row]:=Random(1000)*0.1;
      Colors.TextData[Row]:=ColorNames[Random(Length(ColorNames))];
    end;
  end;

  // Sort table by "Happiness" field, ascending
  result.SortBy(Happiness);
end;

end.
