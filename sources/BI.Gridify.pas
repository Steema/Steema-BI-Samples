{*********************************************}
{  TeeBI Software Library                     }
{  Flat table to pivot-grid algorithm         }
{  Copyright (c) 2016-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Gridify;

interface

(*
  TDataGrid contains methods to convert a normal flat-table into a pivot-grid,
  that is, it returns a new TDataItem that has rows and columns based on values
  of the specified parameters, and cell values from another TDataItem parameter.

  Example:

  BIGrid2.Data:= TGridify.From(BIGrid1.Data, 'Happiness', 'Year', 'Person')

  From this BIGrid1.Data original data:

  -----------------------
  Year  Person Happiness

  2016  Alice    21
  2016  Bob      14
  2016  Mike     39
  2018  Bob      65
  2018  Mike     80
  2018  Alice     3
  2018  Bob       9
  2018  Mike      4




  Output is:

  -----------------------
         Happiness
  Year   Person
         Alice Bob Mike

  2016    21    14   39
  2018          65   80
  2018     3     9    4
  -----------------------

  Source data does not need to be sorted.

  More than one field can be used for rows, columns and values
  (see overload methods).

*)

uses
  BI.DataItem, BI.Arrays.Strings;

type
  TGridify=record
  public
    {
     Explanation of parameters:

     AValue = Field to use as grid cells value
     AData = Table that contains source fields
     ARow / ARows = Fields that will be used at left side (vertical direction)
     ACol / ACols = Fields that will be used at top side (horizontal direction)
    }

    // ADest is the target TDataItem that will contain the "gridify" output
    class procedure Calculate(const ADest:TDataItem;
                              const AValue:TDataItem;
                              const ARows,ACols:TDataArray); static;

    class function From(const AValue:TDataItem;
                        const ARows,ACols:TDataArray):TDataItem; overload; static;

    class function From(const AData:TDataItem;
                        const AValue,ARow,ACol:String):TDataItem; overload; static;

    class function FromItems(const AData:TDataItem;
                        const AValue:String;
                        const ARows,ACols:TStringArray):TDataItem; static;
  end;

implementation

uses
  BI.Arrays, BI.Summary;

{ TDataGrid }

class procedure TGridify.Calculate(const ADest:TDataItem;
                                   const AValue:TDataItem;
                                   const ARows,ACols:TDataArray);

  // Do the "gridify" using a TSummary
  procedure DoCalculate;
  var t : Integer;
      tmpSum : TSummary;
      tmpFirst : TMeasure;
  begin
    tmpSum:=TSummary.Create(nil);
    try
      // Add all rows as group-by
      for t:=0 to ARows.Count-1 do
          tmpSum.AddGroupBy(ARows[t]).Layout:=TGroupByLayout.Rows;

      // Add all columns as group-by
      for t:=0 to ACols.Count-1 do
          tmpSum.AddGroupBy(ACols[t]).Layout:=TGroupByLayout.Items;

      // Add the AValue as measure
      tmpFirst:=tmpSum.AddMeasure(AValue,TAggregate.First);

      // Execute summary over ADest table
      tmpSum.Calculate(ADest);

      // Cosmetic, set measure name to match AValue's name
      tmpFirst.DestData.Name:=AValue.Name;

    finally
      tmpSum.Free;
    end;
  end;

  procedure DoRaise(const AMessage:String);
  begin
    raise EBIException.Create('Error: '+AMessage);
  end;

begin
  if AValue=nil then
     DoRaise('Value item is nil')
  else
  if (ARows.Count>0) and (ACols.Count>0) then
     DoCalculate
  else
     DoRaise('At least one row and one column item are needed');
end;

class function TGridify.From(const AValue: TDataItem;
                             const ARows,ACols: TDataArray): TDataItem;
begin
  result:=TDataItem.Create;
  Calculate(result,AValue,ARows,ACols);
end;

// Single field parameters
class function TGridify.From(const AData:TDataItem; const AValue,ARow,ACol:String):TDataItem;
var tmpRows,
    tmpCols : TDataArray;
begin
  {$IFDEF FPC}
  tmpRows:=nil;
  tmpCols:=nil;
  {$ENDIF}

  tmpRows.Add(AData[ARow]);
  tmpCols.Add(AData[ACol]);

  result:=From(AData[AValue],tmpRows,tmpCols);
end;

// Multiple field parameters
class function TGridify.FromItems(const AData: TDataItem; const AValue: String;
  const ARows, ACols: TStringArray): TDataItem;

  // Find all ANames in AData and return their items
  function ToArray(const ANames:TStringArray):TDataArray;
  var s : String;
  begin
    {$IFDEF FPC}
    result:=nil;
    {$ENDIF}

    for s in ANames do
        result.Add(AData[s]);
  end;

begin
  result:=From(AData[AValue],ToArray(ARows),ToArray(ACols));
end;

end.
