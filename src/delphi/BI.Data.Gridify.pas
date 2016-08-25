{*********************************************}
{  TeeBI Software Library                     }
{  Flat table to pivot-grid algorithm         }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.Gridify;

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
  2017  Bob      65
  2017  Mike     80
  2018  Alice     3
  2018  Bob       9
  2018  Mike      4

  Output is:

  -----------------------
         Happiness
  Year   Person
         Alice Bob Mike

  2016    21    14   39
  2017          65   80
  2018     3     9    4
  -----------------------

  Source data does not need to be sorted.

  More than one field can be used for rows, columns and values
  (see overload methods).

*)

uses
  BI.Data, BI.Arrays;

type
  TGridify=record
  public
    class function From(const AValue:TDataItem;
                        const ARows,ACols:TDataArray):TDataItem; overload; static;

    class function From(const AData:TDataItem;
                        const AValue,ARow,ACol:String):TDataItem; overload; static;

    class function FromItems(const AData:TDataItem;
                        const AValue:String;
                        const ARows,ACols:TStringArray):TDataItem; static;
  end;

implementation
