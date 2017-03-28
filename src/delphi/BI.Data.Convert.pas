{*********************************************}
{  TeeBI Software Library                     }
{  TDataItem Kind Conversion                  }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.Convert;

interface

uses
  BI.Data;

{
  Use TDataKindConvert.Convert method to change the "Kind" property of a
  TDataItem object.

  All data is preserved and checked for compatibility with the new Kind.
  All "Missing" data values are also preserved (not converted).

  Note:
    TDataItem AsTable (multiple fields) cannot be converted.
    All Items must be converted individually.
}

type
  TDataKindConvert=record
  public
    class function CanConvert(const AData:TDataItem; const AKind:TDataKind):Boolean; static;
    class function Convert(const AData:TDataItem; const AKind:TDataKind):Boolean; static;
  end;

implementation
