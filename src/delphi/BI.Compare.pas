{*********************************************}
{  TeeBI Software Library                     }
{  Data comparison                            }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Compare;

interface

uses
  BI.DataItem, BI.Arrays;

type
  TBaseDifferences=class(TDataItem)
  public
    Added,
    Removed,
    Modified : TDataItem;

    Constructor Create;
  end;

  TItemDifference=class(TBaseDifferences)
  public
    Item : TDataItem;

    SameKind  : Boolean;
    SameTable : Boolean;
  end;

  TRenamedItem=record
  public
    Before,
    After : TDataItem;
  end;

  TItemDifferences=class(TBaseDifferences)
  public
    Renamed  : Array of TRenamedItem;
  end;

  TDifference=class(TDataItem)
  public
    Items : TItemDifferences;
    Rows  : TBaseDifferences;

    Constructor Create;
  end;

  TRenamedData=record
  public
    Before,
    After : TDataItem;
  end;

  TDataDifferences=class(TDataItem)
  public
    Added    : TDataArray;
    Removed  : TDataArray;
    Renamed  : Array of TRenamedData;
    Modified : Array of TDifference;
  end;

  // Compare two TDataItem structures (including nested children items),
  // and return the differences both in structure and data in the Diff parameter.
  TDataCompare=record
  private
    class function Same(const Before,After:TDataItem; var Diff:TItemDifference):Boolean; overload; static;
    class function Same(const Before,After:TDataItems; var Diff:TItemDifferences):Boolean; overload; static;

    class function Same(const Before,After:TDataItem; var Diff:TBaseDifferences):Boolean; overload; static;
  public
    class function Compare(const A:TDataItem; const ARow:TInteger;
                           const B:TDataItem; const BRow:TInteger): Boolean; overload; static;

    class function Compare(const Before, After: TDataItem; const ARow:TInteger): Boolean; overload; static;

    class function Compare(const A:TDataArray; const ARow:TInteger;
                           const B:TDataArray; const BRow:TInteger): Boolean; overload; static;

    class function Same(const Before,After:TDataItem):Boolean; overload; static;
    class function Same(const Before,After:TDataItem; var Diff:TDifference):Boolean; overload; static;
    class procedure Compare(const Before,After:TDataItem; const Dest:TDataItem); overload; static;
  end;

implementation
