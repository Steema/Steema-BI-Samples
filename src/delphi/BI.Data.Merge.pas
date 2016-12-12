{*********************************************}
{  TeeBI Software Library                     }
{  Data Merge                                 }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.Merge;

// TDataMerge methods return a single TDataItem that is the result of
// aggregating many DataItems that have the same structure.

// "Same structure" means the number of sub-items and their Kind are
// identical. For sub-tables, structure is checked recursively.

interface

uses
  BI.Arrays, BI.Data, BI.Persist;

type
  TDataMerge=record
  private
    class procedure AppendTo(const ASource,ADest:TDataItem); static;

    class function Has(const A,B:TDataArray; const IndexB:TInteger):TInteger; static;
  public
    // Returns a new Data item with the same structure as AData.
    // If AData has sub-tables, they are also cloned recursively
    class function CloneStructure(const AData:TDataItem):TDataItem; static;

    // Returns a copy of all identical rows in A and B
    class procedure Common(const ADest:TDataItem; const A,B:TDataArray); static;

    // Returns a copy of all different rows in A and B
    class procedure Different(const ADest:TDataItem; const A,B:TDataArray); static;

    // Merges all data files in folder, of given extension
    class function FromFolder(const AFolder,AExtension:String):TDataItem; static;

    // Merges all data files of a TStore (with same structure) into a single one
    class function FromStore(const AStore,AName:String):TDataItem; static;

    // Merges all AData items into a single one.
    // When FreeData is True, all AData items are destroyed
    // (except the first one, that is the returned result)

    class function FromData(const AData:TDataArray; const FreeData:Boolean=True):TDataItem; overload; static;

    // Merges a copy of all AData items into ADest
    class procedure FromData(const ADest:TDataItem; const AData:TDataArray); overload; static;

    // Returns True when A and B have the same structure
    class function SameStructure(const A,B:TDataItem):Boolean; overload; static;

    // Returns True when all AItems have the same structure
    class function SameStructure(const AItems:TDataArray):Boolean; overload; static;

    // Removes data in ADest that is also in ASource
    class procedure Subtract(const ADest,ASource:TDataItem); static;
  end;

implementation
