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
  BI.Data, BI.Persist;

type
  TDataMerge=record
  public
    // Returns a new Data item with the same structure as AData.
    // If AData has sub-tables, they are also cloned recursively
    class function CloneStructure(const AData:TDataItem):TDataItem; static;

    // Merges all data files in folder, of given extension
    class function FromFolder(const AFolder,AExtension:String):TDataItem; static;

    // Merges all data files of a TStore (with same structure) into a single one
    class function FromStore(const AStore,AName:String):TDataItem; static;

    // Merges all ADatas into a single one.
    // When FreeDatas is True, all ADatas are destroyed
    // (except the first one, that is the returned result)

    class function FromDatas(const ADatas:TDataArray; const FreeDatas:Boolean=True):TDataItem; static;

    // Returns True when A and B have the same structure
    class function SameStructure(const A,B:TDataItem):Boolean; overload; static;

    // Returns True when all AItems have the same structure
    class function SameStructure(const AItems:TDataArray):Boolean; overload; static;
  end;

implementation
