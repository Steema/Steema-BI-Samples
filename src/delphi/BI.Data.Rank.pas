{*********************************************}
{  TeeBI Software Library                     }
{  Data Query Rankings                        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.Rank;

(*
   TDataRank methods below calculate "Rankings" of TDataItem values,
   grouped by other sibling items.

   This is similar to SQL "rank over partition" query clause.

   Ranking values start at 1 and are sequential (1..2..3..4...etc)

   Pending: Rank "Mode" (Dense or Sparse)

   Example:

   https://github.com/Steema/BI/tree/master/demos/delphi/vcl/Query/Ranks
*)

interface

uses
  BI.Data, BI.DataSource;

type
  TDataRank=record
  private
    class function DataFrom(const AIndex:TCursorIndex;
                            const LastGroup:TDataItem):TDataItem; static;
  public
    // Note:
    // "AGroups" parameter can be nil. In this case the Rankings will be
    // calculated from 1 to AValue.Count (all values)

    class function From(const AData:TDataItem;
                        const AGroups:TDataArray;
                        const AValue:TDataItem;
                        const Ascending:Boolean=False):TDataItem; static;
  end;

implementation
