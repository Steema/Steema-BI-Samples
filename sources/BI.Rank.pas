{*********************************************}
{  TeeBI Software Library                     }
{  Data Query Rankings                        }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Rank;

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
  BI.DataItem, BI.DataSource;

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

uses
  BI.Arrays;

{ TDataRank }

// Returns a new TDataItem filled with "rank" values using the Index
// parameter as sort order
class function TDataRank.DataFrom(const AIndex:TCursorIndex;
                                  const LastGroup:TDataItem):TDataItem;
var t : Integer;
    tmpRank : Integer;
begin
  result:=TDataItem.Create(TDataKind.dkInt32);
  result.Resize(AIndex.Count);

  if result.Count>0 then
  begin
    if LastGroup=nil then
       for t:=0 to result.Count-1 do
           result.Int32Data[AIndex[t]]:=t+1
    else
    begin
      tmpRank:=1;

      for t:=0 to result.Count-1 do
      begin
        result.Int32Data[AIndex[t]]:=tmpRank;

        if (t=result.Count-1) or LastGroup.SameData(AIndex[t],AIndex[t+1]) then
           Inc(tmpRank)
        else
           tmpRank:=1;
      end;
    end;
  end;
end;

class function TDataRank.From(const AData: TDataItem;
                              const AGroups: TDataArray;
                              const AValue: TDataItem;
                              const Ascending:Boolean): TDataItem;

  procedure DoError;
  begin
    raise EBIException.Create('Error: TDataRank Value is not assigned');
  end;

var tmp : TDataCursor;
    t : Integer;

    LastGroup : TDataItem;
begin
  if AValue=nil then
     DoError;

  tmp:=TDataCursor.Create(nil);
  try
    tmp.Data:=AData;

    if AGroups=nil then
       LastGroup:=nil
    else
    begin
      for t:=Low(AGroups) to High(AGroups) do
          tmp.SortBy.Add(AGroups[t]);

      LastGroup:=AGroups[High(AGroups)];
    end;

    tmp.SortBy.Add(AValue,Ascending);

    tmp.PrepareIndex;

    result:=DataFrom(tmp.Index,LastGroup);
  finally
    tmp.Free;
  end;
end;

end.
