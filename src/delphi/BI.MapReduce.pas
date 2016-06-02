{*********************************************}
{  TeeBI Software Library                     }
{  Map-Reduce algorithm using TDataItem data  }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit BI.MapReduce;

interface

// Map-Reduce algorithm using TDataItem data sources.

(*
  Examples of use:

    // Simple count using a data item "Year" as keys
    result:=TMapReduce.Count(Year);

    // Simple aggregation using a data item "Year" as keys, and "Rating" as values
    result:=TMapReduce.Aggregate(Year,Rating,TAggregate.Average);

    // Simple aggregation using a data item "Year" as keys, and a expression as values
    result:=TMapReduce.Aggregate(Year,'Votes/Length',TAggregate.Average);

    // Advanced usage, with a Map and Reduce anonymous functions
    result:=TMapReduce<Integer,Single>.From(Movies,TDataKind.dkInt32,

         // Map
         function(const Index:TInteger):Integer
         begin
           result:=Year.Int32Data[Index];
         end,

         // Reduce
         function(const Key:Integer; const List:TIndices):Single
         var t : TLoopInteger;
             tmp : Single;
             L : TNativeInteger;
         begin
           L:=Length(List);

           if L=0 then
              result:=0
           else
           begin
             // Calculate average
             tmp:=0;

             for t:=0 to L-1 do
                 tmp:=tmp+Rating.SingleData[List[t]];

             result:=tmp/L;
           end;
         end);
*)

uses
  BI.Arrays, BI.Data, BI.Summary;

type
  TKeyIndex=TInteger;
  TIndices=TInt64Array;

  TMapReduce=class
  private
    class function DataFrom<T>(const AData:TArray<T>):TDataItem; static;
    class function KindOf<T>:TDataKind; static;
  public
    class var
      Parallel : Boolean;

    class function Count(const AKey:TDataItem):TDataItem; static;

    class function Aggregate(const AKey,AValue:TDataItem;
                             const AAggregate:TAggregate):TDataItem; overload; static;

    class function Aggregate(const AKey:TDataItem;
                             const AExpression:String;
                             const AAggregate:TAggregate):TDataItem; overload; static;

    class function Mean(const AData:TDataItem; const AIndices:TIndices):Double; static;
  end;

  TMapReduce<T,V>=class(TMapReduce)
  public
  type
    TMapProc=reference to function(const Index:TKeyIndex):T;
    TReduceProc=reference to function(const Key:T; const List:TIndices):V;

  private
    class function DoMap(const AFrom,ATo:TNativeInteger;
                         const AMap: TMapProc;
                         var AKey:TArray<T>;
                         var AItems:TArray<TIndices>): TDataItem; static;

    class function DoReduce(const AKey:TArray<T>;
                             const AReduce:TReduceProc;
                             const AItems:TArray<TIndices>):TArray<V>; static;

    class function TableFrom(const AKey:TArray<T>; const AValue:TArray<V>):TDataItem; static;
  public
    class function From(const AData:TDataItem;
                        const AMap:TMapProc;
                        const AReduce:TReduceProc):TDataItem; overload; static;
  end;

implementation
