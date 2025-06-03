{*********************************************}
{  TeeBI Software Library                     }
{  Map-Reduce algorithm using TDataItem data  }
{  Copyright (c) 2016-2025 by Steema Software }
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
  BI.Arrays, BI.DataItem, BI.Summary;

type
  TKeyIndex=TInteger;
  TIndices=TInt64Array;

  TMapReduce=class
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

  TMapReduce<T>=class(TMapReduce)
  private
    {$IFDEF FPC}
    type
      TArrayOfT=Array of T;
    {$ENDIF}

    class function DataFrom(const AData:{$IFDEF FPC}TArrayOfT{$ELSE}TArray<T>{$ENDIF}):TDataItem; static;
    class function KindOf:TDataKind; static;
  public
    type
      TMapProc={$IFNDEF FPC}reference to{$ENDIF} function(const Index:TKeyIndex):T;

    class function ForAll(const AData:TDataItem; const AMap:TMapProc):TDataItem; static;
  end;

  TMapReduce<T,V>=class(TMapReduce<T>)
  public
  type
    TReduceProc={$IFNDEF FPC}reference to{$ENDIF} function(const Key:T; const List:TIndices):V;

  private
    {$IFDEF FPC}
    type
      TArrayOfT=Array of T;
      TArrayOfV=Array of V;
      TArrayOfIndices=Array of TIndices;
    {$ENDIF}

    class procedure DoMap(const AFrom,ATo:TNativeInteger;
                          const AMap: TMapReduce<T>.TMapProc;
                          var AKey:{$IFDEF FPC}TArrayOfT{$ELSE}TArray<T>{$ENDIF};
                          var AItems:{$IFDEF FPC}TArrayOfIndices{$ELSE}TArray<TIndices>{$ENDIF}); static;

    class function DoReduce(const AKey:{$IFDEF FPC}TArrayOfT{$ELSE}TArray<T>{$ENDIF};
                             const AReduce:TReduceProc;
                             const AItems:{$IFDEF FPC}TArrayOfIndices{$ELSE}TArray<TIndices>{$ENDIF}):
                                {$IFDEF FPC}TArrayOfV{$ELSE}TArray<V>{$ENDIF}; static;

    class function TableFrom(const AKey:{$IFDEF FPC}TArrayOfT{$ELSE}TArray<T>{$ENDIF};
                             const AValue:{$IFDEF FPC}TArrayOfV{$ELSE}TArray<V>{$ENDIF}):TDataItem; static;
  public
    class function From(const AData:TDataItem;
                        const AMap:TMapReduce<T>.TMapProc;
                        const AReduce:TReduceProc):TDataItem; overload; static;
  end;

implementation

{$IFNDEF FPC}

{$IF CompilerVersion>27}
{$DEFINE THREADING}
{$ENDIF}

{$IF CompilerVersion>28}
{$DEFINE ARRAYINSERT}
{$ENDIF}

{$ENDIF}

uses
  TypInfo,
  {$IFDEF THREADING}
  Classes, Threading,
  {$ENDIF}
  BI.Expressions, BI.DataSource;

{ TMapReduce }

class function TMapReduce.Aggregate(const AKey,AValue:TDataItem;
                        const AAggregate:TAggregate):TDataItem;
var tmp : TSummary;
begin
  tmp:=TSummary.Create(nil);
  try
    if AValue=nil then
       tmp.Measures.Add(AKey,AAggregate)
    else
       tmp.Measures.Add(AValue,AAggregate);

    tmp.By.Add(AKey);

    result:=tmp.Calculate;
  finally
    tmp.Free;
  end;
end;

type
  TDataAccess=class(TDataItem);

class function TMapReduce.Aggregate(const AKey: TDataItem;
  const AExpression: String; const AAggregate: TAggregate): TDataItem;
var tmp : TDataItem;
begin
  tmp:=TDataItem.Create(True);
  try
    tmp.Items.Add(TDataClone.Clone(AKey));
    tmp.Items.Add(TExpressionColumn.From(AKey.Parent,AExpression,True,AExpression));

    TDataAccess(tmp).FCount:=AKey.Count;

    result:=Aggregate(tmp.Items[0],tmp.Items[1],AAggregate);
  finally
    tmp.Free;
  end;
end;

class function TMapReduce.Count(const AKey: TDataItem): TDataItem;
begin
  result:=Aggregate(AKey,nil,TAggregate.Count);
end;

class function TMapReduce.Mean(const AData: TDataItem;
                               const AIndices: TIndices): Double;
var t : TLoopInteger;
    tmp : Double;
    L : TNativeInteger;
begin
  L:=Length(AIndices);

  if L=0 then
     result:=0
  else
  begin
    // Calculate average
    tmp:=0;

    case AData.Kind of
        dkInt32: for t:=0 to L-1 do
                     tmp:=tmp+AData.Int32Data[AIndices[t]];

        dkInt64: for t:=0 to L-1 do
                     tmp:=tmp+AData.Int64Data[AIndices[t]];

       dkSingle: for t:=0 to L-1 do
                     tmp:=tmp+AData.SingleData[AIndices[t]];

       dkDouble: for t:=0 to L-1 do
                     tmp:=tmp+AData.DoubleData[AIndices[t]];

     dkExtended: for t:=0 to L-1 do
                     tmp:=tmp+AData.ExtendedData[AIndices[t]];

     dkDateTime: for t:=0 to L-1 do
                     tmp:=tmp+AData.DateTimeData[AIndices[t]];

      dkBoolean: ;
    end;

    result:=tmp/L;
  end;
end;

{ TMapReduce<T> }

class function TMapReduce<T>.KindOf:TDataKind;
var p : PTypeInfo;
begin
  {$IFDEF FPC}
  case PTypeInfo(TypeInfo(T)).Kind of
  {$ELSE}
  {$IF CompilerVersion>27}
  case GetTypeKind(T) of
  {$ELSE}
  case PTypeInfo(TypeInfo(T)).Kind of
  {$ENDIF}
  {$ENDIF}

    tkInteger: result:=TDataKind.dkInt32;
      tkInt64: result:=TDataKind.dkInt64;
      tkFloat: begin
                 p:=System.TypeInfo(T);

                 if p=System.TypeInfo(TDatetime) then
                    result:=TDataKind.dkDateTime
                 else
                 case GetTypeData(p)^.FloatType of
                   ftSingle: result:=TDataKind.dkSingle;
                 ftExtended: result:=TDataKind.dkExtended;
                 else
                             result:=TDataKind.dkDouble;
                 end;
               end;

     tkString,
     tkLString,
     tkWString,
     tkUString: result:=TDataKind.dkText;

  tkEnumeration: begin
                   p:=PTypeInfo(TypeInfo(T));

                   if GetTypeData(p)^.BaseType{$IFNDEF FPC}^{$ENDIF} = PTypeInfo(TypeInfo(Boolean)) then
                      result:=TDataKind.dkBoolean
                   else
                      result:=TDataKind.dkUnknown;
                 end;
  else
    result:=TDataKind.dkUnknown;
  end;
end;

class function TMapReduce<T>.DataFrom(const AData:{$IFDEF FPC}TArrayOfT{$ELSE}TArray<T>{$ENDIF}):TDataItem;
var tmp : TDataKind;
begin
  tmp:=KindOf;

  result:=TDataItem.Create(tmp);

  case tmp of
    TDataKind.dkInt32: result.Int32Data:=TInt32Array(AData);
    TDataKind.dkInt64: result.Int64Data:=TInt64Array(AData);
   TDataKind.dkSingle: result.SingleData:=TSingleArray(AData);
     TDataKind.dkText: result.TextData:=TTextArray(AData);
  TDataKind.dkBoolean: result.BooleanData:=TBooleanArray(AData);
  end;

  result.Resize(Length(AData));
end;

class function TMapReduce<T>.ForAll(const AData: TDataItem;
                                    const AMap: TMapProc): TDataItem;
var i : TLoopInteger;
    tmp : {$IFDEF FPC}TArrayOfT{$ELSE}TArray<T>{$ENDIF};
begin
  SetLength(tmp,AData.Count);

  for i:=0 to AData.Count-1 do
      tmp[i]:=AMap(i);

  result:=DataFrom(tmp);
end;

{ TMapReduce<T,V> }

class function TMapReduce<T,V>.TableFrom(const AKey:{$IFDEF FPC}TArrayOfT{$ELSE}TArray<T>{$ENDIF};
                                         const AValue:{$IFDEF FPC}TArrayOfV{$ELSE}TArray<V>{$ENDIF}):TDataItem;
var tmpKey,
    tmpValue : TDataItem;
begin
  result:=TDataItem.Create(True);
  result.Resize(Length(AKey));

  tmpKey:=TMapReduce<T>.DataFrom(AKey);
  tmpKey.Name:='Key';

  tmpValue:=TMapReduce<V>.DataFrom(AValue);
  tmpValue.Name:='Value';

  result.Items.Add(tmpKey);
  result.Items.Add(tmpValue);
end;

class function TMapReduce<T,V>.DoReduce(const AKey:{$IFDEF FPC}TArrayOfT{$ELSE}TArray<T>{$ENDIF};
                                        const AReduce:TReduceProc;
                                        const AItems:{$IFDEF FPC}TArrayOfIndices{$ELSE}TArray<TIndices>{$ENDIF}):
                                          {$IFDEF FPC}TArrayOfV{$ELSE}TArray<V>{$ENDIF};
var tmp : TNativeInteger;
    i   : TLoopInteger;
begin
  tmp:=Length(AKey);

  SetLength(result,tmp);

  for i:=0 to tmp-1 do
      result[i]:=AReduce(AKey[i],TIndices(AItems[i]));
end;

class procedure TMapReduce<T,V>.DoMap(const AFrom,ATo:TNativeInteger;
                                    const AMap: TMapReduce<T>.TMapProc;
                                    var AKey:{$IFDEF FPC}TArrayOfT{$ELSE}TArray<T>{$ENDIF};
                                    var AItems:{$IFDEF FPC}TArrayOfIndices{$ELSE}TArray<TIndices>{$ENDIF});
var i : TLoopInteger;

    tmpKind : TDataKind;

    tmp : T;

    tmpPos : TNativeInteger;
    tmpExists : Boolean;

    {$IFNDEF ARRAYINSERT}
    L : Integer;
    {$ENDIF}
begin
  tmpKind:=KindOf;

  if tmpKind=TDataKind.dkUnknown then
     Exit; //    raise??

  if tmpKind=TDataKind.dkBoolean then
  begin
    tmpExists:=True;

    TBooleanArray(AKey).Resize(2);
    TBooleanArray(AKey)[0]:=False;
    TBooleanArray(AKey)[1]:=True;
  end;

  for i:=AFrom to ATo do
  begin
    tmp:=AMap(i);

    case tmpKind of
        dkInt32: tmpPos:=TInt32Array(AKey).SortedFind(PInteger(@tmp)^,tmpExists);
        dkInt64: tmpPos:=TInt64Array(AKey).SortedFind(PInt64(@tmp)^,tmpExists);
       dkSingle: tmpPos:=TSingleArray(AKey).SortedFind(PSingle(@tmp)^,tmpExists);
       dkDouble: tmpPos:=TDoubleArray(AKey).SortedFind(PDouble(@tmp)^,tmpExists);
     dkExtended: tmpPos:=TExtendedArray(AKey).SortedFind(PExtended(@tmp)^,tmpExists);
         dkText: tmpPos:=TTextArray(AKey).SortedFind(PString(@tmp)^,tmpExists,False);
     dkDateTime: tmpPos:=TDateTimeArray(AKey).SortedFind(PDateTime(@tmp)^,tmpExists);
      dkBoolean: tmpPos:=Ord(PBoolean(@tmp)^);
    else
      tmpPos:=-1; // <-- this should never happen 
    end;

    if not tmpExists then
    begin
      case tmpKind of
          dkInt32: TInt32Array(AKey).Insert(tmpPos,PInteger(@tmp)^);
          dkInt64: TInt64Array(AKey).Insert(tmpPos,PInt64(@tmp)^);
         dkSingle: TSingleArray(AKey).Insert(tmpPos,PSingle(@tmp)^) ;
         dkDouble: TDoubleArray(AKey).Insert(tmpPos,PDouble(@tmp)^);
       dkExtended: TExtendedArray(AKey).Insert(tmpPos,PExtended(@tmp)^) ;
           dkText: TTextArray(AKey).Insert(tmpPos,PString(@tmp)^) ;
       dkDateTime: TDateTimeArray(AKey).Insert(tmpPos,PDateTime(@tmp)^) ;
      end;

      {$IFDEF ARRAYINSERT}
      Insert(nil,AItems,tmpPos);
      {$ELSE}
      L:=Length(AItems);
      SetLength(AItems,L+1);

      if tmpPos<L then
         System.Move(AItems[tmpPos],AItems[tmpPos+1],(L-tmpPos)*SizeOf(TIndices));

      AItems[tmpPos]:=nil;
      {$ENDIF}
    end;

    AItems[tmpPos].Append(i);
  end;
end;

class function TMapReduce<T,V>.From(const AData: TDataItem;
                                    const AMap: TMapReduce<T>.TMapProc;
                                    const AReduce: TReduceProc): TDataItem;

var tmpItems : {$IFDEF FPC}TArrayOfIndices{$ELSE}TArray<TIndices>{$ENDIF};
    tmpKey   : {$IFDEF FPC}TArrayOfT{$ELSE}TArray<T>{$ENDIF};

    {$IFDEF THREADING}
    tmp,
    Steps : Integer;

    tmpKeys : TArray<TArray<T>>;
    tmpItems2 : TArray<TArray<TIndices>>;
    {$ENDIF}
begin
  {$IFDEF THREADING}
  if Parallel then
  begin
    tmp:=TThread.ProcessorCount;

    if tmp>1 then
    begin
      Steps:=AData.Count div tmp;

      SetLength(tmpKeys,tmp);
      SetLength(tmpItems2,tmp);

      TParallel.&For(1,0,tmp-1,
        procedure(Index:Integer)
        begin
          DoMap(Index*Steps,((Index+1)*Steps)-1,AMap,tmpKeys[Index],tmpItems2[Index]);
        end);

      // TODO: Shuffle / Consolidate
    end
    else
      DoMap(0,AData.Count-1,AMap,tmpKey,tmpItems);
  end
  else
  {$ENDIF}
     DoMap(0,AData.Count-1,AMap,tmpKey,tmpItems);

  result:=TableFrom(tmpKey,
                    DoReduce(tmpKey,AReduce,tmpItems));
end;

end.
