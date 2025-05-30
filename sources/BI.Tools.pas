{*********************************************}
{  TeeBI Software Library                     }
{  General utilities to work with TDataItem   }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Tools;

interface

{
 TDataSplit:
   Divide a TDataItem into 2 parts using different methods.

 TDataNormalize:
   Converts existing data into float values from 0 to 1.

}


uses
  BI.Arrays, BI.DataItem;

type
  {
    TDataSplit returns two Indices arrays (A and B) with the row numbers for
    both parts of the split
  }

  TSplitBy=(Percent,Count);

  TSplitMode=(Random,Start);

  TSplitOptions=record
  public
    By : TSplitBy;
    Count : Integer;
    Mode : TSplitMode;
    Percent : Single;

    procedure Initialize;
  end;

  // Fills A and B arrays either using random values or sequential
  TDataSplit=record
    A,
    B : TNativeIntArray;

    procedure From(const ATotal:TInteger; const AOptions:TSplitOptions);
    procedure Random(const CountA, CountB:TInteger);
    procedure Sequence(const CountA, CountB:TInteger);
  end;

  // Converts Data into float values from 0 to 1
  TDataNormalize=record
  private
    class procedure NormalizeInt32(const AData:TDataItem); static;
    class procedure NormalizeInt64(const AData:TDataItem); static;
    class procedure NormalizeSingle(const AData:TDataItem); static;
    class procedure NormalizeDouble(const AData:TDataItem); static;
    class procedure NormalizeExtended(const AData:TDataItem); static;
    class procedure NormalizeDateTime(const AData:TDataItem); static;
    class procedure NormalizeBoolean(const AData:TDataItem); static;
    class procedure NormalizeText(const AData:TDataItem); static;

    class procedure Recalculate(const AData: TDataItem;
                                const AKind: TDataKind); static;
  public
    class procedure Normalize(const AData:TDataItem); overload; static;
    class procedure Normalize(const AData:TDataArray); overload; static;
  end;

implementation

{ TSplitOptions }

procedure TSplitOptions.Initialize;
begin
  Mode:=TSplitMode.Start;
  Percent:=50;
  By:=TSplitBy.Percent;
  Count:=0;
end;

{ TDataSplit }

// Creates indices arrays A and B, fills them with random integers
procedure TDataSplit.From(const ATotal:TInteger;
                          const AOptions: TSplitOptions);
var A,
    B : TInteger;
begin
  if AOptions.By=TSplitBy.Percent then
     A:=Round(0.01*AOptions.Percent*ATotal)
  else
     A:=AOptions.Count;

  if A>ATotal then
     B:=0
  else
     B:=ATotal-A;

  if AOptions.Mode=TSplitMode.Random then
     Random(A,B)
  else
     Sequence(A,B);
end;

procedure TDataSplit.Random(const CountA, CountB: TInteger);
var tmp : TBooleanArray;
    t : TLoopInteger;
    Total,
    Index : TInteger;
begin
  Total:=CountA+CountB;

  tmp.Resize(Total);
  tmp.Initialize; // <-- optional?

  A.Resize(CountA);
  B.Resize(CountB);

  for t:=0 to CountA-1 do
  begin
    repeat
      Index:=System.Random(Total);
    until not tmp[Index];

    tmp[Index]:=True;
    A[t]:=Index;
  end;

  Index:=0;

  for t:=0 to CountB-1 do
  begin
    while tmp[Index] do
      Inc(Index);

    B[t]:=Index;
    Inc(Index);
  end;
end;

// Creates indices arrays A and B, fills them with sequential
// integers
procedure TDataSplit.Sequence(const CountA, CountB: TInteger);
var t : TLoopInteger;
begin
  A.Resize(CountA);

  for t:=0 to CountA-1 do
      A[t]:=t;

  B.Resize(CountB);

  for t:=0 to CountB-1 do
      B[t]:=CountA+t;
end;

{ TDataNormalize }

type
  TDataAccess=class(TDataItem);

class procedure TDataNormalize.Recalculate(const AData: TDataItem;
                                           const AKind: TDataKind);
begin
  // Force Kind to a new type:
  TDataAccess(AData).FKind:=AKind;
  AData.ReCalculate;
end;

class procedure TDataNormalize.NormalizeSingle(const AData:TDataItem);
var Min,
    Max,
    Range : Single;
    t : TLoopInteger;
begin
  Min:=TSingleStats(AData.Stats).Min;
  Max:=TSingleStats(AData.Stats).Max;

  Range:=Max-Min;

  if Range=0 then
     AData.SingleData.Initialize
  else
     for t:=0 to AData.Count-1 do
         AData.SingleData[t]:=(AData.SingleData[t]-Min)/Range;
end;

class procedure TDataNormalize.NormalizeDouble(const AData:TDataItem);
var Min,
    Max,
    Range : Double;
    t : TLoopInteger;
begin
  Min:=TDoubleStats(AData.Stats).Min;
  Max:=TDoubleStats(AData.Stats).Max;

  Range:=Max-Min;

  if Range=0 then
     AData.DoubleData.Initialize
  else
     for t:=0 to AData.Count-1 do
         AData.DoubleData[t]:=(AData.DoubleData[t]-Min)/Range;
end;

class procedure TDataNormalize.NormalizeExtended(const AData:TDataItem);
var Min,
    Max,
    Range : Extended;
    t : TLoopInteger;
begin
  Min:=TExtendedStats(AData.Stats).Min;
  Max:=TExtendedStats(AData.Stats).Max;

  Range:=Max-Min;

  if Range=0 then
     AData.ExtendedData.Initialize
  else
     for t:=0 to AData.Count-1 do
         AData.ExtendedData[t]:=(AData.ExtendedData[t]-Min)/Range;
end;

class procedure TDataNormalize.NormalizeDateTime(const AData:TDataItem);
var Min,
    Max,
    Range : TDateTime;
    t : TLoopInteger;
begin
  Min:=TDateTimeStats(AData.Stats).Min;
  Max:=TDateTimeStats(AData.Stats).Max;

  Range:=Max-Min;

  if Range=0 then
     AData.DateTimeData.Initialize
  else
     for t:=0 to AData.Count-1 do
         AData.DateTimeData[t]:=(AData.DateTimeData[t]-Min)/Range;
end;

class procedure TDataNormalize.NormalizeInt32(const AData:TDataItem);
var Min,
    Max,
    Range : Integer;
    t : TLoopInteger;
begin
  Min:=TInt32Stats(AData.Stats).Min;
  Max:=TInt32Stats(AData.Stats).Max;

  Range:=Max-Min;

  if Range=0 then
     AData.Int32Data.Initialize
  else
  begin
    AData.SingleData.Resize(AData.Count);

    for t:=0 to AData.Count-1 do
        AData.SingleData[t]:=(AData.Int32Data[t]-Min)/Range;

    AData.Int32Data:=nil;
    Recalculate(AData,TDataKind.dkSingle);
  end;
end;

class procedure TDataNormalize.NormalizeInt64(const AData:TDataItem);
var Min,
    Max,
    Range : Integer;
    t : TLoopInteger;
begin
  Min:=TInt64Stats(AData.Stats).Min;
  Max:=TInt64Stats(AData.Stats).Max;

  Range:=Max-Min;

  if Range=0 then
     AData.Int64Data.Initialize
  else
  begin
    AData.SingleData.Resize(AData.Count);

    for t:=0 to AData.Count-1 do
        AData.SingleData[t]:=(AData.Int64Data[t]-Min)/Range;

    AData.Int64Data:=nil;
    Recalculate(AData,TDataKind.dkSingle);
  end;
end;

class procedure TDataNormalize.NormalizeBoolean(const AData:TDataItem);
var t : TLoopInteger;
begin
  AData.Int32Data.Resize(AData.Count);

  for t:=0 to AData.Count-1 do
      AData.Int32Data[t]:=Ord(AData.BooleanData[t]);

  AData.BooleanData:=nil;
  Recalculate(AData,TDataKind.dkInt32);
end;

class procedure TDataNormalize.NormalizeText(const AData:TDataItem);
var t : TLoopInteger;
    tmp : TNativeInteger;
begin
  AData.Int64Data.Resize(AData.Count);

  AData.Stats;

  for t:=0 to AData.Count-1 do
      if TTextMap(AData.DataMap).Find(AData.TextData[t],tmp) then
         AData.Int64Data[t]:=tmp;

  AData.TextData:=nil;
  Recalculate(AData,TDataKind.dkInt64);

  NormalizeInt64(AData);
end;

// Replaces all numeric values in AData with their equivalents in the
// range from 0 to 1  (being 0 the AData minimum and 1 the maximum)
class procedure TDataNormalize.Normalize(const AData: TDataItem);
begin
  case AData.Kind of
     dkInt32: NormalizeInt32(AData);
     dkInt64: NormalizeInt64(AData);
    dkSingle: NormalizeSingle(AData);
    dkDouble: NormalizeDouble(AData);
  dkExtended: NormalizeExtended(AData);
  dkDateTime: NormalizeDateTime(AData);
   dkBoolean: NormalizeBoolean(AData);
      dkText: NormalizeText(AData);
  else
    Normalize(AData.Items.AsArray); // Recursive
  end;
end;

// Normalizes all data items in AData array
class procedure TDataNormalize.Normalize(const AData: TDataArray);
var Item : TDataItem;
begin
  for Item in AData do
      Normalize(Item);
end;

end.
