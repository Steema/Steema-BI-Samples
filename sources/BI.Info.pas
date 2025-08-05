{*********************************************}
{  TeeBI Software Library                     }
{  Structure Information                      }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Info;

interface

uses
  BI.DataItem;

type
  // TDataInfo returns the structure of a TDataItem (properties of all sub-items)
  // in the Data property.

  // GetMinMax returns the minimum and maximum value of AData, using the Stats
  // property. If Stats are already calculated, they are reused.

  TDataInfo=class(TDataItem)
  private
    FData : TDataItem;

    procedure Fill;
    procedure SetData(const Value: TDataItem);
  public
    type
      TDataSizes=record
      public
        Tables,
        Columns,
        Rows,
        Cells : Int64;
      end;

    Constructor Create(const AData:TDataItem); overload;

    class procedure GetMinMax(const AData:TDataItem; out AMin,AMax:Extended); static;
    class function Sizes(const AData:TDataItem):TDataSizes; static;

    property Data:TDataItem read FData write SetData;
  end;

  // ItemsOf return a more complex view of the structure of AData, together with
  // all possible statistics of AData sub-items

  TDataItemsInfo=record
  private
    class procedure AddItems(const AItems:TDataItems); static;
    class procedure FillItemsOf(const AData,ADest:TDataItem); static;
  public
    class function ItemsOf(const AData:TDataItem):TDataItem; overload; static;
  end;

implementation

uses
  {System.}SysUtils, BI.Arrays;

{ TDataInfo }

Constructor TDataInfo.Create(const AData:TDataItem);
begin
  Create(True);

  Items.Add('Name',TDataKind.dkText);
  Items.Add('Kind',TDataKind.dkInt32);
  Items.Add('Unique',TDataKind.dkBoolean);
  Items.Add('Primary',TDataKind.dkBoolean);
  Items.Add('Count',TDataKind.dkInt64);
  Items.Add('Missing',TDataKind.dkInt64);
  Items.Add('Master',TDataKind.dkText);

  Data:=AData;
end;

type
  TDataAccess=class(TDataItem);

procedure TDataInfo.Fill;
var t : Integer;
    tmp : TDataItem;
begin
  Name:='Items_of_'+FData.Name;

  Resize(FData.Items.Count);

  for t:=0 to FData.Items.Count-1 do
  begin
    tmp:=FData.Items[t];

    Items[0].TextData[t]:=tmp.Name;
    Items[1].Int32Data[t]:=Ord(tmp.Kind);
    Items[2].BooleanData[t]:=tmp.Unique;
    Items[3].BooleanData[t]:=tmp.Primary;
    Items[4].Int64Data[t]:=tmp.Count;
    Items[5].Int64Data[t]:=tmp.Missing.MissingCount;

    if TDataAccess(tmp).HasMaster then
    try
      Items[6].TextData[t]:=tmp.Master.Name // OriginOf(tmp.Master) better?
    except
      // Protect against possible error when trying to access Master
      on Exception do
         Items[6].TextData[t]:=TDataAccess(tmp).IMaster.Origin; // <-- How to notify user the error?
    end
    else
       Items[6].Missing[t]:=True;
  end;
end;

class procedure TDataInfo.GetMinMax(const AData: TDataItem; out AMin,AMax: Extended);
var tmp : TDataStats;
begin
  tmp:=AData.Stats;

  if tmp=nil then
  begin
    AMin:=0;
    AMax:=0;
  end
  else
  begin
    case AData.Kind of
        dkInt32: AMax:=TInt32Stats(tmp).Max;
        dkInt64: AMax:=TInt64Stats(tmp).Max;
       dkSingle: AMax:=TSingleStats(tmp).Max;
       dkDouble: AMax:=TDoubleStats(tmp).Max;
     dkExtended: AMax:=TExtendedStats(tmp).Max;
     dkDateTime: AMax:=TDateTimeStats(tmp).Max;
    else
      AMax:=0;
    end;

    case AData.Kind of
        dkInt32: AMin:=TInt32Stats(tmp).Min;
        dkInt64: AMin:=TInt64Stats(tmp).Min;
       dkSingle: AMin:=TSingleStats(tmp).Min;
       dkDouble: AMin:=TDoubleStats(tmp).Min;
     dkExtended: AMin:=TExtendedStats(tmp).Min;
     dkDateTime: AMin:=TDateTimeStats(tmp).Min;
    else
      AMin:=0;
    end;
  end;
end;

procedure TDataInfo.SetData(const Value:TDataItem);
begin
  FData:=Value;

  if FData=nil then
     Resize(0)
  else
     Fill;
end;

// Returns the sizes of AData, recursively (the amount of rows, columns, cells and tables)
class function TDataInfo.Sizes(const AData: TDataItem): TDataSizes;

  procedure Process(const AData: TDataItem);
  var t : Integer;
  begin
    if AData<>nil then
    begin
      if AData.AsTable then
      begin
        Inc(result.Tables);

        Inc(result.Columns,AData.Items.Count);
        Inc(result.Rows,AData.Count);

        // Rows * Columns
        Inc(result.Cells,AData.Count*AData.Items.Count);
      end;

      for t:=0 to AData.Items.Count-1 do
          Process(AData[t]);
    end;
  end;

begin
  result.Tables:=0;
  result.Columns:=0;
  result.Rows:=0;
  result.Cells:=0;

  Process(AData);
end;

{ TDataItemsInfo }

class procedure TDataItemsInfo.AddItems(const AItems:TDataItems);
begin
  // Basic attributes
  AItems.Add('Table',dkText);
  AItems.Add('Name',dkText);
  AItems.Add('Kind',dkText);
  AItems.Add('Sorted',dkText);
  AItems.Add('IsPrimary',dkBoolean);
  AItems.Add('IsUnique',dkBoolean);
  AItems.Add('Master',dkText);

  // Size
  AItems.Add('Records',dkInt64);
  AItems.Add('Missing',dkInt64);
  AItems.Add('Valid',dkInt64);
  AItems.Add('Unique',dkInt64);
  AItems.Add('Unique Ratio',dkSingle);

  // Benchmark
  AItems.Add('CalcTime',dkInt32);

  // Stats
  AItems.Add('Minimum',dkExtended);
  AItems.Add('Maximum',dkExtended);

  AItems.Add('Mean',dkExtended);
  AItems.Add('Median',dkExtended);
  AItems.Add('Mode',dkExtended);

  AItems.Add('Range',dkExtended);
  AItems.Add('Std Deviation',dkExtended);
  AItems.Add('Sum',dkExtended);
  AItems.Add('Variance',dkExtended);
  AItems.Add('Peakedness',dkExtended); // (Kurtosis)
  AItems.Add('Skewness',dkExtended);
end;

class function TDataItemsInfo.ItemsOf(const AData:TDataItem):TDataItem;
begin
  result:=TDataItem.Create(True);
  result.Name:='Items of '+AData.Name;

  AddItems(result.Items);
  FillItemsOf(AData,result);
end;

class procedure TDataItemsInfo.FillItemsOf(const AData,ADest:TDataItem);

  procedure AddStats(const Dest:TDataArray; const AIndex:Integer; const AStats:TDataStats);
  begin
    Dest[15].ExtendedData[AIndex]:=AStats.Mean;
    Dest[18].ExtendedData[AIndex]:=AStats.Range;
    Dest[19].ExtendedData[AIndex]:=AStats.StdDeviation;
    Dest[20].ExtendedData[AIndex]:=AStats.Sum;
    Dest[21].ExtendedData[AIndex]:=AStats.Variance;
    Dest[22].ExtendedData[AIndex]:=AStats.Peakedness;
    Dest[23].ExtendedData[AIndex]:=AStats.Skewness;
  end;

  procedure GetBasicStats(const AData:TDataItem;
                          const Dest:TDataArray; const AIndex:Integer);
  var tmpMedian,
      tmpMode : TFloat;
      tmpMin,
      tmpMax : Extended;
      tmpStats : TDataStats;
  begin
    tmpStats:=AData.Stats;
    TDataInfo.GetMinMax(AData,tmpMin,tmpMax);

    if tmpStats=nil then
    begin
      tmpMedian:=0;
      tmpMode:=0;
    end
    else
    begin
      case AData.Kind of
        dkInt32: begin
             tmpMedian:=TInt32Stats(tmpStats).Median;
             tmpMode:=TInt32Stats(tmpStats).Mode;
           end;

        dkInt64: begin
             tmpMedian:=TInt64Stats(tmpStats).Median;
             tmpMode:=TInt64Stats(tmpStats).Mode;
           end;

        dkSingle: begin
             tmpMedian:=TSingleStats(tmpStats).Median;
             tmpMode:=TSingleStats(tmpStats).Mode;
        end;

        dkDouble : begin
             tmpMedian:=TDoubleStats(tmpStats).Median;
             tmpMode:=TDoubleStats(tmpStats).Mode;
        end;

        dkExtended : begin
             tmpMedian:=TExtendedStats(tmpStats).Median;
             tmpMode:=TExtendedStats(tmpStats).Mode;
        end;

        dkDateTime: begin
             tmpMedian:=TDateTimeStats(tmpStats).Median;
             tmpMode:=TDateTimeStats(tmpStats).Mode;
        end;
        // dkText:
        // dkBoolean:
      else
        begin
          tmpMin:=0;
          tmpMax:=0;
          tmpMedian:=0;
          tmpMode:=0;
        end;
      end;
    end;

    Dest[13].ExtendedData[AIndex]:=tmpMin;
    Dest[14].ExtendedData[AIndex]:=tmpMax;
    Dest[16].ExtendedData[AIndex]:=tmpMedian;
    Dest[17].ExtendedData[AIndex]:=tmpMode;
  end;

var
  tmpStart : Integer;
  tmpTable : String;

  procedure DoAddItem(const ACol:TDataItem; const Dest:TDataArray);
  var tmpMissing,
      tmpValid,
      tmpRows,
      tmpUnique : TInteger;

      tmpRatio : Single;

      tmpSorted : TDataOrder;
  begin
    tmpMissing:=ACol.Missing.MissingCount;

    ACol.Stats;

    if ACol.DataMap=nil then
    begin
      tmpUnique:=0;
      tmpSorted:=TDataOrder.None;
    end
    else
    begin
      tmpUnique:=ACol.DataMap.Count;
      tmpSorted:=ACol.DataMap.Sorted;
    end;

    tmpRows:=ACol.Count;

    if tmpRows>tmpMissing then
       tmpValid:=tmpRows-tmpMissing
    else
       tmpValid:=0;

    Dest[0].TextData[tmpStart]:=tmpTable;
    Dest[1].TextData[tmpStart]:=ACol.Name;
    Dest[2].TextData[tmpStart]:=ACol.Kind.ToString;
    Dest[3].TextData[tmpStart]:=tmpSorted.ToString;
    Dest[4].BooleanData[tmpStart]:=ACol.Primary;
    Dest[5].BooleanData[tmpStart]:=ACol.Unique;

    if ACol.Master=nil then
       Dest[6].TextData[tmpStart]:=''
    else
       Dest[6].TextData[tmpStart]:=ACol.Master.Name;

    Dest[7].Int64Data[tmpStart]:=tmpRows;
    Dest[8].Int64Data[tmpStart]:=tmpMissing;
    Dest[9].Int64Data[tmpStart]:=tmpValid;
    Dest[10].Int64Data[tmpStart]:=tmpUnique;

    if tmpValid=0 then
       tmpRatio:=0
    else
       tmpRatio:=tmpUnique/tmpValid;

    Dest[11].SingleData[tmpStart]:=tmpRatio;

    Dest[12].Int32Data[tmpStart]:=ACol.History.Times.Calculating;

    GetBasicStats(ACol,Dest,tmpStart);

    if ACol.Stats<>nil then
       AddStats(Dest,tmpStart,ACol.Stats);

    Inc(tmpStart);
  end;

var tmpCol : TDataItem;
    Dest : TDataArray;
begin
  tmpTable:=AData.Name;

  if tmpTable='' then
     if AData.Parent<>nil then
        tmpTable:=IntToStr(AData.Parent.Items.AsArray.IndexOf(AData));

  tmpStart:=ADest.Count;

  if AData.AsTable then
     ADest.Resize(tmpStart+AData.Items.Count)
  else
     ADest.Resize(tmpStart+1);

  Dest:=ADest.Items.AsArray;

  if AData.AsTable then
     for tmpCol in AData.Items.AsArray do
     begin
       DoAddItem(tmpCol,Dest);

       if tmpCol.AsTable then
          FillItemsOf(tmpCol,ADest);
     end
  else
     DoAddItem(AData,Dest);
end;

end.
