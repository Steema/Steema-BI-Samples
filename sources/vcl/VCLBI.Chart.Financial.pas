{*********************************************}
{  TeeBI Software Library                     }
{  TChart Financial Charts                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Chart.Financial;
{.$DEFINE FMX}

interface

uses
  BI.Arrays, BI.DataItem,

  {$IFDEF FMX}
  FMXTee.Constants, FMXTee.Procs,
  FMXTee.Engine, FMXTee.Chart,
  {$ELSE}
  VCL.Graphics, VCL.Controls,

  VCLTee.TeeConst, VCLTee.TeeProcs,
  VCLTee.TeeGDIPlus,
  VCLTee.TeEngine, VCLTee.Chart,
  {$ENDIF}

  {$IFDEF FMX}
  FMXBI.Chart.Plugin,
  {$ELSE}
  VCLBI.Chart.Plugin,
  {$ENDIF}

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}
  {$ENDIF}

  {$IFDEF TEEPRO}
  {$IFDEF FMX}
  FMXTee.Series.OHLC, FMXTee.Series.Candle
  {$ELSE}
  VCLTee.OHLChart, VCLTee.CandleCh
  {$ENDIF}
  {$ENDIF}

  ;

type
  TOHLCSeriesClass=class of TOHLCSeries;

  TFinancialChart=record
  private
    class function CreateFinancial(const AChart:TBITChart;
                             const ADate,AOpen,AClose,AHigh,ALow,AVolume:TDataItem;
                             const AReverseDate:Boolean):TOHLCSeries; static;

    class function GetDateTime(const AData:TDataItem; const Index:TInteger;
                               const Reverse:Boolean):TDateTime; static;
  public
    class var
      SeriesClass : TOHLCSeriesClass;

    class function CanReuse(const AChart:TBITChart):Boolean; static;

    class procedure Fill(const AChart:TBITChart;
                         const AValues:TDataArray;
                         const AX:TDataItem); static;

    class function Guess(const AValues:TDataArray):Boolean; static;
  end;

implementation

uses
  System.SysUtils;

procedure SwapFloat(var A,B:TChartValue);
var tmp : TChartValue;
begin
  tmp:=A;
  A:=B;
  B:=tmp;
end;

// yyyy-mm-dd to dd-mm-yyyy or mm-dd-yyyy
function ReverseDate(const S:String):String;
var fs : TFormatSettings;
    dt : TDateTime;
Begin
  {$IFNDEF FPC}
  fs:=TFormatSettings.Create;
  {$ENDIF}

  fs.DateSeparator:=S[5];
  fs.ShortDateFormat:='yyyy'+fs.DateSeparator+'mm'+fs.DateSeparator+'dd';

  if TryStrToDateTime(S,dt,fs) then
     result:=DateTimeToStr(dt)
  else
     result:=DateTimeToStr(0);
end;

class function TFinancialChart.CanReuse(const AChart:TBITChart):Boolean;
begin
  // Pending: Try to investigate AChart, to avoid destroying all series in it
  result:=False;
end;

class function TFinancialChart.GetDateTime(const AData:TDataItem; const Index:TInteger; const Reverse:Boolean):TDateTime;
var tmp : String;
begin
  if AData.Kind=TDataKind.dkDateTime then
     result:=AData.DateTimeData[Index]
  else
  if AData.Kind=TDataKind.dkText then
  begin
    if Reverse then
       tmp:=ReverseDate(AData.TextData[Index])
    else
       tmp:=AData.TextData[Index];

    result:=StrToDateTime(tmp);
  end
  else
     result:=0; // raise ??
end;

function ContainsText(const Big,Small:String):Boolean;
begin
  result:=Pos(UpperCase(Small),UpperCase(Big))>0;
end;

type
  TDataItemAccess=class(TDataItem);

function ExistsData(const AName:String; const AData:TDataArray):TDataItem;
var t : Integer;
    tmp : TDataItem;
begin
  for t:=Low(AData) to High(AData) do
  begin
    tmp:=AData[t];

    if tmp.Kind.IsNumeric and ContainsText(tmp.Name,AName) and
       (not tmp.Primary) and (not TDataItemAccess(tmp).HasMaster) then
         Exit(tmp);
  end;

  result:=nil;
end;

type
  TOpenClose=record
  public
    Open,
    Close : String;
  end;

const
  OpenClose : Array[0..2] of TOpenClose=
    (
      ( Open:'open'; Close:'close'),
      ( Open:'apertura'; Close:'cierre'),
      ( Open:'ouvrir'; Close:'clôture')
    );

class function TFinancialChart.Guess(const AValues: TDataArray): Boolean;
var t : Integer;
begin
  // or ExistsAtLeast(4,Numeric);

  for t:=Low(OpenClose) to High(OpenClose) do
      if (ExistsData(OpenClose[t].Open,AValues)<>nil) and
         (ExistsData(OpenClose[t].Close,AValues)<>nil) then
            Exit(True);

  result:=False;
end;

class function TFinancialChart.CreateFinancial(const AChart:TBITChart;
                                   const ADate,AOpen,AClose,AHigh,ALow,
                                   AVolume:TDataItem;
                                   const AReverseDate:Boolean):TOHLCSeries;
var
  tmp : TOHLCSeries;
  tmpVolume : TVolumeSeries;

  procedure Add(const Index:Integer);
  var tmpOpen,
      tmpClose,
      tmpHigh,
      tmpLow : TChartValue;
      tmpDate : String;
  begin
    tmpOpen:=TBITChart.GetValue(AOpen,Index);
    tmpClose:=TBITChart.GetValue(AClose,Index);

    if AHigh=nil then
       tmpHigh:=tmpOpen
    else
       tmpHigh:=TBITChart.GetValue(AHigh,Index);

    if ALow=nil then
       tmpLow:=tmpClose
    else
       tmpLow:=TBITChart.GetValue(ALow,Index);

    if tmpLow>tmpHigh then
       SwapFloat(tmpLow,tmpHigh);

    if ADate=nil then
       tmpDate:=''
    else
       tmpDate:=DateTimeToStr(GetDateTime(ADate,Index,AReverseDate));

    tmp.AddOHLC(tmp.Count,tmpOpen,tmpHigh,tmpLow,tmpClose,tmpDate);

    if AVolume<>nil then
       tmpVolume.Add(TBITChart.GetValue(AVolume,Index));
  end;

  function DateOf(const AIndex:Integer):TDateTime;
  begin
    result:=GetDateTime(ADate,AIndex,AReverseDate);
  end;

  function IsAscendingDate:Boolean;
  begin
    if ADate.Count<2 then
       result:=True
    else
       result:=DateOf(0)<DateOf(ADate.Count-1);
  end;

  function CreateVolumeSeries:TVolumeSeries;
  begin
    result:=AChart.NewSeries(TVolumeSeries) as TVolumeSeries;
    result.VertAxis:=TVertAxis.aRightAxis;
    result.ParentChart.Axes.Right.Grid.Hide;

    result.ParentChart.Axes.Right.Title.Caption:=AVolume.Name;
  end;

  procedure FillCandle;
  var t : TLoopInteger;
  begin
    if (ADate=nil) or IsAscendingDate then
        for t:=0 to AOpen.Count-1 do
            Add(t)
    else
        for t:=AOpen.Count-1 downto 0 do
            Add(t);
  end;

begin
  result:=AChart.NewSeries(SeriesClass) as SeriesClass;
  tmp:=result;

  if AVolume=nil then
     tmpVolume:=nil
  else
     tmpVolume:=CreateVolumeSeries;

  if ADate=nil then
     tmp.DateValues.DateTime:=False;

  tmp.BeginUpdate;
  try
    FillCandle;
  finally
    tmp.EndUpdate;
  end;
end;

class procedure TFinancialChart.Fill(const AChart:TBITChart;
                                     const AValues:TDataArray;
                                     const AX:TDataItem);

  function IsDateSeparator(const C:Char):Boolean;
  begin
    result:=(C='-') or (C='/');
  end;

  function IsTextDateTime(const AData:TDataItem; out IsReversed:Boolean):Boolean;
  var tmp : TDateTime;
      tmpS : String;
  begin
    result:=False;
    IsReversed:=False;

    if AData.Kind=TDataKind.dkText then
       if AData.Count>0 then
       begin
         tmpS:=AData.TextData[0];

         result:=TryStrToDateTime(tmpS,tmp);

         // Try yyyy-mm-dd.  (Pending: try yyyy-m-d)

         if (not result) and (Length(tmpS)>8) then
         begin
           if IsDateSeparator(tmpS[5]) and
              IsDateSeparator(tmpS[8]) then
           begin
             result:=TryStrToDateTime(ReverseDate(tmpS),tmp);
             IsReversed:=result;
           end;
         end;
       end;
  end;

  function IsDateTime(const AData:TDataItem; out IsReversed:Boolean):Boolean;
  begin
    result:=(AData.Kind=TDataKind.dkDateTime) or IsTextDateTime(AData,IsReversed);
  end;

  function ExistsAnyDateTime(const AData:TDataArray; out IsReversed:Boolean):TDataItem;
  var t : Integer;
      tmp : TDataItem;
  begin
    for t:=Low(AData) to High(AData) do
    begin
      tmp:=AData[t];

      if IsDateTime(tmp,IsReversed) then
         Exit(tmp);
    end;

    result:=nil;
  end;

var tmpS : TChartSeries;

    D,O,C,H,L,V : TDataItem;
    tmpReverse : Boolean;
begin
  O:=ExistsData('open',AValues);
  H:=ExistsData('high',AValues);
  L:=ExistsData('low',AValues);
  C:=ExistsData('close',AValues);

  V:=ExistsData('volume',AValues);

  if (AX<>nil) and IsDateTime(AX,tmpReverse) then
     D:=AX
  else
     D:=ExistsAnyDateTime(AValues,tmpReverse);

  tmpS:=CreateFinancial(AChart,D,O,C,H,L,V,tmpReverse);

  // Limit candle points to last N
  if tmpS.Count>240 then
  begin
    // Alternative: Add ChartScroller tool
    AChart.Axes.Bottom.SetMinMax(tmpS.Count-241,tmpS.Count-1);

    // Pending: Auto axis min max based on current displayed range
  end;

  // Disabled due to a Tee.js bug (bad cursor repaint)
  // AddCursorTool;
end;

initialization
  TBITChart.FourItemSeries:=TOHLCSeries;
  TFinancialChart.SeriesClass:=TCandleSeries;
finalization
  TBITChart.FourItemSeries:=nil;
end.
