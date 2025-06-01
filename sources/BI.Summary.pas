{*********************************************}
{  TeeBI Software Library                     }
{  Summary and Query classes                  }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Summary;

interface

{
  This is the main unit to perform summaries, groupby queries, histograms etc.
}

uses
  {System.}Classes,
  {$IFNDEF FPC}
  System.Generics.Collections,
  {$ENDIF}
  BI.Arrays, BI.DataItem, BI.Expression, BI.Expressions;

type
  TBaseSummaryPersistent=class(TPersistent)
  protected
    IChanged : TNotifyEvent;

    procedure Changed;
    procedure DoChanged(Sender:TObject);
  end;

  // Count is the number of data values that belong to a Histogram interval <Maximum
  TBin=record
  public
    Maximum : TFloat;
    Count : TInteger;
  end;

  // Array of Histogram bins
  TBinArray=Array of TBin;

  // Accumulates data by numeric intervals
  THistogram=class(TBaseSummaryPersistent)
  private
    // Pending:
    // Derive THistogram from TSummaryItem, to allow expressions instead of just data Items.

    FActive : Boolean;
    FBinSize : TFloat;
    FNumBins : Integer;

    FAutoMinimum : Boolean;
    FMinimum : TFloat;

    FAutoMaximum : Boolean;
    FMaximum : TFloat;

    FFloatFormat : String;

    FSource : TDataItem;

    Bins : TBinArray;
    HighBins : Integer;
    procedure SetActive(const Value: Boolean);
    procedure SetAutoMaximum(const Value: Boolean);
    procedure SetAutoMinimum(const Value: Boolean);
    procedure SetBinSize(const Value: TFloat);
    procedure SetMaximum(const Value: TFloat);
    procedure SetMinimum(const Value: TFloat);
    procedure SetNumBins(const Value: Integer);

  const
    OrdA=Ord('A');
    OrdZ=Ord('Z');
    AllChars=OrdZ-OrdA+1;

    function BinOf(const AData:TDataItem; const AIndex:TInteger):Integer;
    function BinToString(const Index:Integer):String;

    procedure Fill(var Bins:TBinArray);
    function Prepare(const Source:TDataItem):Integer;
  public
    const
      DefaultName='Histogram';
      UpToText='Up to';
      CountOfText='Count of ';

    Constructor Create;

    procedure Assign(Source:TPersistent); override;

    function Calculate(const Source:TDataItem):TDataItem;
    function BinCount:Integer; inline;
  published
    property Active:Boolean read FActive write SetActive default False;
    property AutoMinimum : Boolean read FAutoMinimum write SetAutoMinimum default True;
    property AutoMaximum : Boolean read FAutoMaximum write SetAutoMaximum default True;
    property BinSize : TFloat read FBinSize write SetBinSize; // default 0
    property FloatFormat : String read FFloatFormat write FFloatFormat;
    property Minimum : TFloat read FMinimum write SetMinimum; // default 0
    property Maximum : TFloat read FMaximum write SetMaximum; // default 0
    property NumBins : Integer read FNumBins write SetNumBins default 0;
  end;

  // Basic summary operations
  TAggregate=(Count,Sum,Average,Minimum,Maximum,First,Last);

  TAggregateHelper=record helper for TAggregate
  private
    const
      Names:Array[TAggregate] of String=('Count','Sum','Average','Minimum','Maximum','First','Last');
  public
    class function AllToText:String; static;
    class function FromString(const S:String; out Aggregate:TAggregate):Boolean; static;
    function SupportsAsZero:Boolean;
    function ToString:String; inline;
  end;

  // Options to include or not missing (null) values in summarizations
  TMeasureMissing=class(TBaseSummaryPersistent)
  private
    FAsZero : Boolean;
    procedure SetAsZero(const Value: Boolean);
  public
    procedure Assign(Source:TPersistent); override;
  published
    property AsZero : Boolean read FAsZero write SetAsZero default False;
  end;

  TSummaryItemType=(GroupBy, Measure, GroupOrMeasure);

  // Base class for summary Measures and GroupBy dimensions
  TSummaryItem=class(TBaseSummaryPersistent)
  private
    FActive : Boolean;
    FData : TDataItem;
    FName : String;

    FDestData  : TDataItem;

    KeepSource : Boolean;

    procedure LoadData(const Item:TExpression);
    procedure SetActive(const Value: Boolean);
    procedure SetData(const Value: TDataItem);
    procedure SetExpression(const Value:TExpression);
    procedure SetName(const Value: String);
    function UniqueName(const AData:TDataItem):String;
  protected
    Source : TExpression;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    procedure Clear;

    class function GuessType(const AData:TDataItem):TSummaryItemType; static;

    function RealData:TDataItem;
  published
    property Active:Boolean read FActive write SetActive default True;
    property Data:TDataItem read FData write SetData;
    property DestData:TDataItem read FDestData;
    property Expression:TExpression read Source write SetExpression;
    property Name:String read FName write SetName;
  end;

  // Apply a cumulative sum or difference with previous value
  TCalculationRunning=(No, Cumulative, Difference);

  // Calculate measure values as percentages on column total, row total, or grand total
  TCalculationPercentage=(None, Column, Row, Total);

  TMeasureCalculation=class(TBaseSummaryPersistent)
  private
    FPercentage : TCalculationPercentage;
    FRunning : TCalculationRunning;
    FRunningByRows : Boolean;

    procedure SetPercentage(const Value: TCalculationPercentage);
    procedure SetRunning(const Value: TCalculationRunning);
    procedure SetRunningByRows(const Value: Boolean);
  public
    procedure Assign(Source:TPersistent); override;
  published
    property Percentage : TCalculationPercentage read FPercentage write SetPercentage default TCalculationPercentage.None;
    property Running : TCalculationRunning read FRunning write SetRunning default TCalculationRunning.No;
    property RunningByRows : Boolean read FRunningByRows write SetRunningByRows default False;
  end;

  // Defines a data (item or expression), and an aggregation type
  TMeasure=class(TSummaryItem)
  private
    FAggregate : TAggregate;
    FCalculation: TMeasureCalculation;
    FMissing : TMeasureMissing;

    CalcCounts : Boolean;
    BinCounts : Array of TInt32Array;

    Current : TDataItem;
    Dest : TDataArray;

    FIsSum : Boolean; // Sum or Average

    procedure Accumulate(const Index,ByIndex,By2:TInteger);
    procedure CalcData;
    procedure CalculateAverages;
    procedure CalculatePercentages;
    procedure CalculateRunningValues;
    procedure ConvertToFloat;
    procedure Finish;
    procedure Prepare;
    procedure SetZeroAsMissing;
    procedure SetAggregate(const Value: TAggregate);
    procedure SetCalculation(const Value: TMeasureCalculation);
    procedure SetAllMissing;
    procedure SetMissing(const Value: TMeasureMissing);
  public
    Constructor Create; override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    function Clone:TMeasure;
    function Kind:TDataKind;
    function ToString:String; override;

  published
    property Aggregate : TAggregate read FAggregate write SetAggregate default TAggregate.Count;
    property Calculation : TMeasureCalculation read FCalculation write SetCalculation;
    property Missing : TMeasureMissing read FMissing write SetMissing;
  end;

  TMeasures=Array of TMeasure;

  // Pending: Remove this helper, use a TList
  TMeasuresHelper=record helper for TMeasures
  private
    function Active:TMeasures;
    function AddDirect(const AExpression: TExpression; const Aggregate: TAggregate):TMeasure;
    procedure Finish; inline;
    procedure Prepare; inline;
  public
    function Add(const AData: TDataItem; const Aggregate: TAggregate):TMeasure; overload;
    function Add(const AExpression: TExpression; const Aggregate: TAggregate):TMeasure; overload;
    procedure Append(const AMeasure:TMeasure);
    function Count:Integer; inline;
    procedure Delete(const Index:Integer);
    procedure Exchange(const A,B:Integer);
    function IndexOf(const AMeasure:TMeasure):Integer;
    procedure Remove(const AMeasure:TMeasure);
  end;

  // For date-time groups, specify which part of the datetime (by Year, by Month, etc)
  TGroupByDate=record
  private
    MinYear : Integer;

    function BinCount(const AData:TDataItem):Integer;
    function BinIndex(const ADate:TDateTime):TNativeInteger;
    procedure FillGroupBy(const Source,Dest:TDataItem; const Repeated,MaxSteps:TInteger); overload; // 1D
    procedure FillGroupBy(const Source:TDataItem; const Items:TDataArray); overload; // 2D
  public
    Part : TDateTimePart;
  end;

  // Orientation of a groupby
  TGroupByLayout=(Automatic,Rows,Items);

  // Defines the data (item or expression) used to create groups
  TGroupBy=class(TSummaryItem)
  private
    IsDataItem : Boolean; // optimization

    FHistogram : THistogram;

    FLayout : TGroupByLayout;

    Steps : TInteger;
    DestCount : TInteger;

    // For redundant groups:
    IParent : TGroupBy;

    function CalcBinCount: Integer;
    function BinIndex(Index:TInteger; out ABin:TNativeInteger):Boolean;
    function DestName:String;
    procedure DoFill;
    procedure DoFillParent;
    procedure FillDest(const ADest:TDataItem); // 2D
    procedure FillGroupBy(const AData:TDataItem; const Repeated,MaxSteps:TInteger); // 1D
    function GetDatePart: TDateTimePart; inline;

    procedure ObtainData(const AHops:TDataHops);
    procedure Prepare(const AData:TDataItem);
    procedure SetDatePart(const Value: TDateTimePart);
    procedure SetHistogram(const Value: THistogram);
    procedure TryFreeData;
  protected
    DateOptions : TGroupByDate;
    RealLayout : TGroupByLayout;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    function Clone:TGroupBy;
    function ToString:String; override;
  published
    property DatePart:TDateTimePart read GetDatePart write SetDatePart default TDateTimePart.None;
    property Histogram:THistogram read FHistogram write SetHistogram;
    property Layout:TGroupByLayout read FLayout write FLayout default TGroupByLayout.Automatic;
  end;

  TGroupBys=Array of TGroupBy;

  // Pending: Remove this helper, use a TList
  TGroupBysHelper=record helper for TGroupBys
  private
    function Active:TGroupBys;
    function AddDirect(const AExpression: TExpression): TGroupBy;
    procedure Extract(const AIndex:Integer);
    procedure GuessSteps;
    function Position(const Pos:TInteger; out Index:TInteger):Boolean;
    procedure RemoveRedundant;
    function Total:TInteger;
  public
    function Add(const AData:TDataItem):TGroupBy; overload;
    function Add(const AExpression: TExpression):TGroupBy; overload;
    procedure Append(const AGroupBy:TGroupBy);
    function Count:Integer; inline;
    procedure Delete(const AIndex:Integer);
    procedure Exchange(const A,B:Integer);
    function IndexOf(const AGroupBy:TGroupBy):Integer;
    procedure Remove(const AGroupBy:TGroupBy);
  end;

  TSummaryExpression=class(TExpression)
  private
    FItem : TSummaryItem;
    FExpression : String;

    IExpression : TExpression;
  public
    Destructor Destroy; override;

    procedure Assign(const Source:TExpression); override;
    class function FromString(const AItem:TSummaryItem; const AExpression:String):TSummaryExpression; static;

    function GetExpression(const AData:TDataItem):TExpression;
    function ToString:String; override;
    function Value:TData; override;
  end;

  TSummary=class;

  TSummaryFilter=class(TDataExpression)
  private
    Items : Array of TSummaryExpression;

    ISummary : TSummary;
  protected
    procedure Reset;
    class function Resolve(const AData:TDataItem; const AText:String;
                           const Error:TErrorProc):TExpression; override;
  public
    Destructor Destroy; override;

    procedure Add(const AItem:TSummaryItem; const AExpression:String); overload;
    procedure Add(const AExpression:String); overload;

    procedure Assign(const AFilter:TSummaryFilter);

    procedure Clear;

    function GetExpression(const AData:TDataItem):TExpression;
    function ToString:String; override;
  end;

  TRemoveMissing=class(TBaseSummaryPersistent)
  private
    FColumns : Boolean;
    FRows : Boolean;

    procedure SetColumns(const Value: Boolean);
    procedure SetRows(const Value: Boolean);
  public
    procedure Assign(Source:TPersistent); override;
  published
    property Columns:Boolean read FColumns write SetColumns default False;
    property Rows:Boolean read FRows write SetRows default False;
  end;

  TFilterEvent=function(const Sender:TSummary):Boolean of object;

  // Returns a data structure from measures and dimensions.
  // This is similar to an SQL select query.
  TSummary=class(TDataProvider)
  public // Made public for direct access from TBIQuery as per plan
    FTopValue: Int64;
    FTopIsPercent: Boolean;
  private
    FDescription : String;
    FFilter : TExpression;
    FRemove : TRemoveMissing;
    FUseFilter : Boolean;
    FHaving : TSummaryFilter;

    ActiveMeasures : TMeasures;
    ActiveMeasuresCount : Integer;

    ActiveBy : TGroupBys;
    ActiveByCount : Integer;

    procedure ApplyHaving(const AData:TDataItem);
    function BySameParent(const ABy:TGroupBy):TGroupBy;
    procedure DoRemoveMissing(const Data:TDataItem);
    procedure Fill;
    procedure GetActive;
    function GetHaving:TSummaryFilter;
    function GetMainData: TDataItem;
    procedure SetFilter(const Value: TExpression);
    procedure SetHaving(const Value: TSummaryFilter);
    procedure SetRemove(const Value: TRemoveMissing);
  protected
    ByRows,
    ByCols : TGroupBys;

    Hops : TDataHops;

    procedure FillGroupByRows;
    procedure GuessByLayout;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure SetDirectFilter(const Value: TExpression);
  public
    By : TGroupBys;
    Measures : TMeasures;
    SortBy : TSortItems;

    FilterEvent : TFilterEvent;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    function AddGroupBy(const AData: TDataItem):TGroupBy; overload; inline;
    function AddGroupBy(const AExpression: TExpression):TGroupBy; overload; inline;

    function AddMeasure(const AExpression:TExpression; const Aggregate:TAggregate):TMeasure; overload; inline;
    function AddMeasure(const AData:TDataItem; const Aggregate:TAggregate):TMeasure; overload; inline;

    procedure Assign(Source:TPersistent); override;

    function Calculate:TDataItem; overload;
    procedure Calculate(const AData:TDataItem); overload;

    procedure Clear;

    procedure Prepare;
    function ToString:String; override;
    function Valid:Boolean;

    property MainData:TDataItem read GetMainData;
  published
    property Description:String read FDescription write FDescription;
    property Filter:TExpression read FFilter write SetFilter;
    property Having:TSummaryFilter read GetHaving write SetHaving;
    property RemoveMissing:TRemoveMissing read FRemove write SetRemove;
    property UseFilter:Boolean read FUseFilter write FUseFilter default True;
  end;

  // Expression to Aggregate data using all items in a row
  TRowFunction=class(TColumnExpression)
  protected
    procedure Calculate(const Hops:TDataHops; const Dest:TDataItem); override;
    function Kind:TDataKind; override;
    class function TryParse(const S:String):TColumnExpression; override;
  public
    Operand : TAggregate;
    MissingAsZero : Boolean;

    function ToString:String; override;
    function Value:TData; override;
  end;

implementation

uses
  {System.}SysUtils, {System.}DateUtils,

  {$IFDEF FPC}
  BI.FPC,
  {$ELSE}
  System.Diagnostics,
  {$ENDIF}

  BI.Expression.DateTime, BI.Languages.English, BI.SQL, BI.Info;

{ THistogram }

Constructor THistogram.Create;
begin
  inherited Create;

  AutoMinimum:=True;
  AutoMaximum:=True;
end;

procedure THistogram.Assign(Source:TPersistent);
begin
  if Source is THistogram then
  begin
    FActive:=THistogram(Source).Active;

    Bins:=nil;
    FNumBins:=THistogram(Source).NumBins;
    HighBins:=-1;

    FAutoMinimum:=THistogram(Source).AutoMinimum;
    FMinimum:=THistogram(Source).Minimum;

    FAutoMaximum:=THistogram(Source).AutoMaximum;
    FMaximum:=THistogram(Source).Maximum;

    FBinSize:=THistogram(Source).BinSize;

    FFloatFormat:=THistogram(Source).FloatFormat;
  end
  else
    inherited;
end;

function THistogram.BinCount: Integer;
begin
  result:=Length(Bins);
end;

function THistogram.BinToString(const Index: Integer): String;

  function BinAsText:String;

    function CharOfBin(const AIndex:Integer):Integer;
    begin
      result:=Trunc(Bins[AIndex].Maximum);
    end;

  var tmpFirst,
      tmpLast,
      t : Integer;
  begin
    tmpLast:=CharOfBin(Index)-1;

    if tmpLast>OrdZ then
       tmpLast:=OrdZ;

    if BinCount<=AllChars then
    begin
      if Index=0 then
         tmpFirst:=OrdA
      else
         tmpFirst:=CharOfBin(Index-1);

      if tmpFirst<tmpLast-2 then
         result:=Chr(tmpFirst)+'..'+Chr(tmpLast)
      else
      begin
        result:=Chr(tmpFirst);

        for t:=tmpFirst+1 to tmpLast do
            result:=result+Chr(t);
      end;
    end
    else
      result:='';
  end;

var tmp : String;
begin
  if FSource.Kind=TDataKind.dkText then
     result:=BinAsText
  else
  if FSource.Kind=TDataKind.dkBoolean then
     result:=BoolToStr(Index=1,True)
  else
  begin
    tmp:=FloatFormat;

    if tmp='' then
       tmp:='0.###';  // <-- Pending: Automatic calculation of number of # decimals

    result:='<'+FormatFloat(tmp,Bins[Index].Maximum);
  end;
end;

function THistogram.BinOf(const AData:TDataItem; const AIndex:TInteger):Integer;

  // TODO: Replace with a binary search (for speed)
  function BinOf(const Value:TInteger):Integer; overload;
  var t : Integer;
  begin
    for t:=0 to HighBins do
        if Value<Bins[t].Maximum then
           Exit(t);

    result:=HighBins;
  end;

  function BinOf(const Value: String): Integer; overload;
  begin
    if Value='' then
       result:=0
    else
       result:=BinOf(Ord(UpCase(Value{$IFDEF FPC}[1]{$ELSE}.Chars[0]{$ENDIF})));
  end;

  function BinOf(const Value:TFloat):Integer; overload;
  var t : Integer;
  begin
    for t:=0 to HighBins do
        if Value<Bins[t].Maximum then
           Exit(t);

    result:=HighBins;
  end;

begin
  case AData.Kind of
     dkInt32: result:=BinOf(AData.Int32Data[AIndex]);
     dkInt64: result:=BinOf(AData.Int64Data[AIndex]);
    dkSingle: result:=BinOf(AData.SingleData[AIndex]);
    dkDouble: result:=BinOf(AData.DoubleData[AIndex]);
  dkExtended: result:=BinOf(AData.ExtendedData[AIndex]);
  dkDateTime: result:=BinOf(AData.DateTimeData[AIndex]);
      dkText: result:=BinOf(AData.TextData[AIndex]);

   dkBoolean: if AData.BooleanData[AIndex] then
                 result:=1
              else
                 result:=0;
  else
    result:=-1;
  end;
end;

procedure THistogram.Fill(var Bins:TBinArray);
var t : TLoopInteger;
    tmpBin : Integer;
begin
  if FSource.Kind<>dkUnknown then
  begin
    FSource.Load;

    for t:=0 to FSource.Count-1 do
      if not FSource.Missing[t] then
      begin
        tmpBin:=BinOf(FSource,t);

        if tmpBin<>-1 then // <-- assert?
           Inc(Bins[tmpBin].Count);
      end;
  end;
end;

function THistogram.Prepare(const Source:TDataItem):Integer;

  function PrepareBins(Num:Integer):TBinArray;
  var t : Integer;
      tmpMin,
      tmpMax : Extended;
      tmpBin : TFloat;
  begin
    SetLength(result,Num);

    if FSource.Kind=TDataKind.dkBoolean then
    begin
      result[0].Maximum:=0;
      result[1].Maximum:=1;
    end
    else
    begin
      if FSource.Kind=TDataKind.dkText then
      begin
        tmpMin:=Ord('A');
        tmpMax:=Ord('Z')+2;
      end
      else
        TDataInfo.GetMinMax(FSource,tmpMin,tmpMax);

      if not AutoMinimum then
         tmpMin:=Minimum;

      if not AutoMaximum then
         tmpMax:=Maximum;

      if BinSize=0 then
         tmpBin:=(tmpMax-tmpMin)/Num
      else
         tmpBin:=BinSize;

      if FSource.Kind<>TDataKind.dkUnknown then
         for t:=0 to Num-1 do
             result[t].Maximum:=tmpMin+(t+1)*tmpBin;

      if (BinSize<>0) or (not AutoMaximum) then
      begin
        if FSource.Kind=TDataKind.dkText then
           tmpMax:=Ord('Z')+2
        else
           TDataInfo.GetMinMax(FSource,tmpMin,tmpMax);

        if tmpMax>result[Num-1].Maximum then
        begin
          Inc(Num);
          SetLength(result,Num);
          result[Num-1].Maximum:=tmpMax;
        end;
      end;
    end;
  end;

begin
  FSource:=Source;

  if FSource=nil then
  begin
    result:=0;
    Bins:=nil;
  end
  else
  begin
    if FSource.Kind=TDataKind.dkBoolean then
       result:=2
    else
    begin
      result:=NumBins;

      if result=0 then
         if FSource.Kind=TDataKind.dkText then
            result:=9
         else
            result:=10 // <-- Make this count more smart: Use FSource.Stats.Range etc
      else
      if FSource.Kind=TDataKind.dkText then
         if result>AllChars then
            result:=AllChars;
    end;

    if result>0 then
       Bins:=PrepareBins(result)
    else
       Bins:=nil;
  end;

  HighBins:=High(Bins);
end;

procedure THistogram.SetActive(const Value: Boolean);
begin
  if FActive<>Value then
  begin
    FActive:=Value;
    Changed;
  end;
end;

procedure THistogram.SetAutoMaximum(const Value: Boolean);
begin
  if FAutoMaximum<>Value then
  begin
    FAutoMaximum := Value;
    Changed;
  end;
end;

procedure THistogram.SetAutoMinimum(const Value: Boolean);
begin
  if FAutoMinimum<>Value then
  begin
    FAutoMinimum := Value;
    Changed;
  end;
end;

procedure THistogram.SetBinSize(const Value: TFloat);
begin
  if FBinSize<>Value then
  begin
    FBinSize := Value;
    Changed;
  end;
end;

procedure THistogram.SetMaximum(const Value: TFloat);
begin
  if FMaximum<>Value then
  begin
    FMaximum := Value;
    Changed;
  end;
end;

procedure THistogram.SetMinimum(const Value: TFloat);
begin
  if FMinimum<>Value then
  begin
    FMinimum := Value;
    Changed;
  end;
end;

procedure THistogram.SetNumBins(const Value: Integer);
begin
  if FNumBins<>Value then
  begin
    FNumBins := Value;
    Changed;
  end;
end;

function THistogram.Calculate(const Source:TDataItem):TDataItem;
var t,
    tmp : Integer;

    tmpUpTo,
    tmpCount : TDataItem;
begin
  result:=TDataItem.Create(True);
  result.Name:=DefaultName;

  tmpUpTo:=result.Items.Add(UpToText,dkDouble,Source);

  if Source=nil then
     tmpCount:=nil
  else
     tmpCount:=result.Items.Add(CountOfText+Source.Name,dkInt32,Source);

  tmp:=Prepare(Source);

  if tmp>0 then
  begin
    Fill(Bins);

    result.Resize(tmp);

    for t:=0 to tmp-1 do
    begin
      tmpUpTo.DoubleData[t]:=Bins[t].Maximum;
      tmpCount.Int32Data[t]:=Bins[t].Count;
    end;
  end;
end;

{ TMeasuresHelper }

function TMeasuresHelper.Active: TMeasures;
var t,
    L : Integer;
begin
  result:=nil;

  L:=0;

  for t:=0 to High(Self) do
      if Self[t].Active and (Self[t].Source<>nil) then
      begin
        SetLength(result,L+1);
        result[L]:=Self[t];
        Inc(L);
      end;
end;

function TMeasuresHelper.AddDirect(const AExpression: TExpression; const Aggregate: TAggregate):TMeasure;
begin
  result:=TMeasure.Create;
  result.Source:=AExpression;
  result.Aggregate:=Aggregate;
  result.Active:=True;
end;

function TMeasuresHelper.Add(const AExpression: TExpression; const Aggregate: TAggregate):TMeasure;
begin
  if AExpression=nil then
     raise EBIException.Create(BIMsg_Summary_MeasureNilExpression)
  else
  begin
    result:=AddDirect(AExpression,Aggregate);
    result.KeepSource:=True;

    Append(result);
  end;
end;

function TMeasuresHelper.Add(const AData: TDataItem; const Aggregate: TAggregate):TMeasure;
begin
  if AData=nil then
     raise EBIException.Create(BIMsg_Summary_MeasureNilData)
  else
  begin
    AData.Load;
    result:=AddDirect(TDataItemExpression.Create(AData),Aggregate);

    Append(result);
  end;
end;

procedure TMeasuresHelper.Append(const AMeasure: TMeasure);
var L : Integer;
begin
  L:=Length(Self);
  SetLength(Self,L+1);
  Self[L]:=AMeasure;
end;

function TMeasuresHelper.Count: Integer;
begin
  result:=Length(Self);
end;

procedure TMeasuresHelper.Delete(const Index: Integer);
var t : Integer;
begin
  if (Index>=0) and (Index<Length(Self)) then
  begin
    Self[Index].Free;

    // Slow and not backwards compatible:
    // System.Delete(Self,Index,1);

    for t:=Index to Count-2 do
        Self[t]:=Self[t+1];

    SetLength(Self,Count-1);
  end
  else
    raise EBIException.CreateFmt(BIMsg_Summary_DeleteMeasure,[Index]);
end;

procedure TMeasuresHelper.Exchange(const A, B: Integer);

  procedure DoError(const Index:Integer);
  begin
    raise EBIException.CreateFmt(BIMsg_Summary_SwapMeasure,[Index]);
  end;

var tmp : TMeasure;
begin
  if A<>B then
  begin
    if (A>=0) and (A<Length(Self)) then
    begin
      if (B>=0) and (B<Length(Self)) then
      begin
        tmp:=Self[B];
        Self[B]:=Self[A];
        Self[A]:=tmp;
      end
      else
        DoError(B);
    end
    else
      DoError(A);
  end;
end;

procedure TMeasuresHelper.Finish;
var t : Integer;
begin
  for t:=0 to High(Self) do
      Self[t].Finish;
end;

function TMeasuresHelper.IndexOf(const AMeasure: TMeasure): Integer;
var t : Integer;
begin
  for t:=0 to Count-1 do
      if Self[t]=AMeasure then
         Exit(t);

  result:=-1;
end;

procedure TMeasuresHelper.Prepare;
var t : Integer;
begin
  for t:=0 to High(Self) do
      Self[t].Prepare;
end;

procedure TMeasuresHelper.Remove(const AMeasure: TMeasure);
var t,
    tmp : Integer;
begin
  tmp:=IndexOf(AMeasure);

  if tmp<>-1 then
  begin
    {$IFDEF AUTOREFCOUNT}
    AMeasure.DisposeOf;
    {$ELSE}
    AMeasure.Free;
    {$ENDIF}

    // Slow and not backwards compatible:
    // System.Delete(Self,tmp,1);

    for t:=tmp to Count-2 do
        Self[t]:=Self[t+1];

    SetLength(Self,Count-1);
  end;
end;

{ TGroupBysHelper }

function TGroupBysHelper.Add(const AData: TDataItem): TGroupBy;
begin
  if AData=nil then
     raise EBIException.Create(BIMsg_Summary_GroupByNilData)
  else
  begin
    AData.Load;

    result:=AddDirect(TDataItemExpression.Create(AData));

    Append(result);
  end;
end;

function TGroupBysHelper.Active: TGroupBys;
var t,
    L : Integer;
begin
  result:=nil;

  L:=0;

  for t:=0 to High(Self) do
      if Self[t].Active and (Self[t].Source<>nil) then
      begin
        SetLength(result,L+1);
        result[L]:=Self[t];
        Inc(L);
      end;
end;

function TGroupBysHelper.AddDirect(const AExpression: TExpression): TGroupBy;
begin
  result:=TGroupBy.Create;
  result.Source:=AExpression;
  result.Active:=True;
end;

function TGroupBysHelper.Add(const AExpression: TExpression): TGroupBy;
begin
  if AExpression=nil then
     raise EBIException.Create(BIMsg_Summary_GroupByNilExpression)
  else
  begin
    result:=AddDirect(AExpression);
    result.KeepSource:=True;

    Append(result);
  end;
end;

procedure TGroupBysHelper.Append(const AGroupBy: TGroupBy);
var L : Integer;
begin
  L:=Length(Self);
  SetLength(Self,L+1);
  Self[L]:=AGroupBy;
end;

function TGroupBysHelper.Count: Integer;
begin
  result:=Length(Self);
end;

procedure TGroupBysHelper.Delete(const AIndex: Integer);
begin
  if (AIndex>=0) and (AIndex<Length(Self)) then
     Remove(Self[AIndex])
  else
     raise EBIException.CreateFmt(BIMsg_Summary_DeleteGroupBy,[AIndex]);
end;

procedure TGroupBysHelper.Exchange(const A, B: Integer);

  procedure DoError(const Index:Integer);
  begin
    raise EBIException.CreateFmt(BIMsg_Summary_SwapGroupBy,[Index]);
  end;

var tmp : TGroupBy;
begin
  if A<>B then
  begin
    if (A>=0) and (A<Length(Self)) then
    begin
      if (B>=0) and (B<Length(Self)) then
      begin
        tmp:=Self[B];
        Self[B]:=Self[A];
        Self[A]:=tmp;
      end
      else
        DoError(B);
    end
    else
      DoError(A);
  end;
end;

procedure TGroupBysHelper.Extract(const AIndex:Integer);
var t : Integer;
begin
  // Slow and not backwards compatible:
  // System.Delete(Self,AIndex,1);

  for t:=AIndex to Count-2 do
      Self[t]:=Self[t+1];

  SetLength(Self,Count-1);
end;

procedure TGroupBysHelper.Remove(const AGroupBy:TGroupBy);
var tmp : Integer;
begin
  tmp:=IndexOf(AGroupBy);

  if tmp<>-1 then
  begin
    {$IFDEF AUTOREFCOUNT}
    AGroupBy.DisposeOf;
    {$ELSE}
    AGroupBy.Free;
    {$ENDIF}

    Extract(tmp);
  end;
end;

procedure TGroupBysHelper.RemoveRedundant;
var t : Integer;
begin
  t:=0;

  while t<Count do
  begin
    //Self[t].IParent:=BySameParent(Self[t]);

    if Self[t].IParent=nil then
       Inc(t)
    else
       Extract(t);
  end;
end;

procedure TGroupBysHelper.GuessSteps;
var H,
    t, tt : Integer;
    tmp : TInteger;
begin
  H:=High(Self);

  for t:=0 to H do
  begin
    tmp:=1;

    for tt:=t+1 to H do
        tmp:=tmp*Self[tt].DestCount;

    Self[t].Steps:=tmp;
  end;
end;

function TGroupBysHelper.IndexOf(const AGroupBy: TGroupBy): Integer;
var t : Integer;
begin
  for t:=0 to Count-1 do
      if Self[t]=AGroupBy then
         Exit(t);

  result:=-1;
end;

function TGroupBysHelper.Position(const Pos: TInteger; out Index: TInteger): Boolean;
var t,
    H : Integer;

    tmp : TNativeInteger;
    tmpBy : TGroupBy;
begin
  Index:=0;

  H:=High(Self);

  for t:=H downto 0 do
  begin
    tmpBy:=Self[t];

    if tmpBy.BinIndex(Pos,tmp) then
       if t=H then
          Index:=tmp
       else
          Index:=Index+tmpBy.Steps*tmp
    else
       Exit(False);
  end;

  result:=True;
end;

function TGroupBysHelper.Total:TInteger;
var L,
    t : Integer;
begin
  L:=Length(Self);

  if L=0 then
     result:=0
  else
  begin
    result:=Self[0].DestCount;

    for t:=1 to L-1 do
        result:=result*Self[t].DestCount;
  end;
end;

{ TSummary }

Constructor TSummary.Create(AOwner: TComponent);
begin
  inherited;
  FUseFilter:=True;
  FRemove:=TRemoveMissing.Create;
  FTopValue := 0;
  FTopIsPercent := False;
end;

Destructor TSummary.Destroy;
begin
  Clear;
  Hops.Free;

  FRemove.Free;
  inherited;
end;

procedure TSummary.Clear;
var t : Integer;
begin
  for t:=0 to High(Measures) do
      Measures[t].Free;

  Measures:=nil;

  for t:=0 to High(By) do
      By[t].Free;

  By:=nil;

  FFilter.Free;
  FFilter:=nil;

  FTopValue := 0;        // Reset TopValue
  FTopIsPercent := False; // Reset TopIsPercent

  FHaving.Free;
  FHaving:=nil;

  RemoveMissing.FColumns:=False;
  RemoveMissing.FRows:=False;
end;

procedure TSummary.GetActive;
begin
  ActiveMeasures:=Measures.Active;
  ActiveMeasuresCount:=Length(ActiveMeasures);

  ActiveBy:=By.Active;
  ActiveByCount:=Length(ActiveBy);
end;

function TSummary.GetMainData: TDataItem;
begin
  if Hops=nil then
     Prepare;

  result:=Hops.Main;
end;

function TSummary.GetHaving: TSummaryFilter;
begin
  if FHaving=nil then
  begin
    FHaving:=TSummaryFilter.Create;
    FHaving.ISummary:=Self;
  end;

  result:=FHaving;
end;

type
  TDataItemAccess=class(TDataItem);

procedure TSummary.Load(const AData: TDataItem; const Children: Boolean);
begin
  AData.Clear;
  Calculate(AData);
end;

procedure TSummary.Fill;

  procedure DoAccumulate(const AIndex:TLoopInteger; const APos:TInteger); overload;
  var t : Integer;
  begin
    for t:=0 to ActiveMeasuresCount-1 do
        ActiveMeasures[t].Accumulate(AIndex,APos,0);
  end;

  procedure DoAccumulate(const AIndex:TLoopInteger; const ARow,ACol:TInteger); overload;
  var t : Integer;
      tmp : TMeasure;
  begin
    for t:=0 to ActiveMeasuresCount-1 do
    begin
      tmp:=ActiveMeasures[t];
      tmp.Current:=tmp.Dest[ACol];
      tmp.Accumulate(AIndex,ARow,ACol);
    end;
  end;

  procedure FillNoGroups;
  var t : TLoopInteger;
  begin
    for t:=0 to Hops.Main.Count-1 do
    begin
      Hops.Invalidate(t);
      DoAccumulate(t,0);
    end;
  end;

  procedure FillNoGroupsFilter;
  var Pass : Boolean;
      t : TLoopInteger;
  begin
    for t:=0 to Hops.Main.Count-1 do
    begin
      Hops.Invalidate(t);

      Pass:=(not Assigned(FilterEvent)) or FilterEvent(Self);

      if Pass then
      begin
        if Filter<>nil then
           Pass:=Filter.Value; // <-- compiler error, needs temp boolean "Pass"

        if Pass then
           DoAccumulate(t,0);
      end;
    end;
  end;

  procedure FillGroups;
  var t : TLoopInteger;
      tmpRow,
      tmpCol : TInteger;
  begin
    for t:=0 to Hops.Main.Count-1 do
    begin
      Hops.Invalidate(t);

      if ByRows.Position(t,tmpRow) then
         if ByCols=nil then
            DoAccumulate(t,tmpRow)
         else
         if ByCols.Position(t,tmpCol) then
            DoAccumulate(t,tmpRow,tmpCol);
    end;
  end;

  procedure FillGroupsFilter;
  var Pass : Boolean;
      t : TLoopInteger;
      tmpRow,
      tmpCol : TInteger;
  begin
    for t:=0 to Hops.Main.Count-1 do
    begin
      Hops.Invalidate(t);

      Pass:=(not Assigned(FilterEvent)) or FilterEvent(Self);

      if Pass then
      begin
        if Filter<>nil then
           Pass:=Filter.Value; // <-- compiler error, needs temp boolean "Pass"

        if Pass then
           if ByRows.Position(t,tmpRow) then
              if ByCols=nil then
                 DoAccumulate(t,tmpRow)
              else
              if ByCols.Position(t,tmpCol) then
                 DoAccumulate(t,tmpRow,tmpCol);
      end;
    end;
  end;

var tmpUseFilter : Boolean;
begin
  ActiveMeasures.Prepare;

  tmpUseFilter:=UseFilter and ((Filter<>nil) or Assigned(FilterEvent));

  Hops.Main.Load;

  if (ByCols=nil) and (ByRows=nil) then
     if tmpUseFilter then
        FillNoGroupsFilter
     else
        FillNoGroups
  else
     if tmpUseFilter then
        FillGroupsFilter
     else
        FillGroups;

  ActiveMeasures.Finish;
end;

function TSummary.AddMeasure(const AData: TDataItem; const Aggregate: TAggregate):TMeasure;
begin
  result:=Measures.Add(AData,Aggregate);
end;

procedure TSummary.Assign(Source:TPersistent);
var tmpMeasure : TMeasure;
    tmpGroupBy : TGroupBy;
    Summary : TSummary;
begin
  Clear;

  if Source is TSummary then
  begin
    Summary:=TSummary(Source);

    for tmpMeasure in Summary.Measures do
        Measures.Append(tmpMeasure.Clone);

    for tmpGroupBy in Summary.By do
        By.Append(tmpGroupBy.Clone);

    FRemove.Assign(Summary.FRemove);
    Description:=Summary.Description;

    Filter:=Summary.FFilter;
    FUseFilter:=Summary.UseFilter;

    Having:=Summary.FHaving;

    SortBy:=Summary.SortBy;

    Self.FTopValue := Summary.FTopValue;         // Assign TopValue
    Self.FTopIsPercent := Summary.FTopIsPercent; // Assign TopIsPercent
  end;

  inherited;
end;

function TSummary.AddMeasure(const AExpression: TExpression; const Aggregate: TAggregate):TMeasure;
begin
  result:=Measures.Add(AExpression,Aggregate);
end;

function TSummary.AddGroupBy(const AExpression: TExpression):TGroupBy;
begin
  result:=By.Add(AExpression);
end;

function TSummary.AddGroupBy(const AData: TDataItem):TGroupBy;
begin
  result:=By.Add(AData);
end;

procedure TSummary.DoRemoveMissing(const Data:TDataItem);

  procedure RemoveColumns(const Data:TDataItem);

    procedure DoRemove(const AItems:TDataItems);
    var t : Integer;
        tmp : TDataItem;
    begin
      t:=0;

      while t<AItems.Count do
      begin
        tmp:=AItems[t];

        if (tmp.Missing.Count=tmp.Count) and tmp.Missing.All then
           tmp.Free
        else
        begin
          if tmp.AsTable then
          begin
            DoRemove(tmp.Items);

            if tmp.Items.Count=0 then
               tmp.Free
            else
               Inc(t);
          end
          else
            Inc(t);
        end;
      end;
    end;

  begin
    DoRemove(Data.Items);
  end;

  function RecursiveItems(const AData:TDataItem):TDataArray;
  var tmp : TDataArray;
      t : Integer;
  begin
    if AData.AsTable then
    begin
      result:=nil;

      tmp:=AData.Items.AsArray;

      for t:=0 to tmp.Count-1 do
          result.Add(RecursiveItems(tmp[t]));
    end
    else
    begin
      SetLength(result,1);
      result[0]:=AData;
    end;
  end;

  procedure DoRemoveRows;
  var t,
      tmpFrom,
      tmpCount : TLoopInteger;

      m,tt : Integer;
      tmpItems : Array of TDataArray;
      tmpDelete : Boolean;
  begin
    SetLength(tmpItems,ActiveMeasuresCount);

    for t:=0 to ActiveMeasuresCount-1 do
        tmpItems[t]:=RecursiveItems(ActiveMeasures[t].DestData);

    // Pending:
    // Check at least one tmpItems MissingCount is > 0, otherwise it is not
    // worth it to traverse all Data (no row will be deleted)

    t:=0;

    repeat
      tmpFrom:=t;
      tmpCount:=0;

      // For each output row
      while t<Data.Count do
      begin
        tmpDelete:=True;

        // For each active measure
        for m:=0 to High(tmpItems) do
        begin
          // Special case for Count measures, remove rows with all cells zero
          if ActiveMeasures[m].Aggregate=TAggregate.Count then
          begin
            // Any cell in the measure is > 0 ?
            for tt:=0 to tmpItems[m].Count-1 do
                if tmpItems[m][tt].Int64Data[t]>0 then
                begin
                  tmpDelete:=False;
                  break;
                end;
          end
          else
         // Any cell in the measure is not null ?
          for tt:=0 to tmpItems[m].Count-1 do
              if not tmpItems[m][tt].Missing[t] then
              begin
                tmpDelete:=False;
                break;
              end;

          if not tmpDelete then
             break;
        end;

        if tmpDelete then
        begin
          Inc(tmpCount);
          Inc(t);
        end
        else
           break;
      end;

      if tmpCount>0 then
      begin
        Data.Delete(tmpFrom,tmpCount);
        t:=tmpFrom+1;
      end
      else
        Inc(t);

    until t>=Data.Count;
  end;

begin
  if RemoveMissing.Columns then
     RemoveColumns(Data);

  if RemoveMissing.Rows then
     DoRemoveRows;
end;

procedure TSummary.Prepare;
var t : Integer;
begin
  Hops.Free;
  Hops:=TDataHops.Create;

  for t:=0 to High(By) do
      By[t].FDestData:=nil;

  if not Valid then
     raise EBIException.Create(BIMsg_Summary_AtLeastOne);

  if ActiveMeasuresCount=0 then
     Hops.Add(By[0].Source,True)
  else
  for t:=0 to ActiveMeasuresCount-1 do
  begin
    ActiveMeasures[t].CalcData;
    Hops.Add(ActiveMeasures[t].Source,True);
  end;

  if UseFilter and (Filter<>nil) then
     Hops.Add(Filter,False);

  Hops.Init;

  if Hops.Main=nil then
     raise EBIException.Create(BIMsg_Summary_NoMainData);
end;

// Assign FFilter without cloning
procedure TSummary.SetDirectFilter(const Value: TExpression);
begin
  FFilter.Free;
  FFilter:=Value;
end;

// Assign FFilter cloning Value
procedure TSummary.SetFilter(const Value: TExpression);
begin
  if FFilter<>Value then
     SetDirectFilter(TExpression.Clone(Value));
end;

procedure TSummary.SetHaving(const Value: TSummaryFilter);
begin
  if Value=nil then
     FreeAndNil(FHaving)
  else
     Having.Assign(Value); // TExpression.Clone(Value)
end;

procedure TSummary.SetRemove(const Value: TRemoveMissing);
begin
  FRemove.Assign(Value);
end;

function TSummary.ToString: String;
begin
  result:=TBISQL.From(Self);
end;

function TSummary.Valid: Boolean;
begin
  GetActive;

  result:=(ActiveMeasuresCount>0) or (ActiveByCount>0);
end;

procedure TSummary.ApplyHaving(const AData:TDataItem);
var t : TInteger;
    HavingHops : TDataHops;
    tmp : TExpression;
begin
  if ByCols=nil then // <-- pending case: multi-column summary
  begin
    HavingHops:=TDataHops.Create;
    try
      tmp:=Having.GetExpression(AData);

      HavingHops.Main:=AData;
      HavingHops.Add(tmp,False);
      HavingHops.Init;

      if HavingHops.Main=nil then
         raise EBIException.Create('Error: Incorrect Having filter');

      t:=0;

      while t<=AData.Count-1 do
      begin
        HavingHops.Invalidate(t);

        if tmp.Value then
           Inc(t)
        else
           AData.Delete(t); // delete row
      end;

      Having.Reset;

    finally
      HavingHops.Free;
    end;
  end;
end;

function TSummary.Calculate:TDataItem;
begin
  result:=TDataItem.Create;
  try
    Calculate(result);
  except
    on Exception do
    begin
      // Avoid memory leak
      result.Free;

      raise;
    end;
  end;
end;

function TSummary.BySameParent(const ABy:TGroupBy):TGroupBy;
var t : Integer;
begin
  for t:=0 to High(ActiveBy) do
      if ActiveBy[t]=ABy then
         break
      else
      // Pending: Use THops to determine if by[t] is the same as by[tt]
      if (ABy.FData.Parent=ActiveBy[t].FData.Master)
         or
         (
           (ABy.FData.Parent=ActiveBy[t].FData.Parent)
           and
           (ABy.FData.Parent<>Hops.Main)
           and
           (ABy.DatePart=TDateTimePart.None)
         )
         then
           Exit(ActiveBy[t]);

  result:=nil;
end;

procedure TSummary.GuessByLayout;
var t : Integer;
    tmp : TGroupBy;
begin
  ByRows:=nil;
  ByCols:=nil;

  for t:=0 to ActiveByCount-1 do
  begin
    tmp:=ActiveBy[t];

    // Try to find redundant existing groupby
    tmp.IParent:=BySameParent(tmp);

    if tmp.Layout=TGroupByLayout.Automatic then
    begin
      {
      if ThereIsHopBetween(tmp,other groups) then
         tmp.RealLayout:=TGroupByLayout.Rows
      else
      }
      if tmp.IParent=nil then
         if (t=1) and (Length(ByRows)>0) and (ActiveMeasuresCount>0) then
            tmp.RealLayout:=TGroupByLayout.Items
         else
            tmp.RealLayout:=TGroupByLayout.Rows
      else
         tmp.RealLayout:=tmp.IParent.RealLayout;
    end
    else
      tmp.RealLayout:=tmp.Layout;

    if tmp.RealLayout=TGroupByLayout.Rows then
       ByRows.Append(tmp)
    else
       ByCols.Append(tmp);
  end;
end;

procedure TSummary.Calculate(const AData:TDataItem);

  function ByDestGroup(const AMeasure:TMeasure; const Start:Integer; var DestPos:Integer):TDataItem;
  var t : Integer;
      tmpGroup,
      tmp : TDataItem;
      tmpBy : TGroupBy;
  begin
    result:=TDataItem.Create(True);

    tmpBy:=ByCols[Start];

    for t:=0 to tmpBy.DestCount-1 do
    begin
      if Start<High(ByCols) then
      begin
         tmp:=ByDestGroup(AMeasure,Start+1,DestPos);

         // Add an extra group table:
         tmpGroup:=TDataItem.Create(True);

         tmpGroup.Name:=tmpBy.DestName;
         tmpGroup.Items.Add(tmp);

         // Add the group items:
         result.Items.Add(tmpGroup);
      end
      else
      begin
        // Temporary name
        if AMeasure<>nil then
           AMeasure.Dest[DestPos]:=result.Items.Add('_'+IntToStr(t),AMeasure.Kind,AMeasure.Data);

        Inc(DestPos);
      end;
    end;

    tmpBy.FillDest(result);

    TDataItemAccess(result).TagObject:=tmpBy.RealData;
  end;

  procedure AddDest(const AData:TDataItem); overload;

    function ByGroup(const AGroup:TGroupBy):TDataItem;
    begin
      result:=TDataItem.Create;
      result.Name:=AGroup.DestName;
      TDataItemAccess(result).TagObject:=AGroup.Data;
    end;

  var t : Integer;
  begin
    for t:=0 to ByRows.Count-1 do
        AData.Items.Add(ByGroup(ByRows[t]));

    for t:=0 to ByCols.Count-1 do
        AData.Items.Add(ByGroup(ByCols[t]));
  end;

  procedure AddDest(var AMeasure:TMeasure; const AData:TDataItem); overload; // 2D
  var tmpCols : TDataItem;
      DestPos : Integer;
      tmpGroup : TDataItem;
  begin
    if ByCols=nil then
    begin
      AMeasure.Current:=AData.Items.Add(AMeasure.UniqueName(AData),AMeasure.Kind,AMeasure.Data);

      SetLength(AMeasure.Dest,1);
      AMeasure.Dest[0]:=AMeasure.Current;

      AMeasure.FDestData:=AMeasure.Current;
    end
    else
    begin
      SetLength(AMeasure.Dest,ByCols.Total); // <-- replace AMeasure.Dest with TDataItem? (nested cols)
      DestPos:=0;

      tmpCols:=TDataItem.Create(True);
      tmpCols.Name:=AMeasure.UniqueName(AData);

      tmpGroup:=ByDestGroup(AMeasure,0,DestPos);

      tmpCols.Items.Add(tmpGroup);

      TDataItemAccess(tmpCols).TagObject:=AMeasure.Data;

      AMeasure.FDestData:=tmpCols;

      AData.Items.Add(tmpCols);
    end;

    if AMeasure.Data<>nil then
    begin
      AMeasure.Data.Stats;

      if not AMeasure.DestData.AsTable then
         if (AMeasure.Aggregate=TAggregate.Minimum) or (AMeasure.Aggregate=TAggregate.Maximum) then
            AMeasure.DestData.Master:=AMeasure.Data;
    end;
  end;

  procedure FillRedundant;
  var t : Integer;
  begin
    for t:=0 to ActiveByCount-1 do
        if ActiveBy[t].IParent<>nil then
           ActiveBy[t].DoFillParent;
  end;

  procedure DoCalculate(const AResult:TDataItem);
  var tmpRows : Integer;
      t : Integer;
      t1 : TStopWatch;
  begin
    t1:=TStopwatch.StartNew;

    Prepare;

    if Description<>'' then
       AResult.Name:=Description;

    AResult.History.DateTime:=Now;

    for t:=0 to ActiveByCount-1 do
        ActiveBy[t].ObtainData(Hops);

    GuessByLayout;

    for t:=0 to ActiveByCount-1 do
        ActiveBy[t].Prepare(AResult);

    ByRows.RemoveRedundant;
    ByCols.RemoveRedundant;

    ByRows.GuessSteps;
    ByCols.GuessSteps;

    if ActiveMeasuresCount=0 then
       AddDest(AResult)
    else
    for t:=0 to ActiveMeasuresCount-1 do
        AddDest(ActiveMeasures[t],AResult);

    if ByRows=nil then
       tmpRows:=1
    else
       tmpRows:=ByRows.Total;

    AResult.Resize(tmpRows);

    if ActiveMeasuresCount>0 then
       Fill;

    if AResult.Items.Count>0 then
       TDataItemAccess(AResult).FCount:=AResult.Items[0].Count;

    if ByRows<>nil then
       FillGroupByRows;

    FillRedundant;

    if (FHaving<>nil) and (FHaving.Items<>nil) then
       ApplyHaving(AResult);

    DoRemoveMissing(AResult);

    if SortBy.ActiveCount>0 then
       SortBy.SortData(AResult);

      // Apply TOP N [PERCENT] logic after sorting
      if Self.FTopValue > 0 then // FTopValue holds N from TOP N or LIMIT N
      begin
        declare // Using declare block for local variable as per Delphi style
          EffectiveMaxRows: Int64;
        begin
          if Self.FTopIsPercent then
          begin
            if AResult.Count > 0 then
              EffectiveMaxRows := (AResult.Count * Self.FTopValue) div 100
            else
              EffectiveMaxRows := 0;
          end
          else // Absolute TOP N
          begin
            EffectiveMaxRows := Self.FTopValue;
          end;

          if AResult.Count > EffectiveMaxRows then
            AResult.Resize(EffectiveMaxRows);
        end;
      end;

    AResult.History.Times.Calculating:=t1.ElapsedMilliseconds;
    AResult.History.Times.Total:=AResult.History.Times.Calculating;
  end;

begin
  AData.AsTable:=True;
  DoCalculate(AData);
  TDataItemAccess(AData).ClearDelay;
end;

procedure TSummary.FillGroupByRows;
var t : Integer;
    tmp : TGroupBy;
begin
  for t:=0 to High(ByRows) do
  begin
    tmp:=ByRows[t];
    tmp.DoFill;

    if t>0 then
       tmp.FDestData.ParentGroup:=ByRows[t-1].FDestData;
  end;
end;

function DecadeOf(const AYear:Integer):Integer; inline;
begin
  result:=(AYear div 10); // Round to decades
end;

{ TGroupByDate }

function TGroupByDate.BinCount(const AData:TDataItem): Integer;

  function MaxYear:Word;
  begin
    result:=TBIDateTime.YearOf(TDateTimeStats(AData.Stats).Max);
  end;

begin
  case Part of
        TDateTimePart.Year: result:=MaxYear-MinYear+1;
      TDateTimePart.Decade: result:=DecadeOf(MaxYear)-DecadeOf(MinYear)+1;
     TDateTimePart.Century: result:=(MaxYear div 100)-(MinYear div 100)+1;
  TDateTimePart.Millennium: result:=(MaxYear div 1000)-(MinYear div 1000)+1;
  else
    result:=Part.High;
  end;
end;

function TGroupByDate.BinIndex(const ADate:TDateTime):TNativeInteger;
var tmpYear : Word;
begin
  case Part of
//       Nanosecond: ;
//      Microsecond: ;
     TDateTimePart.Millisecond: result:=MilliSecondOf(ADate);
TDateTimePart.HundredsOfSecond: result:=MilliSecondOf(ADate) div 10;
  TDateTimePart.TenthsOfSecond: result:=MilliSecondOf(ADate) div 100;
          TDateTimePart.Second: result:=SecondOf(ADate);
          TDateTimePart.Minute: result:=MinuteOf(ADate);
     TDateTimePart.QuarterHour: result:=MinuteOf(ADate) div 15;
            TDateTimePart.Hour: result:=HourOf(ADate);

       TDateTimePart.DayOfYear: result:=TBIDateTime.DayOfTheYear(ADate)-1;
      TDateTimePart.DayOfMonth: result:=TBIDateTime.DayOf(ADate)-1;
      TDateTimePart.WeekOfYear: result:=WeekOfTheYear(ADate)-1;

 TDateTimePart.WeekDay,
 TDateTimePart.ShortWeekDayName,
 TDateTimePart.LongWeekDayName: result:=DayOfTheWeek(ADate)-1;

   TDateTimePart.Month,
   TDateTimePart.ShortMonthName,
   TDateTimePart.LongMonthName: result:=TBIDateTime.MonthOf(ADate)-1;

         TDateTimePart.Quarter: result:=(TBIDateTime.MonthOf(ADate)-1) div 3;
  else
    begin
      tmpYear:=TBIDateTime.YearOf(ADate);

      case Part of
            TDateTimePart.Year: result:=tmpYear-MinYear;
          TDateTimePart.Decade: result:=DecadeOf(tmpYear)-DecadeOf(MinYear);
    TDateTimePart.DecadeOfYear: result:=((tmpYear div 10) mod 10);
         TDateTimePart.Century: result:=(tmpYear div 100)-(MinYear div 100);
      TDateTimePart.Millennium: result:=(tmpYear div 1000)-(MinYear div 1000);
      else
        result:=-1;
      end;

    end;
  end;
end;

procedure TGroupByDate.FillGroupBy(const Source: TDataItem; const Items: TDataArray);

  procedure FillInteger(const AMin:Integer);
  var t : Integer;
  begin
    for t:=0 to High(Items) do
        Items[t].Name:=IntToStr(AMin+t);
  end;

  procedure FillDecade;
  var t,
      tmp : Integer;
  begin
    tmp:=DecadeOf(MinYear);

    for t:=0 to High(Items) do
        Items[t].Name:=IntToStr(10*(tmp+t));
  end;

var t : Integer;
begin
  case Part of
    TDateTimePart.Millisecond,
    TDateTimePart.HundredsOfSecond,
    TDateTimePart.TenthsOfSecond,
    TDateTimePart.Second,
    TDateTimePart.Minute,
    TDateTimePart.QuarterHour,
    TDateTimePart.Hour        : FillInteger(0);

    TDateTimePart.DayOfMonth,
    TDateTimePart.DayOfYear,
    TDateTimePart.Month,
    TDateTimePart.WeekDay,
    TDateTimePart.WeekOfYear  : FillInteger(1);
    TDateTimePart.Year        : FillInteger(MinYear);
    TDateTimePart.Decade      : FillDecade;
    TDateTimePart.DecadeOfYear: FillInteger(1);
    TDateTimePart.Century     : FillInteger(1+(MinYear div 100));
    TDateTimePart.Millennium  : FillInteger(1+(MinYear div 1000));
  else
    for t:=0 to High(Items) do
        Items[t].Name:=Part.AsString(t);
  end;
end;

procedure TGroupByDate.FillGroupBy(const Source,Dest:TDataItem; const Repeated,MaxSteps:TInteger);

  procedure FillInteger(const AMin:Integer; const AStep:Integer=1);
  var t,tt : TLoopInteger;
      tmpStep : Integer;
      tmpMax : TInteger;
  begin
    tmpStep:=AMin;
    tmpMax:=AMin+(AStep*MaxSteps);

    t:=0;

    if Repeated=1 then
       while t<Dest.Count do
       begin
         Dest.Int32Data[t]:=tmpStep;

         Inc(t);
         Inc(tmpStep,AStep);

         if tmpStep>=tmpMax then
            tmpStep:=AMin;
       end
    else
       while t<Dest.Count do
       begin
         for tt:=0 to Repeated-1 do
             Dest.Int32Data[t+tt]:=tmpStep;

         Inc(t,Repeated);
         Inc(tmpStep,AStep);

         if tmpStep>=tmpMax then
            tmpStep:=AMin;
       end;
  end;

var t : TLoopInteger;
    tt : Integer;
    tmpIndex : Integer;
    tmpS : String;
begin
  case Part of
    TDateTimePart.Millisecond,
    TDateTimePart.HundredsOfSecond,
    TDateTimePart.TenthsOfSecond,
    TDateTimePart.Second,
    TDateTimePart.Minute,
    TDateTimePart.QuarterHour,
    TDateTimePart.Hour        : FillInteger(0);

    TDateTimePart.DayOfMonth,
    TDateTimePart.DayOfYear,
    TDateTimePart.Month,
    TDateTimePart.ShortWeekDayName,
    TDateTimePart.LongWeekDayName,
    TDateTimePart.ShortMonthName,
    TDateTimePart.LongMonthName,
    TDateTimePart.Quarter,
    TDateTimePart.WeekDay,
    TDateTimePart.WeekOfYear  : FillInteger(1);
          TDateTimePart.Year  : FillInteger(MinYear);
        TDateTimePart.Decade  : FillInteger(10*DecadeOf(MinYear),10);
  TDateTimePart.DecadeOfYear  : FillInteger(1);
       TDateTimePart.Century  : FillInteger(1+(MinYear div 100));
    TDateTimePart.Millennium  : FillInteger(1+(MinYear div 1000));
  else
  begin
    t:=0;
    tmpIndex:=1;

    while t<Dest.Count do
    begin
      tmpS:=Part.AsString(tmpIndex-1);

      for tt:=0 to Repeated-1 do
          Dest.TextData[t+tt]:=tmpS;

      Inc(t,Repeated);

      Inc(tmpIndex);

      if tmpIndex>MaxSteps then
         tmpIndex:=1;
    end;
  end;
  end;
end;

{ TAggregateHelper }

class function TAggregateHelper.AllToText: String;
const
  CRLF=#13#10;

var tmp : TAggregate;
begin
  result:=Low(TAggregate).ToString;

  for tmp:=TAggregate(1+Ord(Low(TAggregate))) to High(TAggregate) do
      result:=result+CRLF+tmp.ToString;
end;

function TAggregateHelper.SupportsAsZero:Boolean;
begin
  result:=(Self<>TAggregate.Count) and
          (Self<>TAggregate.First) and
          (Self<>TAggregate.Last);
end;

class function TAggregateHelper.FromString(const S:String; out Aggregate:TAggregate):Boolean;
var t : TAggregate;
begin
  for t:=Low(Names) to High(Names) do
      if SameText(S,Names[t]) then
      begin
        Aggregate:=t;
        Exit(True);
      end;

  result:=False;
end;

function TAggregateHelper.ToString: String;
begin
  result:=Names[Self];
end;

{ TMeasure }

Constructor TMeasure.Create;
begin
  inherited Create;

  FCalculation:=TMeasureCalculation.Create;
  FCalculation.IChanged:=DoChanged;

  FMissing:=TMeasureMissing.Create;
  FMissing.IChanged:=DoChanged;
end;

Destructor TMeasure.Destroy;
begin
  FMissing.Free;
  FCalculation.Free;
  inherited;
end;

{$IFDEF FPC}
{$IFNDEF CPUX64}
{$DEFINE HASEXTENDED}
{$ENDIF}
{$ELSE}
{$DEFINE HASEXTENDED}
{$ENDIF}

procedure TMeasure.Accumulate(const Index,ByIndex,By2: TInteger);

  procedure CheckMinMax(const Value:Integer); overload;
  var tmp : PInteger; // opt
  begin
    tmp:=@Current.Int32Data[ByIndex];

    if Aggregate=TAggregate.Minimum then
    begin
      if Value<tmp^ then
         tmp^:=Value;
    end
    else
    if Value>tmp^ then
       tmp^:=Value;
  end;

  procedure CheckMinMax(const Value:Int64); overload;
  var tmp : PInt64; // opt
  begin
    tmp:=@Current.Int64Data[ByIndex];

    if Aggregate=TAggregate.Minimum then
    begin
      if Value<tmp^ then
         tmp^:=Value;
    end
    else
    if Value>tmp^ then
       tmp^:=Value;
  end;

  procedure CheckMinMax(const Value:Single); overload;
  var tmp : PSingle; // opt
  begin
    tmp:=@Current.SingleData[ByIndex];

    if Aggregate=TAggregate.Minimum then
    begin
      if Value<tmp^ then
         tmp^:=Value;
    end
    else
    if Value>tmp^ then
       tmp^:=Value;
  end;

  procedure CheckMinMax(const Value:Double); overload;
  var tmp : PDouble; // opt
  begin
    tmp:=@Current.DoubleData[ByIndex];

    if Aggregate=TAggregate.Minimum then
    begin
      if Value<tmp^ then
         tmp^:=Value;
    end
    else
    if Value>tmp^ then
       tmp^:=Value;
  end;

  {$IFDEF HASEXTENDED}
  procedure CheckMinMax(const Value:Extended); overload;
  var tmp : PExtended; // opt
  begin
    tmp:=@Current.ExtendedData[ByIndex];

    if Aggregate=TAggregate.Minimum then
    begin
      if Value<tmp^ then
         tmp^:=Value;
    end
    else
    if Value>tmp^ then
       tmp^:=Value;
  end;
  {$ENDIF}

  procedure CheckMinMax(const Value:String); overload;
  begin
    if Aggregate=TAggregate.Minimum then
    begin
      if Value<Current.TextData[ByIndex] then
         Current.TextData[ByIndex]:=Value;
    end
    else
    if Value>Current.TextData[ByIndex] then
       Current.TextData[ByIndex]:=Value;
  end;

  procedure CheckMinMax(const Value:TDateTime); overload;
  var tmp : PDateTime; // opt
  begin
    tmp:=@Current.DateTimeData[ByIndex];

    if Aggregate=TAggregate.Minimum then
    begin
      if Value<tmp^ then
         tmp^:=Value;
    end
    else
    if Value>tmp^ then
       tmp^:=Value;
  end;

  procedure CheckMinMax(const Value:Boolean); overload;
  var tmp : PBoolean; // opt
  begin
    tmp:=@Current.BooleanData[ByIndex];

    if Aggregate=TAggregate.Minimum then
    begin
      if Value<tmp^ then
         tmp^:=Value;
    end
    else
    if Value>tmp^ then
       tmp^:=Value;
  end;

  procedure AccumulateData(const AData:TDataItem);

    // Recursive
    procedure DoAccumulate(const AItems:TDataArray);
    var t : Integer;
    begin
      for t:=0 to AItems.Count-1 do
          AccumulateData(AItems[t]);
    end;

    procedure DoSum(const tmp:PDouble);
    begin
      // Note: Do not add a local array variable set to "Current.DoubleData"
      // as this forces the compiler to add costly calls to System DynArrayAsg
      // and DynArrayClear.

      case AData.Kind of
          dkInt32: tmp^:=tmp^+AData.Int32Data[Index];
          dkInt64: tmp^:=tmp^+AData.Int64Data[Index];
         dkSingle: tmp^:=tmp^+AData.SingleData[Index];
         dkDouble: tmp^:=tmp^+AData.DoubleData[Index];
       dkExtended: tmp^:=tmp^+AData.ExtendedData[Index];
       dkDateTime: tmp^:=tmp^+AData.DateTimeData[Index];
        dkBoolean: tmp^:=tmp^+Ord(AData.BooleanData[Index]);
      end;
    end;

  begin
    if AData.AsTable then
       DoAccumulate(AData.Items.AsArray)
    else
    begin
      if Aggregate=TAggregate.First then
      begin
        if Current.Missing[ByIndex] then
        begin
          Current.Missing[ByIndex]:=False;
          Current.CopyFrom(ByIndex,AData,Index); // Copy first time
        end;
      end
      else
      if Aggregate=TAggregate.Last then
      begin
        Current.Missing[ByIndex]:=False;
        Current.CopyFrom(ByIndex,AData,Index); // Copy always
      end
      else
      begin
        if CalcCounts then
           Inc(BinCounts[By2,ByIndex]);

        if FIsSum then
           DoSum(@Current.DoubleData[ByIndex]) // <-- speed opt, pointer skips DynArrayxxx
        else
        begin
          if BinCounts[By2,ByIndex]=1 then
             Current.CopyFrom(ByIndex,AData,Index) // First time, just copy it
          else
          case AData.Kind of
              dkInt32: CheckMinMax(AData.Int32Data[Index]);
              dkInt64: CheckMinMax(AData.Int64Data[Index]);
             dkSingle: CheckMinMax(AData.SingleData[Index]);
             dkDouble: CheckMinMax(AData.DoubleData[Index]);
           dkExtended: CheckMinMax(AData.ExtendedData[Index]);
           dkDateTime: CheckMinMax(AData.DateTimeData[Index]);
               dkText: CheckMinMax(AData.TextData[Index]);
            dkBoolean: CheckMinMax(AData.BooleanData[Index]);
          end;
        end;
      end;
    end;
  end;

  // Warning: Speed opt. This sub procedure is important to be isolated
  // at it produces enter and exit (due to Variant, VarClear, etc) overheads.
  procedure AccumulateExpression;
  begin
    if Aggregate=TAggregate.Count then
       Inc(Current.Int64Data[ByIndex])
    else
    begin
      if CalcCounts then
         Inc(BinCounts[By2,ByIndex]);

      if FIsSum then
         Current.DoubleData[ByIndex]:=Current.DoubleData[ByIndex]+Source.Value
      else
      if BinCounts[By2,ByIndex]=1 then
         Current.DoubleData[ByIndex]:=Source.Value
      else
         CheckMinMax(TFloat(Source.Value));
    end;
  end;

begin
  // Special case for TDateSummary "Year" etc
  if Current.Count<=ByIndex then
     Current.Resize(ByIndex+1);

  if (Data=nil) or
     Missing.AsZero or
     (not Data.Missing[Index]) then
  begin
    if Data=nil then
       AccumulateExpression
    else
    if Aggregate=TAggregate.Count then
       Inc(Current.Int64Data[ByIndex])
    else
       AccumulateData(Data);
  end;
end;

function TMeasure.ToString:String;
var tmp : String;
begin
  if Source=nil then
     tmp:='?'
  else
     tmp:=Source.ToString;

  result:=Format(BIMsg_Summary_MeasureName,[Aggregate.ToString,tmp]);
end;

procedure TMeasure.CalcData;
begin
  if Source is TDataItemExpression then
  begin
    FData:=TDataItemExpression(Source).Data;

    if Data<>nil then
       Data.Load;
  end
  else
    FData:=nil;
end;

procedure TMeasure.Assign(Source:TPersistent);
begin
  if Source is TMeasure then
  begin
    FAggregate:=TMeasure(Source).FAggregate;
    Calculation:=TMeasure(Source).Calculation;
    Missing:=TMeasure(Source).Missing;
  end;

  inherited;
end;

function TMeasure.Clone: TMeasure;
begin
  result:=TMeasure.Create;
  result.Assign(Self);
end;

// Convert non-ocurrences (BinCounts zero) to missing values
procedure TMeasure.SetAggregate(const Value: TAggregate);
begin
  if FAggregate<>Value then
  begin
    FAggregate:=Value;
    Changed;
  end;
end;

procedure TMeasure.SetCalculation(const Value: TMeasureCalculation);
begin
  Calculation.Assign(Value);
end;

procedure TMeasure.SetMissing(const Value: TMeasureMissing);
begin
  Missing.Assign(Value);
end;

procedure TMeasure.SetZeroAsMissing;
var t : TLoopInteger;
    tt : TLoopInteger;
    Bins : TDoubleArray;
    tmpCounts : TInt32Array;
begin
  for t:=0 to High(Dest) do
  begin
    Bins:=Dest[t].DoubleData;
    tmpCounts:=BinCounts[t];

    for tt:=0 to Bins.Count-1 do
        if tmpCounts[tt]=0 then
           Dest[t].Missing[tt]:=True;
  end;
end;

// Divide all sums by its corresponding counts, to calculate averages
procedure TMeasure.CalculateAverages;
var t : TLoopInteger;
    tt : TLoopInteger;
    Bins : TDoubleArray;
    tmpCounts : TInt32Array;
begin
  // Each column:
  for t:=0 to High(Dest) do
  begin
    Bins:=Dest[t].DoubleData;
    tmpCounts:=BinCounts[t];

    for tt:=0 to Bins.Count-1 do
        if tmpCounts[tt]>0 then
           Bins[tt]:=Bins[tt]/tmpCounts[tt];
  end;
end;

// When calculating Percentage on a "Count" measure (Int64),
// its necessary to convert all outputs to Double, for percent decimals.
procedure TMeasure.ConvertToFloat;
var t : TLoopInteger;
    tt : TLoopInteger;
    tmp : TDataItem;
begin
  if (Length(Dest)>0) and (Dest[0].Kind<>TDataKind.dkDouble) and
     Dest[0].Kind.IsNumeric then

     for t:=0 to High(Dest) do
     begin
       tmp:=Dest[t];

       tmp.DoubleData.Resize(tmp.Count);

       case tmp.Kind of
         dkInt32: begin
                    for tt:=0 to tmp.Count-1 do
                        tmp.DoubleData[tt]:=tmp.Int32Data[tt];

                    tmp.Int32Data:=nil;
                  end;

         dkInt64: begin
                    for tt:=0 to tmp.Count-1 do
                        tmp.DoubleData[tt]:=tmp.Int64Data[tt];

                    tmp.Int64Data:=nil;
                  end;

        dkSingle: begin
                    for tt:=0 to tmp.Count-1 do
                        tmp.DoubleData[tt]:=tmp.SingleData[tt];

                    tmp.SingleData:=nil;
                  end;

      dkExtended: begin
                    for tt:=0 to tmp.Count-1 do
                        tmp.DoubleData[tt]:=tmp.ExtendedData[tt];

                    tmp.ExtendedData:=nil;
                  end;
       end;

       TDataItemAccess(tmp).FKind:=TDataKind.dkDouble;
     end;
end;

procedure TMeasure.CalculatePercentages;

  function GrandTotal:TFloat;
  var t : Integer;
  begin
    result:=0;

    for t:=0 to High(Dest) do
        result:=result+Dest[t].DoubleData.Sum;
  end;

  procedure Multiply(var Values:TDoubleArray; const Factor:TFloat);
  var t : TLoopInteger;
  begin
    for t:=0 to High(Values) do
        Values[t]:=Values[t]*Factor;
  end;

  procedure CalculateOnTotal;
  var t : TLoopInteger;
      tmp : TFloat;
  begin
    // First, calculate grand total
    tmp:=GrandTotal;

    if tmp<>0 then
    begin
      tmp:=100/tmp;

      // Apply to all values
      for t:=0 to High(Dest) do
          Multiply(Dest[t].DoubleData,tmp);
    end;
  end;

  procedure CalculateOnColumn;
  var t : TLoopInteger;
      tmp : TFloat;
      Bins : TDoubleArray;
  begin
    for t:=0 to High(Dest) do
    begin
      Bins:=Dest[t].DoubleData;

      // Column total
      tmp:=Bins.Sum;

      // Apply to column
      if tmp<>0 then
         Multiply(Bins,100/tmp);
    end;
  end;

  procedure CalculateOnRow;
  var t : TLoopInteger;
      tt : TLoopInteger;
      tmp : TFloat;
      tmpMax : Integer;
  begin
    if Length(Dest)>0 then
    begin
      tmpMax:=Dest[0].Count;

      // Each row:
      for t:=0 to tmpMax-1 do
      begin
        // First, calculate row total
        tmp:=0;

        for tt:=0 to High(Dest) do
            tmp:=tmp+Dest[tt].DoubleData[t];

        if tmp<>0 then
        begin
          // Apply to row values
          tmp:=100/tmp;

          for tt:=0 to High(Dest) do
              Dest[tt].DoubleData[t]:=Dest[tt].DoubleData[t]*tmp;
        end;
      end;
    end;
  end;

var t : Integer;
begin
  ConvertToFloat;

  case Calculation.Percentage of
    TCalculationPercentage.Total: CalculateOnTotal;
   TCalculationPercentage.Column: CalculateOnColumn;
  else
    CalculateOnRow;
  end;

  // Mark data as Percentages (to display 0.00% at BIDataset)
  for t:=0 to High(Dest) do
      Dest[t].NumericValues:=TNumericData.Percentages;
end;

procedure TMeasure.CalculateRunningValues;
var t,
    tt : TLoopInteger;
    tmpMax : TLoopInteger;
    Bins : TDoubleArray;
begin
  // Converting to float could not be mandatory, if we account for all the
  // different numeric data Kinds.
  ConvertToFloat;

  if Calculation.RunningByRows then
  begin
    if Length(Dest)>0 then
    begin
      tmpMax:=Dest[0].Count;

      // Each row:
      for t:=0 to tmpMax-1 do
      begin
        case Calculation.Running of
        Cumulative: for tt:=1 to High(Dest) do
                        Dest[tt].DoubleData[t]:=Dest[tt].DoubleData[t]+Dest[tt-1].DoubleData[t];

        Difference: for tt:=1 to High(Dest) do
                        Dest[tt].DoubleData[t]:=Dest[tt].DoubleData[t]-Dest[tt-1].DoubleData[t];

        end;
      end;
    end;
  end
  else
  begin
    // Each column:
    for t:=0 to High(Dest) do
    begin
      Bins:=Dest[t].DoubleData;

      case Calculation.Running of
        Cumulative: for tt:=1 to High(Bins) do
                        Bins[tt]:=Bins[tt]+Bins[tt-1];

        Difference: for tt:=1 to High(Bins) do
                        Bins[tt]:=Bins[tt]-Bins[tt-1];
      end;
    end;
  end;
end;

procedure TMeasure.Finish;
begin
  if Aggregate=TAggregate.Average then
     CalculateAverages;

  if Calculation.Percentage<>TCalculationPercentage.None then
     CalculatePercentages;

  if Calculation.Running<>TCalculationRunning.No then
     CalculateRunningValues;

  if (not Missing.AsZero) and Aggregate.SupportsAsZero then
     SetZeroAsMissing;
end;

function TMeasure.Kind: TDataKind;

  function CheckFirstRealItem(const AData:TDataItem):TDataItem;
  begin
    result:=AData;

    if result<>nil then
       while result.AsTable do
       begin
         if result.Items.Count>0 then
            result:=result.Items[0]
         else
            Exit;
       end;
  end;

var tmp : TDataItem;
begin
  if Aggregate=TAggregate.Count then
     result:=TDataKind.dkInt64
  else
  begin
    tmp:=CheckFirstRealItem(RealData);

    if tmp=nil then
       result:=TDataKind.dkDouble
    else
    begin
      case Aggregate of
      TAggregate.Average: if tmp.Kind=dkDateTime then
                             result:=TDataKind.dkDateTime
                          else
                          if tmp.Kind=TDataKind.dkExtended then
                             result:=TDataKind.dkExtended
                          else
                             result:=TDataKind.dkDouble;

          TAggregate.Sum: if tmp.Kind=TDataKind.dkExtended then
                             result:=TDataKind.dkExtended
                          else
                             result:=TDataKind.dkDouble;
      else
        result:=tmp.Kind;
      end;
    end;
  end;
end;

// For Aggregate "First" and "Last", flag all values initially as Missing
procedure TMeasure.SetAllMissing;
var t : Integer;
begin
  for t:=0 to High(Dest) do
      Dest[t].Missing.Init(Dest[t].Count);
end;

procedure TMeasure.Prepare;

  procedure PrepareBinCounts;
  var L,
      t : Integer;
  begin
    L:=Length(Dest);
    SetLength(BinCounts,L);

    for t:=0 to L-1 do
    begin
      BinCounts[t]:=nil; // <-- important, clear data
      BinCounts[t].Resize(Dest[t].Count{DoubleData.Count});
    end;
  end;

begin
  if Source<>nil then
     Source.Traverse(LoadData);

  FIsSum:=(Aggregate=TAggregate.Sum) or (Aggregate=TAggregate.Average);

  CalcCounts:=(Aggregate<>TAggregate.First) and (Aggregate<>TAggregate.Last);

  if CalcCounts then
     CalcCounts:=(not Missing.AsZero) or
                 (
                   (Aggregate=TAggregate.Average) or
                   (Aggregate=TAggregate.Minimum) or
                   (Aggregate=TAggregate.Maximum)
                 )
  else
     SetAllMissing;

  if CalcCounts then
     PrepareBinCounts;
end;

{ TGroupBy }

Constructor TGroupBy.Create;
begin
  inherited Create;

  FHistogram:=THistogram.Create;
  FHistogram.IChanged:=DoChanged;
end;

function TGroupBy.DestName: String;
begin
  if Name='' then
  begin
    if FData=nil then
       result:=''
    else
       result:=FData.Name;
  end
  else
    result:=Name;
end;

Destructor TGroupBy.Destroy;
begin
  FHistogram.Free;
  TryFreeData;

  inherited;
end;

type
  TExpressionColumnAccess=class(TExpressionColumn);

// Avoid memory leak when calling Summary.Calculate more than once
procedure TGroupBy.TryFreeData;
begin
  if (not IsDataItem) and (FData<>nil) then
  begin
    // Prevent double-free (see TSummaryItem.Destroy)
    TExpressionColumnAccess(FData).FExpression:=nil;

    FData.Free;
    FData:=nil;
  end;
end;

function TGroupBy.CalcBinCount: Integer;
begin
  if DateOptions.Part=TDateTimePart.None then
     if Histogram.Active then
        result:=FHistogram.BinCount
     else
     begin
       FData.Load;

       if FData.AsTable then
          result:=FData.Items.Count // pending verify groupby of "Columns" layout source
       else
       if FData.Kind=TDataKind.dkUnknown then
          result:=0
       else
       begin
         FData.Stats;
         result:=FData.DataMap.Count;
       end;
     end
  else
     result:=DateOptions.BinCount(FData);
end;

function TGroupBy.BinIndex(Index: TInteger; out ABin:TNativeInteger):Boolean;
begin
  if IsDataItem then
     Index:=TDataItemExpression(Source).Hops.Index; // opt

  result:=(Index<>-1) and (not FData.Missing[Index]);

  if result then
    if DateOptions.Part=TDateTimePart.None then
    begin
      if FHistogram.Active then
      begin
        ABin:=FHistogram.BinOf(FData,Index);

        if ABin=-1 then
           result:=False;
      end
      else
        result:=FData.FindInMap(Index,ABin);
    end
    else
      ABin:=DateOptions.BinIndex(FData.DateTimeData[Index])
end;

procedure TGroupBy.FillDest(const ADest:TDataItem);
var t : TLoopInteger;
    tmpMap : TDataMap;
begin
  FDestData:=ADest;
  FDestData.Name:=ToString;

//  FDestData.Master:=FData; // <-- Pending fix aggregate calc

  if DateOptions.Part=TDateTimePart.None then
  begin
    if FHistogram.Active then
       for t:=0 to FHistogram.HighBins do
           ADest.Items[t].Name:=FHistogram.BinToString(t)
    else
    begin
      tmpMap:=FData.DataMap;

      for t:=0 to tmpMap.Count-1 do
          ADest.Items[t].Name:=tmpMap.AsString(t);
    end;
  end
  else
    DateOptions.FillGroupBy(FData,ADest.Items.AsArray);
end;

procedure TGroupBy.FillGroupBy(const AData:TDataItem; const Repeated,MaxSteps:TInteger);

  procedure Fill(const AValues:TInt32Array); overload;
  var Pos,
      t,
      tt : TLoopInteger;
  begin
    Pos:=0;

    while Pos<AData.Count do
    for t:=0 to AValues.Count-1 do
        for tt:=1 to Repeated do
        begin
          AData.Int32Data[Pos]:=AValues[t];
          Inc(Pos);
        end;
  end;

  procedure Fill(const AValues:TInt64Array); overload;
  var Pos,
      t,
      tt : TLoopInteger;
  begin
    Pos:=0;

    while Pos<AData.Count do
    for t:=0 to AValues.Count-1 do
        for tt:=1 to Repeated do
        begin
          AData.Int64Data[Pos]:=AValues[t];
          Inc(Pos);
        end;
  end;

  procedure Fill(const AValues:TTextArray); overload;
  var Pos,
      t,
      tt : TLoopInteger;
  begin
    Pos:=0;

    while Pos<AData.Count do
    for t:=0 to AValues.Count-1 do
        for tt:=1 to Repeated do
        begin
          AData.TextData[Pos]:=AValues[t];
          Inc(Pos);
        end;
  end;

  procedure Fill(const AValues:TDateTimeArray); overload;
  var Pos,
      t,
      tt : TLoopInteger;
  begin
    Pos:=0;

    while Pos<AData.Count do
    for t:=0 to AValues.Count-1 do
        for tt:=1 to Repeated do
        begin
          AData.DateTimeData[Pos]:=AValues[t];
          Inc(Pos);
        end;
  end;

  procedure FillBoolean;
  var Pos,
      t : TLoopInteger;
  begin
    Pos:=0;

    while Pos<AData.Count do
    begin
      for t:=0 to Repeated-1 do
          AData.BooleanData[t+Pos]:=False;

      Inc(Pos,Repeated);

      for t:=0 to Repeated-1 do
          AData.BooleanData[t+Pos]:=True;

      Inc(Pos,Repeated);
    end;
  end;

  procedure FillHistogram;
  var Pos,
      t,
      tt : TLoopInteger;
  begin
    Pos:=0;

    while Pos<AData.Count do
    for t:=0 to FHistogram.HighBins do
        for tt:=1 to Repeated do
        begin
          AData.TextData[Pos]:=FHistogram.BinToString(t);
          Inc(Pos);
        end;
  end;

var t : TLoopInteger;
begin
  if DateOptions.Part=TDateTimePart.None then
     if FHistogram.Active then
        FillHistogram
     else
     begin
       if FData.AsTable then
       begin
         for t:=0 to AData.Count-1 do
             AData.Int32Data[t]:=t;
       end
       else
       case FDestData.Kind of
            dkInt32: Fill(TInt32Map(FData.DataMap).Items);
            dkInt64: Fill(TInt64Map(FData.DataMap).Items);
             dkText: Fill(TTextMap(FData.DataMap).Items);
         dkDateTime: Fill(TDateTimeMap(FData.DataMap).Items);
          dkBoolean: FillBoolean;
       end;
     end
  else
    DateOptions.FillGroupBy(AData,FDestData,Repeated,MaxSteps);
end;

function TGroupBy.GetDatePart: TDateTimePart;
begin
  result:=DateOptions.Part;
end;

procedure TGroupBy.ObtainData(const AHops:TDataHops);

  // Obtain FData depending on Source
  procedure CalcRealSource;
  begin
    IsDataItem:=Source is TDataItemExpression;

    if IsDataItem then
       FData:=TDataItemExpression(Source).Data
    else
    begin
      FData:=TExpressionColumn.Create(Source);
      TExpressionColumn(FData).Fill(AHops.Main);
    end;
  end;

begin
  AHops.Add(Source,False);

  TryFreeData;
  CalcRealSource;
end;

type
  TDataAccess=class(TDataItem);

procedure TGroupBy.Prepare(const AData:TDataItem);
var tmpKind : TDataKind;
    tmpMaster : TDataItem;
begin
  FData.Stats;

  if FData.Kind=dkDateTime then
     DateOptions.MinYear:=TBIDateTime.YearOf(TDateTimeStats(FData.Stats).Min)
  else
  if FHistogram.Active then
     FHistogram.Prepare(FData);

  if RealLayout=TGroupByLayout.Rows then
  begin
    tmpMaster:=nil;

    if (FData.Kind=dkDateTime) and (DateOptions.Part<>TDateTimePart.None) then
       tmpKind:=dkInt32
    else
    if FHistogram.Active then
       tmpKind:=dkText // ?? int32?
    else
    if FData.AsTable then
       tmpKind:=dkInt32
    else
    begin
      tmpKind:=FData.Kind;
      tmpMaster:=FData{.Master}; // <-- big change !
    end;

    FDestData:=AData.Items.Add(UniqueName(AData),tmpKind,FData);
    FDestData.Master:=tmpMaster;

    // This is only to display Week, Month and Quarter as string
    // (without loosing the correct sort order)
    TDataAccess(FDestData).SetInternalDate(DateOptions.Part);

    // When calculating summaries grouping by items that have integer to date
    // conversions (ShortMonthName, Weekday, etc), set output to same.
    // This is specially used in TSummaryTotals class
    if (DateOptions.Part=TDateTimePart.None) and (not Histogram.Active) then
       if TDataAccess(FData).IHasDate then
          TDataAccess(FDestData).SetInternalDate(TDataAccess(FData).IDate);
  end;

  DestCount:=CalcBinCount;
end;

procedure TGroupBy.SetDatePart(const Value: TDateTimePart);
begin
  if DateOptions.Part<>Value then
  begin
    if Value<>TDateTimePart.None then
       if (RealData<>nil) and (RealData.Kind<>TDataKind.dkDateTime) then
           raise EBIException.Create('Error: Data '+RealData.Name+' Kind is not date-time');

    DateOptions.Part:=Value;
    Changed;
  end;
end;

procedure TGroupBy.SetHistogram(const Value: THistogram);
begin
  FHistogram.Assign(Value)
end;

function TGroupBy.ToString: String;
begin
  if Source=nil then
     result:='?'
  else
     result:=Source.ToString;

  if DateOptions.Part<>TDateTimePart.None then
     result:=Format(BIMsg_Summary_MeasureName,[DateOptions.Part.ToString,result]);
end;

procedure TGroupBy.Assign(Source:TPersistent);
begin
  if Source is TGroupBy then
  begin
    DateOptions:=TGroupBy(Source).DateOptions;

    Layout:=TGroupBy(Source).Layout;
    RealLayout:=Layout;

    Histogram:=TGroupBy(Source).FHistogram;
  end;

  inherited;
end;

function TGroupBy.Clone: TGroupBy;
begin
  result:=TGroupBy.Create;
  result.Assign(Self);
end;

procedure TGroupBy.DoFill;
begin
  FillGroupBy(FDestData,Steps,DestCount);
end;

procedure TGroupBy.DoFillParent;
var tmpDest : TDataItem;
    t : TLoopInteger;
    tmp : TNativeInteger;
begin
  tmpDest:=IParent.FDestData;
  tmpDest.Stats;

  for t:=0 to tmpDest.Count-1 do
      if tmpDest.FindInMap(t,tmp) then
         FDestData.CopyFrom(t,FData,tmp)
      else
         FDestData.Missing[t]:=True;
end;

{ TSummaryItem }

Destructor TSummaryItem.Destroy;
begin
  Clear;
  inherited;
end;

class function TSummaryItem.GuessType(const AData: TDataItem): TSummaryItemType;

  function IsSmallMapSorted:Boolean;
  begin
    case AData.Kind of
      dkInt32: begin
                 AData.Load;
                 AData.Stats;

                 result:=(TInt32Map(AData.DataMap).Sorted<>TDataOrder.None)
                          //IsSequence(AData.Int32Data)
                          and (AData.Count<=32);
               end;

      dkInt64: begin
                 AData.Load;
                 AData.Stats;

                 result:=(TInt64Map(AData.DataMap).Sorted<>TDataOrder.None)
                          //IsSequence(AData.Int64Data)
                          and (AData.Count<=32);
               end;
    else
      result:=False;
    end;
  end;

  function OrdinalType:TSummaryItemType;
  begin
    if (TDataAccess(AData).IDate<>TDateTimePart.None) or
       //(UniqueRatio(AData)<=0.25) or
       IsSmallMapSorted then
         if TDataAccess(AData).IHasDate then
            result:=TSummaryItemType.GroupBy
         else
            result:=TSummaryItemType.GroupOrMeasure
    else
      result:=TSummaryItemType.Measure;
  end;

begin
  if TDataAccess(AData).HasMaster or AData.Primary then
     result:=TSummaryItemType.GroupBy
  else
  case AData.Kind of
    dkInt32,
    dkInt64   : result:=OrdinalType;

    dkText,
    dkDateTime,
    dkBoolean : result:=TSummaryItemType.GroupBy;

    dkUnknown : if AData.AsTable then
                   result:=TSummaryItemType.GroupOrMeasure // Count(*)
                else
                   result:=TSummaryItemType.GroupBy;
  else
    result:=TSummaryItemType.Measure;
  end;
end;

procedure TSummaryItem.Assign(Source:TPersistent);
begin
  if Source is TSummaryItem then
  begin
    FActive:=TSummaryItem(Source).Active;
    Self.Source:=TExpression.Clone(TSummaryItem(Source).Source);
    KeepSource:=False;

    FDestData:=nil;
  end
  else
    inherited;
end;

procedure TSummaryItem.Clear;
begin
  if not KeepSource then
     Source.Free;
end;

constructor TSummaryItem.Create;
begin
  inherited Create;
  FActive:=True;
end;

function TSummaryItem.RealData: TDataItem;
begin
  if Source is TDataItemExpression then
     result:=TDataItemExpression(Source).Data
  else
     result:=Data;
end;

procedure TSummaryItem.LoadData(const Item:TExpression);
begin
  if Item is TDataItemExpression then
     TDataItemExpression(Item).Data.Load;
end;

procedure TSummaryItem.SetActive(const Value: Boolean);
begin
  if FActive<>Value then
  begin
    FActive:=Value;
    Changed;
  end;
end;

procedure TSummaryItem.SetData(const Value: TDataItem);
begin
  if Value<>Data{RealData} then
  begin
    Source.Free;
    FData:=Value;
    Changed;
  end;
end;

procedure TSummaryItem.SetExpression(const Value: TExpression);
begin
  if Source<>Value then
  begin
    Source.Free;
    Source:=Value;
    Changed;
  end;
end;

procedure TSummaryItem.SetName(const Value: String);
begin
  if FName<>Value then
  begin
    FName:=Value;
    Changed;
  end;
end;

function TSummaryItem.UniqueName(const AData: TDataItem): String;
var tmp : String;
begin
  if Name='' then
     tmp:=ToString
  else
     tmp:=Name;

  result:=AData.Items.UniqueName(tmp);
end;

{ TSummaryExpression }

Destructor TSummaryExpression.Destroy;
begin
  IExpression.Free;
  inherited;
end;

procedure TSummaryExpression.Assign(const Source: TExpression);
var tmp : TSummaryExpression;
begin
  if Source is TSummaryExpression then
  begin
    tmp:=TSummaryExpression(Source);

    FExpression:=tmp.FExpression;

    if tmp.FItem is TMeasure then
       FItem:=TMeasure(tmp.FItem).Clone
    else
       FItem:=TGroupBy(tmp.FItem).Clone;

    IExpression.Free;
    IExpression:=nil;
  end
//  else
//    inherited;
end;

class function TSummaryExpression.FromString(const AItem: TSummaryItem;
  const AExpression: String): TSummaryExpression;
begin
  result:=TSummaryExpression.Create;
  result.FItem:=AItem;
  result.FExpression:=AExpression;
end;

function TSummaryExpression.GetExpression(const AData:TDataItem): TExpression;
var tmp : String;
begin
  if IExpression=nil then
  begin
    tmp:=ToString;
    IExpression:=TDataFilter.VerifyLogical(TSummaryFilter.FromString(AData,tmp),tmp,nil);
  end;

  result:=IExpression;
end;

function TSummaryExpression.ToString: String;
var tmp : TDataItem;
begin
  if FItem=nil then
     result:=FExpression
  else
  begin
    if FItem is TMeasure then
       tmp:=TMeasure(FItem).DestData
    else
    if FItem is TGroupBy then
       tmp:=TGroupBy(FItem).FDestData
    else
       Exit('');

    result:='{'+tmp.Name+'}'+FExpression;
  end;
end;

function TSummaryExpression.Value: TData;
begin
  if IExpression=nil then
     result:=False // <-- TExpression.Null ?
  else
     result:=IExpression.Value;
end;

{ TSummaryFilter }

Destructor TSummaryFilter.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSummaryFilter.Add(const AItem: TSummaryItem;
  const AExpression: String);
var L : Integer;
begin
  L:=Length(Items);
  SetLength(Items,L+1);
  Items[L]:=TSummaryExpression.FromString(AItem,AExpression);
end;

procedure TSummaryFilter.Add(const AExpression: String);
var L : Integer;
begin
  L:=Length(Items);
  SetLength(Items,L+1);
  Items[L]:=TSummaryExpression.FromString(nil,AExpression);
end;

procedure TSummaryFilter.Assign(const AFilter: TSummaryFilter);
var t,
    L : Integer;
begin
  Clear;

  L:=Length(AFilter.Items);
  SetLength(Items,L);

  for t:=0 to L-1 do
      Items[t]:=TExpression.Clone(AFilter.Items[t]) as TSummaryExpression;
end;

procedure TSummaryFilter.Clear;
var tmp : TSummaryExpression;
begin
  for tmp in Items do
      tmp.{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};

  Items:=nil;
end;

function TSummaryFilter.GetExpression(const AData: TDataItem): TExpression;
var t,
    L : Integer;
    tmp : TExpression;
begin
  L:=Length(Items);

  if L=0 then
     result:=nil
  else
  begin
    result:=Items[0].GetExpression(AData);

    if L>1 then
       for t:=1 to L-1 do
       begin
         tmp:=Items[t].GetExpression(AData);
         result:=TLogicalExpression.Create(result,TLogicalOperand.&And,tmp);
       end;
  end;
end;

procedure TSummaryFilter.Reset;
var tmp : TSummaryExpression;
begin
  for tmp in Items do
  begin
    tmp.IExpression.Free;
    tmp.IExpression:=nil;
  end;
end;

class function TSummaryFilter.Resolve(const AData: TDataItem;
  const AText: String; const Error: TErrorProc): TExpression;
begin
  if AData=nil then
     // Pending: use "ISummary" to search AText in all measures and groupbys,
     // if not found then call inherited
     result:=inherited
  else
  if SameText(AData.Name,AText) then
     result:=TDataItemExpression.Create(AData)
  else
     result:=inherited;
end;

function TSummaryFilter.ToString: String;
var tmp : TSummaryExpression;
begin
  result:='';

  for tmp in Items do
  begin
    if result<>'' then
       result:=result+' '+TLogicalOperand.&And.ToString+' ';

    result:=result+tmp.ToString;
  end;
end;

{ TMeasureCalculation }

procedure TMeasureCalculation.Assign(Source: TPersistent);
begin
  if Source is TMeasureCalculation then
  begin
    FRunning:=TMeasureCalculation(Source).FRunning;
    FRunningByRows:=TMeasureCalculation(Source).FRunningByRows;
    FPercentage:=TMeasureCalculation(Source).FPercentage;
  end
  else
    inherited;
end;

procedure TMeasureCalculation.SetPercentage(
  const Value: TCalculationPercentage);
begin
  if FPercentage<>Value then
  begin
    FPercentage:=Value;
    Changed;
  end;
end;

procedure TMeasureCalculation.SetRunning(const Value: TCalculationRunning);
begin
  if FRunning<>Value then
  begin
    FRunning:=Value;
    Changed;
  end;
end;

procedure TMeasureCalculation.SetRunningByRows(const Value: Boolean);
begin
  if FRunningByRows<>Value then
  begin
    FRunningByRows:=Value;
    Changed;
  end;
end;

{ TBaseSummaryPersistent }

procedure TBaseSummaryPersistent.Changed;
begin
  if Assigned(IChanged) then
     IChanged(Self);
end;

procedure TBaseSummaryPersistent.DoChanged(Sender: TObject);
begin
  Changed;
end;

{ TMeasureMissing }

procedure TMeasureMissing.Assign(Source: TPersistent);
begin
  if Source is TMeasureMissing then
     FAsZero:=TMeasureMissing(Source).FAsZero
  else
    inherited;
end;

procedure TMeasureMissing.SetAsZero(const Value: Boolean);
begin
  if FAsZero<>Value then
  begin
    FAsZero:=Value;
    Changed;
  end;
end;

{ TRowFunction }

class function TRowFunction.TryParse(const S:String):TColumnExpression;
var tmpOp : TAggregate;
begin
  if TAggregate.FromString(S,tmpOp) then
  begin
    result:=TRowFunction.Create(nil);
    TRowFunction(result).Operand:=tmpOp;
  end
  else
    result:=nil;
end;

procedure TRowFunction.Calculate(const Hops: TDataHops; const Dest: TDataItem);
var
  Cols : TDataArray;

  function ValueOf(const Col:TDataItem; const AIndex:Integer):Single;
  begin
    case Col.Kind of
       dkInt32: result:=Col.Int32Data[AIndex];
       dkInt64: result:=Col.Int64Data[AIndex];
      dkSingle: result:=Col.SingleData[AIndex];
      dkDouble: result:=Col.DoubleData[AIndex];
    dkExtended: result:=Col.ExtendedData[AIndex];
    dkDateTime: result:=Col.DateTimeData[AIndex];
    else
      result:=0;
    end;
  end;

  function SumCols(const AIndex:Integer):Single;
  var t : Integer;
  begin
    result:=0;

    for t:=0 to High(Cols) do
        result:=result+ValueOf(Cols[t],AIndex);
  end;

  function CountCols(const AIndex:Integer):Integer;
  var t : Integer;
  begin
    result:=0;

    for t:=0 to High(Cols) do
        if not Cols[t].Missing[AIndex] then
           Inc(result);
  end;

  function FirstNotMissing(const AIndex:Integer):Integer;
  var t : Integer;
  begin
    result:=-1;

    for t:=0 to High(Cols) do
        if not Cols[t].Missing[AIndex] then
           Exit(t);
  end;

  procedure CalcMinMax(const IsMin:Boolean);
  var t : TLoopInteger;
      L,
      First,
      tt : Integer;

      tmpValue,
      tmpValue2 : Single;
  begin
    L:=Length(Cols);

    for t:=0 to Dest.Count-1 do
    begin
      if MissingAsZero then
         First:=0
      else
         First:=FirstNotMissing(t);

      if First=-1 then
         Dest.Missing[t]:=True
      else
      begin
        tmpValue:=ValueOf(Cols[First],t);

        for tt:=First+1 to L-1 do
        begin
          if MissingAsZero or (not Cols[tt].Missing[t]) then
          begin
            tmpValue2:=ValueOf(Cols[tt],t);

            if IsMin then
            begin
              if tmpValue2<tmpValue then
                 tmpValue:=tmpValue2;
            end
            else
              if tmpValue2>tmpValue then
                 tmpValue:=tmpValue2;
          end;
        end;

        Dest.SingleData[t]:=tmpValue;
      end;
    end;
  end;

  procedure CalcAverage;
  var t : TLoopInteger;
      L : Integer;
  begin
    if MissingAsZero then
    begin
      L:=Length(Cols);

      for t:=0 to Dest.Count-1 do
          Dest.SingleData[t]:=SumCols(t)/L;
    end
    else
    begin
      for t:=0 to Dest.Count-1 do
      begin
        L:=CountCols(t);

        if L>0 then
           Dest.SingleData[t]:=SumCols(t)/L
        else
           Dest.Missing[t]:=True;
      end;
    end;
  end;

  procedure CalcCount;
  var t : TLoopInteger;
      L : Integer;
  begin
    if MissingAsZero then
    begin
      L:=Length(Cols);

      for t:=0 to Dest.Count-1 do
          Dest.SingleData[t]:=L;
    end
    else
       for t:=0 to Dest.Count-1 do
           Dest.SingleData[t]:=CountCols(t);
  end;

  procedure CalcSum;
  var t : TLoopInteger;
  begin
    if MissingAsZero then
       for t:=0 to Dest.Count-1 do
           Dest.SingleData[t]:=SumCols(t)
    else
    begin
      for t:=0 to Dest.Count-1 do
      begin
        if CountCols(t)>0 then
           Dest.SingleData[t]:=SumCols(t)
        else
           Dest.Missing[t]:=True;
      end;
    end;
  end;

var tmp : TDataItem;
begin
  if Expression is TDataItemExpression then
  begin
    tmp:=TDataItemExpression(Expression).Data;

    if (tmp is TDataItem) and (tmp.Master=nil) then
    begin
      Cols:=TDataItem(tmp).Items.AsArray;

      if Length(Cols)>0 then
      case Operand of
            Count: CalcCount;
              Sum: CalcSum;
          Average: CalcAverage;
          Minimum: CalcMinMax(True);
          Maximum: CalcMinMax(False);
      end;
    end;
  end;
end;

function TRowFunction.ToString: String;
begin
  result:=Operand.ToString+'(';

  if Expression<>nil then
     result:=result+Expression.ToString;

  result:=result+')';
end;

function TRowFunction.Value: TData;
begin
  {$IFDEF FPC}
  result:=TExpression.Null;
  {$ENDIF}

  raise EBIException.Create('Expression Row Function cannot be evaluated');
end;

function TRowFunction.Kind:TDataKind;
begin
  if Expression=nil then
     result:=dkUnknown
  else
     result:=dkSingle;
end;

{ TRemoveMissing }

procedure TRemoveMissing.Assign(Source: TPersistent);
begin
  if Source is TRemoveMissing then
  begin
    FColumns:=TRemoveMissing(Source).FColumns;
    FRows:=TRemoveMissing(Source).FRows;
  end
  else
    inherited;
end;

procedure TRemoveMissing.SetColumns(const Value: Boolean);
begin
  if FColumns<>Value then
  begin
    FColumns:=Value;
    Changed;
  end;
end;

procedure TRemoveMissing.SetRows(const Value: Boolean);
begin
  if FRows<>Value then
  begin
    FRows:=Value;
    Changed;
  end;
end;

initialization
  TDataFunctions.Register(TRowFunction);
finalization
  TDataFunctions.UnRegister(TRowFunction);
end.
