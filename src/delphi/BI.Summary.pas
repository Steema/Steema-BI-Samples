{*********************************************}
{  TeeBI Software Library                     }
{  Summary and Query classes                  }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Summary;

interface

uses
  System.Classes,
  {$IFNDEF FPC}
  System.Generics.Collections,
  {$ENDIF}
  BI.Arrays, BI.Data, BI.Expression, BI.Data.Expressions;

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
  TAggregate=(Count,Sum,Average,Minimum,Maximum); // pending: First, Last

  TAggregateHelper=record helper for TAggregate
  public
    class function AllToText:String; static;
    class function FromString(const S:String; out Aggregate:TAggregate):Boolean; static;
    function ToString:String;
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
    FData : TDataItem;
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
    function GetDatePart: TDateTimePart;

    procedure Prepare(const AHops:TDataHops; const AData:TDataItem);
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
    function Position(const Pos:TInteger; out Index:TInteger):Boolean;
    procedure GuessSteps;
    function Total:TInteger;
  public
    function Add(const AData:TDataItem):TGroupBy; overload;
    function Add(const AExpression: TExpression):TGroupBy; overload;
    procedure Append(const AGroupBy:TGroupBy);
    function Count:Integer; inline;
    procedure Delete(const Index:Integer);
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

  // Returns a data structure from measures and dimensions.
  // This is similar to an SQL select query.
  TSummary=class(TDataProvider)
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
    function KindOf:TDataKind; override;
    class function TryParse(const S:String):TColumnExpression; override;
  public
    Operand : TAggregate;
    MissingAsZero : Boolean;

    function ToString:String; override;
    function Value:TData; override;
  end;

implementation
