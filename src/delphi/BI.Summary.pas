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
  BI.Arrays, BI.Data, BI.Expression;

type
  // Count is the number of data values that belong to a Histogram interval <Maximum
  TBin=record
  public
    Maximum : TFloat;
    Count : TInteger;
  end;

  // Array of Histogram bins
  TBinArray=Array of TBin;

  // Accumulates data by numeric intervals
  THistogram=class
  private
    // Pending:
    // Derive THistogram from TSummaryItem, to allow expressions instead of just data Items.

    FActive : Boolean;

    FSource : TDataItem;

    Bins : TBinArray;
    HighBins : Integer;

    function BinOf(const AData:TDataItem; const AIndex:TInteger):Integer;
    function BinToString(const Index:Integer):String;

    procedure Fill(var Bins:TBinArray);
    function Prepare(const Source:TDataItem):Integer;
  public
    NumBins : Integer;
    BinSize : TFloat;

    AutoMinimum : Boolean;
    Minimum : TFloat;

    AutoMaximum : Boolean;
    Maximum : TFloat;

    FloatFormat : String;

    Constructor Create;

    procedure Assign(const Value:THistogram);

    function Calculate(const Source:TDataItem):TDataItem;
    function BinCount:Integer; inline;

    property Active:Boolean read FActive write FActive default False;
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
  TMeasureMissing=record
  public
    AsZero : Boolean;
  end;

  TSummaryItemType=(GroupBy, Measure, GroupOrMeasure);

  // Base class for summary Measures and GroupBy dimensions
  TSummaryItem=class(TPersistent)
  private
    FActive : Boolean;

    FDestData  : TDataItem;
    KeepSource : Boolean;
    Source     : TExpression;
    SourceData : TDataItem;

    procedure LoadData(const Item:TExpression);
    procedure SetExpression(const Value:TExpression);
    procedure SetData(const Value: TDataItem);
  public
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    procedure Clear;

    class function GuessType(const AData:TDataItem):TSummaryItemType; static;

    function RealData:TDataItem;

    property Active:Boolean read FActive write FActive default True;
    property Data:TDataItem read SourceData write SetData;
    property DestData:TDataItem read FDestData;
    property Expression:TExpression read Source write SetExpression;
  end;

  // Apply a cumulative sum or difference with previous value
  TCalculationRunning=(No, Cumulative, Difference);

  // Calculate measure values as percentages on column total, row total, or grand total
  TCalculationPercentage=(None, Column, Row, Total);

  TMeasureCalculation=record
  public
    Running : TCalculationRunning;
    RunningByRows : Boolean;

    Percentage : TCalculationPercentage;
  end;

  // Defines a data (item or expression), and an aggregation type
  TMeasure=class(TSummaryItem)
  private
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
  public
    Aggregate : TAggregate;
    Calculation : TMeasureCalculation;
    Missing : TMeasureMissing;

    procedure Assign(Source:TPersistent); override;

    function Clone:TMeasure;
    function Kind:TDataKind;
    function ToString:String; override;
  end;

  TMeasures=Array of TMeasure;

  // Pending: Remove this helper, use a TList
  TMeasuresHelper=record helper for TMeasures
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

    function BinCount(const AData:TDataItem): Integer;
    function BinIndex(const AData:TDataItem; const Index: TInteger; out ABin:TNativeInteger):Boolean;
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
    procedure DoFill;
    procedure DoFillParent;
    procedure FillDest(const ADest:TDataItem); // 2D
    procedure FillGroupBy(const AData:TDataItem; const Repeated,MaxSteps:TInteger); // 1D

    function GetHistogram: THistogram;
    procedure Prepare(const AHops:TDataHops; const AData:TDataItem);
    procedure SetHistogram(const Value: THistogram);
    procedure TryFreeData;
  protected
    RealLayout : TGroupByLayout;

    function HasHistogram:Boolean; inline;
  public
    DateOptions : TGroupByDate;

    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    function Clone:TGroupBy;
    function IsHistogram:Boolean; inline;
    function ToString:String; override;

    property Histogram:THistogram read GetHistogram write SetHistogram;
    property Layout:TGroupByLayout read FLayout write FLayout; // default TGroupByLayout.Automatic;
  end;

  TGroupBys=Array of TGroupBy;

  // Pending: Remove this helper, use a TList
  TGroupBysHelper=record helper for TGroupBys
  private
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

  // Returns a data structure from measures and dimensions.
  // This is similar to an SQL select query.
  TSummary=class(TDataProvider)
  private
    //GridMode : Boolean; // pending, temporary solution

    ActiveMeasures : TMeasures;
    ActiveMeasuresCount : Integer;

    ActiveBy : TGroupBys;
    ActiveByCount : Integer;

    FFilter : TExpression;
    FUseFilter : Boolean;

    FHaving : TSummaryFilter;

    FDescription : String;

    procedure ApplyHaving(const AData:TDataItem);
    procedure DoRemoveMissing(const Data:TDataItem);
    procedure Fill;
    procedure GetActive;
    function GetData: TDataItem;
    function GetHaving:TSummaryFilter;
    procedure SetFilter(const Value: TExpression);
    procedure SetHaving(const Value: TSummaryFilter);
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

    RemoveMissing,
    RemoveMissingCols : Boolean;

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

  published
    property Data:TDataItem read GetData;
    property Description:String read FDescription write FDescription;
    property Filter:TExpression read FFilter write SetFilter;
    property Having:TSummaryFilter read GetHaving write SetHaving;
    property UseFilter:Boolean read FUseFilter write FUseFilter default True;
  end;

implementation
