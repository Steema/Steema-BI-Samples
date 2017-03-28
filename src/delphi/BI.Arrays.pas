{*********************************************}
{  TeeBI Software Library                     }
{  Base Array helper classes                  }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Arrays;

// Supported from ide XE4 and up (needed: record helpers)

{$IF Declared(CompilerVersion)}
 {$IF CompilerVersion>=25}
  {$EXCESSPRECISION OFF} // x64 "to double" speedup (less precission)
 {$ENDIF}
{$ENDIF}

{$POINTERMATH ON}

interface

uses
  {System.}SysUtils, {System.}DateUtils;

{$IF Declared(CompilerVersion)}
 {$IF CompilerVersion>28}
  {.$DEFINE DELETEARRAY} // <--- Very Slow !!!
 {$ENDIF}
{$ENDIF}

type
  EBIException=class(Exception);

  // 32bit or 64bit
  TInteger=NativeInt;
  TLoopInteger=TInteger;

  TFloat=Double;  // Default float type: 8 bytes

  TBooleanArray=Array of Boolean;

  TTextArray=Array of String;

  TDateTimeArray=Array of TDateTime;

  TInt32Array=Array of Integer;
  TInt64Array=Array of Int64;

  {$IFDEF CPUX64}
  TNativeIntArray=TInt64Array;
  TNativeInteger=Int64;
  {$ELSE}
  TNativeIntArray=TInt32Array;
  TNativeInteger=Integer;
  {$ENDIF}

  TSingleArray=Array of Single;
  TDoubleArray=Array of Double;

  TExtendedArray={$IFDEF CPUX86}Array of Extended{$ELSE}TDoubleArray{$ENDIF};

  TFloatGrid=Array of TDoubleArray;
  TSquareGrid=type TFloatGrid;

  TDataOrder=(None, Ascending, Descending);

  TDataOrderHelper=record helper for TDataOrder
  public
    function ToString:String;
  end;

  TDataMap=class
  protected
    CachedCount : TInteger; // internal, used only when reading maps from streams
    FSorted : TDataOrder;
  public
    Map : TInt64Array;

    function AsString(const Index:TInteger):String; virtual; abstract;
    procedure Clear; virtual;
    function Count:TInteger; inline;

    property Sorted:TDataOrder read FSorted;
  end;

  TDataMapClass=class of TDataMap;

  TInt32Map=class(TDataMap)
  private
    procedure CreateMap(const AItems:TInt32Array);
    function GetItem(const Index:TInteger):Integer; inline;
  public
    Items : TInt32Array;

    procedure AddMap(const Value:Integer);
    function AsString(const Index:TInteger):String; override;
    procedure Clear; override;
    function Find(const Value:Integer; out Exists:Boolean):TNativeInteger; overload; inline;
    function Find(const Value:Integer; out AIndex:TNativeInteger):Boolean; overload; inline;

    property Item[const Index:TInteger]:Integer read GetItem; default;
  end;

  TInt64Map=class(TDataMap)
  private
    procedure CreateMap(const AItems:TInt64Array);
    function GetItem(const Index:TInteger):Int64; inline;
  public
    Items : TInt64Array;

    procedure AddMap(const Value:Int64);
    function AsString(const Index:TInteger):String; override;
    procedure Clear; override;
    function Find(const Value:Int64; out Exists:Boolean):TNativeInteger; overload; inline;
    function Find(const Value:Int64; out Index: TNativeInteger):Boolean; overload; inline;

    property Item[const Index:TInteger]:Int64 read GetItem; default;
  end;

  TSingleMap=class(TDataMap)
  private
    procedure CreateMap(const AItems:TSingleArray);
    function GetItem(const Index:TInteger):Single; inline;
  public
    Items : TSingleArray;

    procedure AddMap(const Value:Single);
    function AsString(const Index:TInteger):String; override;
    procedure Clear; override;
    function Find(const Value:Single; out Exists:Boolean):TNativeInteger; overload; inline;
    function Find(const Value:Single; out Index:TNativeInteger):Boolean; overload; inline;

    property Item[const Index:TInteger]:Single read GetItem; default;
  end;

  TDoubleMap=class(TDataMap)
  private
    procedure CreateMap(const AItems:TDoubleArray);
    function GetItem(const Index:TInteger):Double; inline;
  public
    Items : TDoubleArray;

    procedure AddMap(const Value:Double);
    function AsString(const Index:TInteger):String; override;
    procedure Clear; override;
    function Find(const Value:Double; out Exists:Boolean):TNativeInteger; overload; inline;
    function Find(const Value:Double; out Index:TNativeInteger):Boolean; overload; inline;

    property Item[const Index:TInteger]:Double read GetItem; default;
  end;

  {$IFDEF CPUX86}
  TExtendedMap=class(TDataMap)
  private
    procedure CreateMap(const AItems:TExtendedArray);
    function GetItem(const Index:TInteger):Extended; inline;
  public
    Items : TExtendedArray;

    procedure AddMap(const Value:Extended);
    function AsString(const Index:TInteger):String; override;
    procedure Clear; override;
    function Find(const Value:Extended; out Exists:Boolean):TNativeInteger; overload; inline;
    function Find(const Value:Extended; out Index:TNativeInteger):Boolean; overload; inline;

    property Item[const Index:TInteger]:Extended read GetItem; default;
  end;
  {$ELSE}
  TExtendedMap=TDoubleMap;
  {$ENDIF}

  TDateTimeMap=class(TDataMap)
  private
    procedure CreateMap(const AItems:TDateTimeArray);
    function GetItem(const Index:TInteger):TDateTime; inline;
  public
    Items : TDateTimeArray;

    procedure AddMap(const Value:TDateTime);
    function AsString(const Index:TInteger):String; override;
    procedure Clear; override;
    function Find(const Value:TDateTime; out Exists:Boolean):TNativeInteger; overload; inline;
    function Find(const Value:TDateTime; out Index: TNativeInteger):Boolean; overload; inline;

    property Item[const Index:TInteger]:TDateTime read GetItem; default;
  end;

  TTextMap=class(TDataMap)
  private
    FIgnoreCase : Boolean;

    procedure CreateMap(const AItems:TTextArray);
    function GetItem(const Index:TInteger):String; inline;
  public
    Items : TTextArray;

    procedure AddMap(const Value:String);
    function AsString(const Index:TInteger):String; override;
    procedure Clear; override;
    function Find(const Value:String; out Exists:Boolean):TNativeInteger; overload; inline;
    function Find(const Value:String; out Index: TNativeInteger):Boolean; overload; inline;

    property IgnoreCase:Boolean read FIgnoreCase;
    property Item[const Index:TInteger]:String read GetItem; default;
  end;

  TBooleanMap=class(TDataMap)
  public
    function AsString(const Index:TInteger):String; override;
  end;

  TDataStats=class
  public
    Mean : TFloat;
    Peakedness : TFloat; // Kurtosis, L-Moment
    Range : TFloat;
    Skewness : TFloat;
    StdDeviation : TFloat;
    Sum : TFloat;
    Variance : TFloat;
  end;

  TDataStatsClass=class of TDataStats;

  {$DEFINE GENERICSTATS}

  {$IFDEF GENERICSTATS}
  TDataStats<T>=class(TDataStats)
  public
    Min : T;
    Max : T;
    Median : T;
    Mode : T;
  end;
  {$ENDIF}

  {$IFDEF GENERICSTATS}
  TInt32Stats=class(TDataStats<Integer>)
  private
    procedure CalcStats(const Data:TInt32Array);
  end;
  {$ELSE}
  TInt32Stats=class(TDataStats)
  private
    procedure CalcStats(const Data:TInt32Array);
  public
    Min : Integer;
    Max : Integer;
    Median : Integer;
    Mode : Integer;
  end;
  {$ENDIF}

  {$IFDEF GENERICSTATS}
  TInt64Stats=class(TDataStats<Int64>)
  private
    procedure CalcStats(const Data:TInt64Array);
  end;
  {$ELSE}
  TInt64Stats=class(TDataStats)
  private
    procedure CalcStats(const Data:TInt64Array);
  public
    Min : Int64;
    Max : Int64;
    Median : Int64;
    Mode : Int64;
  end;
  {$ENDIF}

  {$IFDEF GENERICSTATS}
  TSingleStats=class(TDataStats<Single>);
  {$ELSE}
  TSingleStats=class(TDataStats)
  public
    Min : Single;
    Max : Single;
    Median : Single;
    Mode : Single;
  end;
  {$ENDIF}

  {$IFDEF GENERICSTATS}
  TDoubleStats=class(TDataStats<Double>);
  {$ELSE}
  TDoubleStats=class(TDataStats)
  public
    Min : Double;
    Max : Double;
    Median : Double;
    Mode : Double;
  end;
  {$ENDIF}

  {$IFDEF CPUX86}

  {$IFDEF GENERICSTATS}
  TExtendedStats=class(TDataStats<Extended>);
  {$ELSE}
  TExtendedStats=class(TDataStats)
  public
    Min : Extended;
    Max : Extended;
    Median : Extended;
    Mode : Extended;
  end;
  {$ENDIF}

  {$ELSE}
  TExtendedStats=TDoubleStats;
  {$ENDIF}

  {$IFDEF GENERICSTATS}
  TDateTimeStats=class(TDataStats<TDateTime>);
  {$ELSE}
  TDateTimeStats=class(TDataStats)
  public
    Min : TDateTime;
    Max : TDateTime;
    Median : TDateTime;
    Mode : TDateTime;
  end;
  {$ENDIF}

  {$IFDEF GENERICSTATS}
  TBooleanStats=class(TDataStats<Boolean>);
  {$ELSE}
  TBooleanStats=class(TDataStats)
  public
    Min : Boolean;
    Max : Boolean;
    Median : Boolean;
    Mode : Boolean;
  end;
  {$ENDIF}

  {$IFDEF GENERICSTATS}
  TTextStats=class(TDataStats<String>);
  {$ELSE}
  TTextStats=class(TDataStats)
  public
    Min : String;
    Max : String;
    Median : String;
    Mode : String;
  end;
  {$ENDIF}

  // Inverts all elements in array
  TArrayReverse<T>=record
  public
    class procedure Reverse(var Items:Array of T); static;
  end;

  TSwapProc=procedure(const A,B:TInteger) of object;

  TBooleanArrayHelper=record helper for TBooleanArray
  public
    function Append(const Value:Boolean):TInteger; overload; inline;
    procedure Append(const Value:TBooleanArray); overload;
    function Compare(const A,B:TInteger):ShortInt; inline;
    function Copy(const AIndex,ACount:TInteger):TBooleanArray; overload; inline;
    function Copy:TBooleanArray; overload; inline;
    function Copy(const Missing:TBooleanArray):TBooleanArray; overload;
    function Count:TInteger; inline;
    procedure Delete(const Index:TInteger; const ACount:TInteger=1); {$IFDEF DELETEARRAY}inline;{$ENDIF}
    procedure Empty; inline;
    function Equals(const Value:TBooleanArray):Boolean;
    function ExistsBefore(const AIndex:TInteger):Boolean;
    procedure Initialize(const Value:Boolean=False);
    procedure Insert(const Index:TInteger; const Value:Boolean);
    function Map:TBooleanMap;
    procedure Resize(const Count:TInteger); inline;
    procedure Reverse; inline;
    procedure Sort(const Ascending:Boolean; const Swap:TSwapProc); overload;
    procedure Sort(const Ascending:Boolean=True); overload; inline;
    function Stats:TBooleanStats; inline;
    procedure Swap(const A,B:TInteger); inline;
  end;

  TTextArrayHelper=record helper for TTextArray
  private
    type
      TCompareProc=function(const S1, S2: string): Integer;

    //function GuessOrdeness:Single; // <-- from 0..1 (Count of Inversions/Count) 0=Sorted
    function GuessOrder:TDataOrder;
  public
    function Append(const Value:String):TInteger; overload; inline;
    procedure Append(const Value:TTextArray); overload;
    function Copy(const AIndex,ACount:TInteger):TTextArray; overload;
    function Copy:TTextArray; overload;
    function Copy(const Missing:TBooleanArray):TTextArray; overload;
    function Count:TInteger; inline;
    procedure Delete(const Index:TInteger; const ACount:TInteger=1); {$IFDEF DELETEARRAY}inline;{$ENDIF}
    procedure Empty; inline;
    function Equals(const Value:TTextArray):Boolean;
    function ExistsBefore(const AIndex:TInteger):Boolean;
    function IndexOf(const Value:String):TInteger; overload; inline;
    function IndexOf(const Value:String; const CaseSensitive:Boolean):TInteger; overload;
    procedure Initialize(const Value:String='');
    procedure Insert(const Index:TInteger; const Value:String);
    function Map(const IgnoreCase:Boolean=False):TTextMap;
    function MaxLength:Integer;
    procedure Resize(const Count:TInteger); inline;
    procedure Reverse; inline;
    procedure Sort(const Ascending,IgnoreCase:Boolean; const Swap:TSwapProc); overload;
    procedure Sort(const Ascending:Boolean=True; const IgnoreCase:Boolean=False); overload; inline;
    function SortedFind(const Value: String; out Exists:Boolean; const IgnoreCase:Boolean): TNativeInteger;
    function Stats:TTextStats; inline;
    procedure Swap(const A,B:TInteger); inline;
  end;

  TDateTimeArrayHelper=record helper for TDateTimeArray
  private
    function Distribution(const Mean:TDateTime; const StdDeviation:TFloat; const Exponent:Integer):TFloat;
    function GuessOrder:TDataOrder;
  public
    function Append(const Value:TDateTime):TInteger; overload; inline;
    procedure Append(const Value:TDateTimeArray); overload;
    function Copy(const AIndex,ACount:TInteger):TDateTimeArray; overload; inline;
    function Copy:TDateTimeArray; overload; inline;
    function Copy(const Missing:TBooleanArray):TDateTimeArray; overload;
    function Count:TInteger; inline;
    procedure Delete(const Index:TInteger; const ACount:TInteger=1); {$IFDEF DELETEARRAY}inline;{$ENDIF}
    procedure Empty; inline;
    function ExistsBefore(const AIndex:TInteger):Boolean;
    procedure Initialize(const Value:TDateTime=0);
    procedure Insert(const Index:TInteger; const Value:TDateTime);
    function Map(out Median,Mode:TDateTime):TDateTimeMap; overload;
    function Maximum:TDateTime;
    function Mean:TDateTime;
    function Minimum:TDateTime;
    procedure Resize(const Count:TInteger); inline;
    procedure Reverse; inline;
    procedure Sort(const Ascending:Boolean; const Swap:TSwapProc); overload;
    procedure Sort(const Ascending:Boolean=True); overload; inline;
    function SortedFind(const Value:TDateTime; out Exists:Boolean):TNativeInteger;
    function Stats:TDateTimeStats;
    function StdDeviation(const Mean:TDateTime):TFloat;
    procedure Swap(const A,B:TInteger); inline;
    function Variance(const Mean:TDateTime):TFloat;
  end;

  TInt32ArrayHelper=record helper for TInt32Array
  private
    const
      SortThreshold=16;

    function Distribution(const Mean,StdDeviation:TFloat; const Exponent:Integer):TFloat;
    function GuessOrder:TDataOrder;
  public
    function Append(const Value:Integer):TInteger; overload; inline;
    procedure Append(const Value:TInt32Array); overload;
    function Copy(const AIndex,ACount:TInteger):TInt32Array; overload; inline;
    function Copy:TInt32Array; overload; inline;
    function Copy(const Missing:TBooleanArray):TInt32Array; overload;
    function Count:TInteger; inline;
    function Correlation(const Y: TInt32Array; const XMean,YMean: TFloat): TFloat;
    function CoVariance(const Y: TInt32Array; const XMean,YMean: TFloat):TFloat;
    procedure Delete(const Index:TInteger; const ACount:TInteger=1); {$IFDEF DELETEARRAY}inline;{$ENDIF}
    procedure Empty; inline;
    function Equals(const Value:TInt32Array):Boolean;
    function ExistsBefore(const AIndex:TInteger):Boolean;
    function IndexOf(const Value:Integer):TInteger;
    function IndexOfMax:TInteger;
    procedure Initialize(const Value:Integer=0);
    procedure Insert(const Index:TInteger; const Value:Integer);
    function Map(out Median,Mode:Integer):TInt32Map;
    function Maximum:Integer;
    function Mean:TFloat;
    function Minimum:Integer;
    procedure RemoveValue(const Value:Integer);
    procedure Resize(const Count:TInteger); inline;
    procedure Reverse; inline;
    procedure Sort(const FromIndex,ToIndex:TInteger; const Ascending:Boolean=True); overload;
    procedure Sort(const Ascending:Boolean; const Swap:TSwapProc); overload;
    procedure Sort(const Ascending:Boolean=True); overload; inline;
    function SortedFind(const Value: Integer; out Exists:Boolean):TNativeInteger;
    function Stats:TInt32Stats;
    function StdDeviation(const Mean:TFloat):TFloat;
    function Sum:TFloat;
    function SumOfSquares: TFloat;
    procedure Swap(const A,B:TInteger); inline;
    function Variance(const Mean:TFloat):TFloat;
  end;

  TInt64ArrayHelper=record helper for TInt64Array
  private
    const
      SortThreshold=16;

    function Distribution(const Mean,StdDeviation:TFloat; const Exponent:Integer):TFloat;
    function GuessOrder:TDataOrder;
  public
    function Append(const Value:Int64):TInteger; overload; inline;
    procedure Append(const Value:TInt64Array); overload;
    function Copy(const AIndex,ACount:TInteger):TInt64Array; overload; inline;
    function Copy:TInt64Array; overload; inline;
    function Copy(const Missing:TBooleanArray):TInt64Array; overload;
    function Count:TInteger; inline;
    function Correlation(const Y: TInt64Array; const XMean,YMean: TFloat): TFloat;
    function CoVariance(const Y: TInt64Array; const XMean,YMean: TFloat):TFloat;
    procedure Delete(const Index:TInteger; const ACount:TInteger=1); {$IFDEF DELETEARRAY}inline;{$ENDIF}
    procedure Empty; inline;
    function Equals(const Value:TInt64Array):Boolean;
    function ExistsBefore(const AIndex:TInteger):Boolean;
    function IndexOf(const Value:Int64):TInteger;
    function IndexOfMax:TInteger;
    procedure Initialize(const Value:Int64=0);
    procedure Insert(const Index:TInteger; const Value:Int64);
    function Map(out Median,Mode:Int64):TInt64Map;
    function Maximum:Int64;
    function Mean:TFloat;
    function Minimum:Int64;
    procedure RemoveValue(const Value:Int64);
    procedure Resize(const Count:TInteger); inline;
    procedure Reverse; inline;
    procedure Sort(const FromIndex,ToIndex:TInteger; const Ascending:Boolean=True); overload;
    procedure Sort(const Ascending:Boolean; const Swap:TSwapProc); overload;
    procedure Sort(const Ascending:Boolean=True); overload;
    function SortedFind(const Value: Int64; out Exists:Boolean):TNativeInteger;
    function Stats:TInt64Stats;
    function StdDeviation(const Mean:TFloat):TFloat;
    function Sum:TFloat;
    function SumOfSquares: TFloat;
    procedure Swap(const A,B:TInteger); inline;
    function Variance(const Mean:TFloat):TFloat;
  end;

  TSingleArrayHelper=record helper for TSingleArray
  private
    const
      SortThreshold=16;

    function Distribution(const Mean,StdDeviation:Single; const Exponent:Integer):Single;
    function GuessOrder:TDataOrder;
  public
    function Append(const Value:Single):TInteger; overload; inline;
    procedure Append(const Value:TSingleArray); overload;
    function Copy(const AIndex,ACount:TInteger):TSingleArray; overload; inline;
    function Copy:TSingleArray; overload; inline;
    function Copy(const Missing:TBooleanArray):TSingleArray; overload;
    function Count:TInteger; inline;
    function Correlation(const Y: TSingleArray; const XMean,YMean: Single): Single;
    function CoVariance(const Y: TSingleArray; const XMean,YMean: Single):Single;
    procedure Delete(const Index:TInteger; const ACount:TInteger=1); {$IFDEF DELETEARRAY}inline;{$ENDIF}
    procedure Empty; inline;
    function Equals(const Value:TSingleArray):Boolean;
    function ExistsBefore(const AIndex:TInteger):Boolean;
    procedure Initialize(const Value:Single=0);
    procedure Insert(const Index:TInteger; const Value:Single);
    function Map(out Median,Mode:Single):TSingleMap; overload;
    function Maximum:Single;
    function Mean:Single;
    function Minimum:Single;
    procedure Normalize(const Mean:Single);
    procedure Resize(const Count:TInteger); inline;
    procedure Reverse; inline;
    procedure Sort(const FromIndex,ToIndex:TInteger; const Ascending:Boolean=True); overload;
    procedure Sort(const Ascending:Boolean; const Swap:TSwapProc); overload;
    procedure Sort(const Ascending: Boolean=True); overload; inline;
    function SortedFind(const Value: Single; out Exists:Boolean):TNativeInteger;
    function Stats:TSingleStats;
    function StdDeviation(const Mean:Single):Single;
    function Sum:Single;
    function SumOfSquares: Single;
    procedure Swap(const A,B:TInteger); inline;
    function Variance(const Mean:Single):Single;
  end;

  TDoubleArrayHelper=record helper for TDoubleArray
  private
    const
      SortThreshold=16;

    function Distribution(const Mean,StdDeviation:Double; const Exponent:Integer):Double;
    function GuessOrder:TDataOrder;
  public
    function Append(const Value:Double):TInteger; overload; inline;
    procedure Append(const Value:TDoubleArray); overload;
    function Copy(const AIndex,ACount:TInteger):TDoubleArray; overload; inline;
    function Copy:TDoubleArray; overload; inline;
    function Copy(const Missing:TBooleanArray):TDoubleArray; overload;
    function Count:TInteger; inline;
    function Correlation(const Y: TDoubleArray; const XMean,YMean: Double): Double;
    function CoVariance(const Y: TDoubleArray; const XMean,YMean: Double):Double;
    procedure Delete(const Index:TInteger; const ACount:TInteger=1); {$IFDEF DELETEARRAY}inline;{$ENDIF}
    procedure Empty; inline;
    function Equals(const Value:TDoubleArray):Boolean;
    function ExistsBefore(const AIndex:TInteger):Boolean;
    procedure Initialize(const Value:Double=0);
    procedure Insert(const Index:TInteger; const Value:Double);
    function Map(out Median,Mode:Double):TDoubleMap; overload;
    function Maximum:Double;
    function Mean:Double;
    function Minimum:Double;
    procedure Normalize(const Mean:Double);
    procedure Resize(const Count:TInteger); inline;
    procedure Reverse; inline;
    procedure Sort(const FromIndex,ToIndex:TInteger; const Ascending:Boolean=True); overload;
    procedure Sort(const Ascending:Boolean; const Swap:TSwapProc); overload;
    procedure Sort(const Ascending: Boolean=True); overload; inline;
    function SortedFind(const Value: Double; out Exists:Boolean):TNativeInteger;
    function Stats:TDoubleStats;
    function StdDeviation(const Mean:Double):Double;
    function Sum:Double;
    function SumOfSquares: Double;
    procedure Swap(const A,B:TInteger); inline;
    function Variance(const Mean:Double):Double;
  end;

  {$IFDEF CPUX86}
  TExtendedArrayHelper=record helper for TExtendedArray
  private
    function Distribution(const Mean,StdDeviation:Extended; const Exponent:Integer):Extended;
    function GuessOrder:TDataOrder;
  public
    function Append(const Value:Extended):TInteger; overload; inline;
    procedure Append(const Value:TExtendedArray); overload;
    function Copy(const AIndex,ACount:TInteger):TExtendedArray; overload; inline;
    function Copy:TExtendedArray; overload; inline;
    function Copy(const Missing:TBooleanArray):TExtendedArray; overload;
    function Count:TInteger; inline;
    function Correlation(const Y: TExtendedArray; const XMean,YMean: Extended): Extended;
    function CoVariance(const Y: TExtendedArray; const XMean,YMean: Extended):Extended;
    procedure Delete(const Index:TInteger; const ACount:TInteger=1); {$IFDEF DELETEARRAY}inline;{$ENDIF}
    procedure Empty; inline;
    function ExistsBefore(const AIndex:TInteger):Boolean;
    procedure Initialize(const Value:Extended=0);
    procedure Insert(const Index:TInteger; const Value:Extended);
    function Map(out Median,Mode:Extended):TExtendedMap; overload;
    function Maximum:Extended;
    function Mean:Extended;
    function Minimum:Extended;
    procedure Normalize(const Mean:Extended);
    procedure Resize(const Count:TInteger); inline;
    procedure Reverse; inline;
    procedure Sort(const Ascending:Boolean; const Swap:TSwapProc); overload;
    procedure Sort(const Ascending: Boolean=True); overload; inline;
    function SortedFind(const Value: Extended; out Exists:Boolean):TNativeInteger;
    function Stats:TExtendedStats;
    function StdDeviation(const Mean:Extended):Extended;
    function Sum:Extended;
    function SumOfSquares: Extended;
    procedure Swap(const A,B:TInteger); inline;
    function Variance(const Mean:Extended):Extended;
  end;
  {$ENDIF}

  TSquareGridHelper=record helper for TSquareGrid
  public
    function Count:TInteger; inline;
    procedure Initialize(const Value:Double=0);
    procedure MakeIdentity;
    procedure Resize(const Count:TInteger); inline;
  end;

  TFloatGridHelper=record helper for TFloatGrid
  public
    function Count:TInteger; inline;
    function CoVarianceMatrix(const Means:TDoubleArray):TSquareGrid;
    procedure Initialize(const Value:Double=0);
    procedure InitializeRandom;
    function Means:TDoubleArray;
    procedure Normalize(const Means:TDoubleArray);
    procedure Resize(const Cols,Rows:TInteger); inline;
    function ScatterMatrix(const CoVarianceMatrix:TSquareGrid):TSquareGrid;
  end;

implementation
