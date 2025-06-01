{*********************************************}
{  TeeBI Software Library                     }
{  Base Array helper classes                  }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Arrays;

// Supports from RAD ide XE4 and up (record helpers are necessary)

{$IF Declared(CompilerVersion)}
 {$IF CompilerVersion>=25}
  {$EXCESSPRECISION OFF} // x64 "to double" speedup (less precission)
 {$ENDIF}
 {$IF CompilerVersion>=28}
  {$DEFINE D21}
 {$ENDIF}
{$ENDIF}

{$POINTERMATH ON}

interface

uses
  {System.}SysUtils, {System.}DateUtils;

{$IF Declared(CompilerVersion)}
 {$IF CompilerVersion>28}
  {.$DEFINE DELETEARRAY} // <--- Very Slow !!!
  {.$DEFINE INSERTTEXTARRAY}
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
    procedure Resize(const ACount:TInteger); inline;
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
    procedure Resize(const ACount:TInteger); inline;
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
    procedure Resize(const ACount:TInteger); inline;
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
    procedure Resize(const ACount:TInteger); inline;
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
    procedure Resize(const ACount:TInteger); inline;
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
    procedure Resize(const ACount:TInteger); inline;
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
    procedure Resize(const ACount:TInteger); inline;
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
    procedure Resize(const ACount:TInteger); inline;
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
    procedure Resize(const ACount:TInteger); inline;
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

uses
  {System.}Math, BI.Languages.English;

{$IF Declared(CompilerVersion)}
 {$IF CompilerVersion<27}
  // XE4 / XE5 problem:
  // Class helper methods cannot be used as callback procedure params.
  // See TXXArrayHelper.Sort method (call to Swap)
  {$DEFINE XE5BUG}
 {$ENDIF}
{$ENDIF}

procedure DifferentArrayLength;
begin
  raise EBIException.Create(BIMsg_DifferentArrayLength);
end;

{ TArrayReverse<T> }

class procedure TArrayReverse<T>.Reverse(var Items: array of T);
var i,
    i2,
    L,
    Mid : TInteger;
    tmp : T;
begin
  L:=Length(Items);

  if L>1 then
  begin
    Mid:=L shr 1;

    Dec(L);

    for i:=0 to Mid-1 do
    begin
      tmp:=Items[i];

      i2:=L-i;
      Items[i]:=Items[i2];
      Items[i2]:=tmp;
    end;
  end;
end;

{ TSingleArrayHelper }

function TSingleArrayHelper.Count: TInteger;
begin
  result:=Length(Self);
end;

procedure TSingleArrayHelper.Resize(const ACount: TInteger);
begin
  SetLength(Self,ACount);
end;

procedure TSingleArrayHelper.Reverse;
begin
  TArrayReverse<Single>.Reverse(Self);
end;

procedure TSingleArrayHelper.Empty;
begin
  Resize(0);
end;

function TSingleArrayHelper.Equals(const Value: TSingleArray): Boolean;
var t : TInteger;
begin
  result:=Count=Value.Count;

  if result then
     for t:=0 to Count-1 do
         if Self[t]<>Value[t] then // No Epsilon used (assuming identical binary floats)
            Exit(False);
end;

procedure TSingleArrayHelper.Swap(const A, B: TInteger);
var tmp : Single;
begin
  tmp:=Self[A];
  Self[A]:=Self[B];
  Self[B]:=tmp;
end;

function TSingleArrayHelper.Copy(const AIndex,ACount:TInteger): TSingleArray;
begin
  result:=System.Copy(Self,AIndex,ACount);
end;

function TSingleArrayHelper.Copy: TSingleArray;
begin
  result:=Copy(0,Count);
end;

function TSingleArrayHelper.Append(const Value: Single):TInteger;
begin
  result:=Count;
  Resize(result+1);
  Self[result]:=Value;
end;

function TSingleArrayHelper.Map(out Median,Mode: Single): TSingleMap;
var tmp : TSingleArray;
    tmpMax : TInteger;
begin
  result:=TSingleMap.Create;

  if Count>0 then
  begin
    result.FSorted:=GuessOrder;

    if result.Sorted<>Ascending then
    begin
      tmp:=Copy;
      tmp.Sort;
    end
    else
      tmp:=Self;

    Median:=tmp[Count div 2];

    result.CreateMap(tmp);
  end;

  tmpMax:=result.Map.IndexOfMax;

  if tmpMax=-1 then
     Mode:=0
  else
     Mode:=result.Items[tmpMax];
end;

function TSingleArrayHelper.Maximum: Single;
var n : TInteger;
    t : TLoopInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
  begin
    result:=Self[0];

    for t:=1 to n-1 do
        if Self[t]>result then
           result:=Self[t];
  end;
end;

function TSingleArrayHelper.Mean: Single;
var n : TInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
     result:=Sum/n;
end;

function TSingleArrayHelper.Minimum: Single;
var n : TInteger;
    t : TLoopInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
  begin
    result:=Self[0];

    for t:=1 to n-1 do
        if Self[t]<result then
           result:=Self[t];
  end;
end;

procedure TSingleArrayHelper.Normalize(const Mean: Single);
var t : TLoopInteger;
begin
  for t:=0 to Count-1 do
      Self[t]:=Self[t]-Mean;
end;

procedure TSingleArrayHelper.Initialize(const Value:Single);
var t : TLoopInteger;
begin
  for t:=0 to Count-1 do
      Self[t]:=Value;
end;

procedure TSingleArrayHelper.Sort(const Ascending:Boolean; const Swap:TSwapProc);

const
  SortThreshold = 16;

  procedure SingleInsertionSort_WithSwap(L, R: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var i, j, currentIndexToInsert: TInteger;
  begin
    if IsAscending then
    begin
      for i := L + 1 to R do
      begin
        currentIndexToInsert := i;
        j := i - 1;
        while (j >= L) and (Self[j] > Self[currentIndexToInsert]) do
        begin
          CurrentSwap(j, currentIndexToInsert);
          currentIndexToInsert := j;
          Dec(j);
        end;
      end;
    end
    else // Descending
    begin
      for i := L + 1 to R do
      begin
        currentIndexToInsert := i;
        j := i - 1;
        while (j >= L) and (Self[j] < Self[currentIndexToInsert]) do
        begin
          CurrentSwap(j, currentIndexToInsert);
          currentIndexToInsert := j;
          Dec(j);
        end;
      end;
    end;
  end;

  procedure SingleHeapify_WithSwap(CurrentIndex, CurrentCount, LBoundary: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var
    RootToHeapify, LeftChild, RightChild: TInteger;
  begin
    RootToHeapify := CurrentIndex;
    LeftChild := 2 * CurrentIndex + 1;
    RightChild := 2 * CurrentIndex + 2;

    if IsAscending then
    begin
      if (LeftChild < CurrentCount) and (Self[LBoundary + LeftChild] > Self[LBoundary + RootToHeapify]) then
        RootToHeapify := LeftChild;
      if (RightChild < CurrentCount) and (Self[LBoundary + RightChild] > Self[LBoundary + RootToHeapify]) then
        RootToHeapify := RightChild;
    end
    else // Descending
    begin
      if (LeftChild < CurrentCount) and (Self[LBoundary + LeftChild] < Self[LBoundary + RootToHeapify]) then
        RootToHeapify := LeftChild;
      if (RightChild < CurrentCount) and (Self[LBoundary + RightChild] < Self[LBoundary + RootToHeapify]) then
        RootToHeapify := RightChild;
    end;

    if RootToHeapify <> CurrentIndex then
    begin
      CurrentSwap(LBoundary + CurrentIndex, LBoundary + RootToHeapify);
      SingleHeapify_WithSwap(RootToHeapify, CurrentCount, LBoundary, IsAscending, CurrentSwap);
    end;
  end;

  procedure SingleHeapSort_WithSwap(L, R: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var
    i, HeapSize: TInteger;
  begin
    HeapSize := R - L + 1;
    if HeapSize < 2 then Exit;

    for i := (HeapSize div 2) - 1 downto 0 do
      SingleHeapify_WithSwap(i, HeapSize, L, IsAscending, CurrentSwap);

    for i := HeapSize - 1 downto 1 do
    begin
      CurrentSwap(L, L + i);
      SingleHeapify_WithSwap(0, i, L, IsAscending, CurrentSwap);
    end;
  end;

  procedure PrivateSort(const l,r:TInteger; Depth: Integer);
  var
      pivotValue : Single;
      i, j, pivotIndex : TInteger;
      N, MaxDepthAllowed : TInteger;
  begin
    N := r - l + 1;
    if N <= 1 then Exit;

    if N > 0 then
      MaxDepthAllowed := 2 * Trunc(System.Math.Log2(N))
    else
      MaxDepthAllowed := 0;

    if Depth > MaxDepthAllowed then
    begin
      SingleHeapSort_WithSwap(l, r, Ascending, Swap);
      Exit;
    end;

    if N <= SortThreshold then
    begin
      SingleInsertionSort_WithSwap(l, r, Ascending, Swap);
      Exit;
    end;

    i := l;
    j := r;
    pivotIndex := (l + j) shr 1;
    pivotValue := Self[pivotIndex];

    while i <= j do
    begin
      if Ascending then
      begin
        while Self[i] < pivotValue do Inc(i);
        while Self[j] > pivotValue do Dec(j);
      end
      else // Descending
      begin
        while Self[i] > pivotValue do Inc(i);
        while Self[j] < pivotValue do Dec(j);
      end;

      if i <= j then
      begin
        if i <> j then
          Swap(i, j);
        Inc(i);
        Dec(j);
      end;
    end;

    if l < j then
       PrivateSort(l, j, Depth + 1);

    if i < r then
       PrivateSort(i, r, Depth + 1);
  end;

begin
  if Count>1 then
     PrivateSort(0,Count-1, 0);
end;

procedure TSingleArrayHelper.Sort(const FromIndex, ToIndex: TInteger;
  const Ascending: Boolean);

  procedure PrivateSort(const l,r:TInteger);
  var i : TInteger;
      j : TInteger;
      x : TInteger;
      tmp : Single;
  begin
    if r-l<=SortThreshold then
    begin
      // Insertion Sort

      if Ascending then
         for i:=L+1 to R do
         begin
           tmp:=Self[i];
           j:=Pred(i);

           while (j>=L) and (Self[j]>tmp) do
           begin
             Self[j+1]:=Self[j];
             Dec(j);
           end;

           Self[j+1]:=tmp;
         end
      else
         for i:=L+1 to R do
         begin
           tmp:=Self[i];
           j:=Pred(i);

           while (j>=L) and (Self[j]<tmp) do
           begin
             Self[j+1]:=Self[j];
             Dec(j);
           end;

           Self[j+1]:=tmp;
         end;

      Exit;
    end;

    i:=l;
    j:=r;
    x:=(i+j) shr 1;

    while i<j do
    begin
      tmp:=Self[x];

      if Ascending then
      begin
        while Self[i]<tmp do inc(i);
        while tmp<Self[j] do dec(j);
      end
      else
      begin
        while Self[i]>tmp do inc(i);
        while tmp>Self[j] do dec(j);
      end;

      if i<j then
      begin
        tmp:=Self[i];
        Self[i]:=Self[j];
        Self[j]:=tmp;

        if i=x then
           x:=j
        else
        if j=x then
           x:=i;
      end;

      if i<=j then
      begin
        inc(i);
        dec(j)
      end;
    end;

    // Potential parallelization
    if l<j then
       PrivateSort(l,j);

    if i<r then
       PrivateSort(i,r);
  end;

begin
  if ToIndex-FromIndex>0 then
     PrivateSort(FromIndex,ToIndex);
end;

procedure TSingleArrayHelper.Sort(const Ascending: Boolean=True);
begin
  if Count>1 then
     Sort(0,Count-1,Ascending);
end;

function TSingleArrayHelper.Distribution(const Mean,StdDeviation:Single; const Exponent:Integer):Single;

  function SumOfDifferences:Single;
  var t : TLoopInteger;
  begin
    result:=0;

    for t:=0 to Count-1 do
        result:=result+IntPower((Self[t]-Mean),Exponent);
  end;

begin
  result:=(Count-1)*IntPower(StdDeviation,Exponent);

  if result<>0 then
     result:=SumOfDifferences/result;
end;

function TSingleArrayHelper.SortedFind(const Value: Single; out Exists:Boolean): TNativeInteger;
var
  L, H : TInteger;
  tmp : Single; // opt
begin
  L := 0;
  H := High(Self);

  while L <= H do
  begin
    result := (L + H) shr 1;

    tmp:=Self[result];

    if tmp<Value then
       L := Succ(result)
    else
    if tmp=Value then
    begin
      Exists:=True;
      Exit;
    end
    else
       H := Pred(result);
  end;

  result := L;
  Exists := False;
end;

function TSingleArrayHelper.Stats: TSingleStats;
var t : TLoopInteger;
    tmpSum : Single;
begin
  result:=TSingleStats.Create;

  if Count>0 then
  begin
    result.Min:=Self[0];
    result.Max:=Self[0];

    tmpSum:=Self[0];

    for t:=1 to Count-1 do
    begin
      if Self[t]<result.Min then
         result.Min:=Self[t]
      else
      if Self[t]>result.Max then
         result.Max:=Self[t];

      tmpSum:=tmpSum+Self[t];
    end;

    result.Range:=result.Max-result.Min;
    result.Mean:=tmpSum/Count;
    result.Sum:=tmpSum;

    result.Variance:=Variance(result.Mean);
    result.StdDeviation:=Sqrt(result.Variance);

    result.Skewness:=Distribution(result.Mean,result.StdDeviation, 3);
    result.Peakedness:=Distribution(result.Mean,result.StdDeviation, 4);
  end;
end;

function TSingleArrayHelper.StdDeviation(const Mean: Single): Single;
begin
  result:=Sqrt(Variance(Mean));
end;

function TSingleArrayHelper.Sum: Single;
var t : TLoopInteger;
begin
  result:=0;

  for t:=0 to Count-1 do
      result:=result+Self[t];
end;

function TSingleArrayHelper.SumOfSquares: Single;
var t : TLoopInteger;
begin
  result:=0;

  for t:=0 to Count-1 do
      result:=result+Sqr(Self[t]);
end;

function TSingleArrayHelper.Variance(const Mean: Single): Single;
var t : TLoopInteger;
    n : TInteger;
begin
  result:=0;

  n:=Count;

  if n>1 then
  begin
    for t:=0 to n-1 do
        result:=result+Sqr(Self[t]-Mean);

    result:=result/Pred(n);
  end;
end;

function TSingleArrayHelper.CoVariance(const Y: TSingleArray; const XMean,YMean: Single): Single;
var t : TLoopInteger;
    n : TInteger;
begin
  result:=0;

  n:=Count;

  if n>1 then
  begin
    if Y.Count<>n then
       DifferentArrayLength;

    for t:=0 to n-1 do
        result:=result+( (Self[t]-XMean)*(Y[t]-YMean) );

    result:=result/Pred(n);
  end;
end;

procedure TSingleArrayHelper.Delete(const Index:TInteger; const ACount: TInteger=1);
{$IFNDEF DELETEARRAY}
var tmp : TInteger;
{$ENDIF}
begin
  {$IFDEF DELETEARRAY}
  System.Delete(Self,Index,ACount);
  {$ELSE}
  tmp:=Count-ACount;

  if tmp-Index>0 then
     System.Move(Self[Index+ACount],Self[Index],SizeOf(Single)*(tmp-Index));

  Resize(tmp);
  {$ENDIF}
end;

function TSingleArrayHelper.ExistsBefore(const AIndex: TInteger): Boolean;
var t : TLoopInteger;
begin
  for t:=0 to AIndex-1 do
      if Self[t]=Self[AIndex] then
         Exit(True);

  result:=False;
end;

function TSingleArrayHelper.GuessOrder:TDataOrder;

  function GuessStart(out Order:TDataOrder):TInteger;
  begin
    Order:=None;
    result:=1;

    repeat
      if Self[result]>Self[result-1] then
      begin
        Order:=Ascending;
        break;
      end
      else
      if Self[result]<Self[result-1] then
      begin
        Order:=Descending;
        break;
      end
      else
         Inc(result);

    until result>=Count;
  end;

var n,
    c,
    t : TInteger;
begin
  result:=None;

  c:=Count;

  if c=1 then
     result:=Ascending
  else
  if c>1 then
  begin
    n:=GuessStart(result);

    t:=n+1;

    if result=Ascending then
    begin
      while t<c do
      begin
        if Self[t]<Self[t-1] then
           Exit(None);

        Inc(t);
      end;
    end
    else
    if result=Descending then
      while t<c do
      begin
        if Self[t]>Self[t-1] then
           Exit(None);

        Inc(t);
      end;
  end;
end;

procedure TSingleArrayHelper.Insert(const Index: TInteger; const Value: Single);
var tmp : TInteger;
begin
  tmp:=Count;
  Resize(tmp+1);

  if Index<tmp then
     System.Move(Self[Index],Self[Index+1],(tmp-Index)*SizeOf(Single));

  Self[Index]:=Value;
end;

procedure TSingleArrayHelper.Append(const Value: TSingleArray);
var tmp : TInteger;
begin
  if Value<>nil then
  begin
    tmp:=Count;
    Resize(tmp+Value.Count);
    Move(Value[Low(Value)],Self[tmp],Value.Count*SizeOf(Single));
  end;
end;

function TSingleArrayHelper.Copy(const Missing: TBooleanArray): TSingleArray;
var t : TLoopInteger;
    MissingCount : TInteger;
begin
  result:=nil;

  MissingCount:=Missing.Count;

  for t:=0 to Count-1 do
      if (MissingCount<=t) or (not Missing[t]) then
         result.Append(Self[t]);
end;

function TSingleArrayHelper.Correlation(const Y: TSingleArray; const XMean,YMean: Single): Single;
var tmp : Single;
begin
  tmp:=StdDeviation(XMean)*Y.StdDeviation(YMean);

  if tmp=0 then
     result:=0
  else
     result:=CoVariance(Y,XMean,YMean)/tmp;
end;

{ TDoubleArrayHelper }

function TDoubleArrayHelper.Count: TInteger;
begin
  result:=Length(Self);
end;

function TDoubleArrayHelper.Copy(const AIndex,ACount:TInteger): TDoubleArray;
begin
  result:=System.Copy(Self,AIndex,ACount);
end;

function TDoubleArrayHelper.Copy: TDoubleArray;
begin
  result:=Copy(0,Count);
end;

procedure TDoubleArrayHelper.Resize(const ACount: TInteger);
begin
  SetLength(Self,ACount);
end;

procedure TDoubleArrayHelper.Reverse;
begin
  TArrayReverse<Double>.Reverse(Self);
end;

function TDoubleArrayHelper.Append(const Value: Double):TInteger;
begin
  result:=Count;
  Resize(result+1);
  Self[result]:=Value;
end;

procedure TDoubleArrayHelper.Empty;
begin
  Resize(0);
end;

function TDoubleArrayHelper.Equals(const Value: TDoubleArray): Boolean;
var t : TInteger;
begin
  result:=Count=Value.Count;

  if result then
     for t:=0 to Count-1 do
         if Self[t]<>Value[t] then // No Epsilon used (assuming identical binary floats)
            Exit(False);
end;

procedure TDoubleArrayHelper.Swap(const A, B: TInteger);
var tmp : Double;
begin
  tmp:=Self[A];
  Self[A]:=Self[B];
  Self[B]:=tmp;
end;

function TDoubleArrayHelper.Map(out Median,Mode: Double): TDoubleMap;
var tmp : TDoubleArray;
    tmpMax : TInteger;
begin
  result:=TDoubleMap.Create;

  if Count>0 then
  begin
    result.FSorted:=GuessOrder;

    if result.Sorted<>Ascending then
    begin
      tmp:=Copy;
      tmp.Sort;
    end
    else
      tmp:=Self;

    Median:=tmp[Count div 2];

    result.CreateMap(tmp);
  end;

  tmpMax:=result.Map.IndexOfMax;

  if tmpMax=-1 then
     Mode:=0
  else
     Mode:=result.Items[tmpMax];
end;

function TDoubleArrayHelper.Maximum: Double;
var n : TInteger;
    t : TLoopInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
  begin
    result:=Self[0];

    for t:=1 to n-1 do
        if Self[t]>result then
           result:=Self[t];
  end;
end;

function TDoubleArrayHelper.Mean: Double;
var n : TInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
     result:=Sum/n;
end;

function TDoubleArrayHelper.Minimum: Double;
var n : TInteger;
    t : TLoopInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
  begin
    result:=Self[0];

    for t:=1 to n-1 do
        if Self[t]<result then
           result:=Self[t];
  end;
end;

procedure TDoubleArrayHelper.Normalize(const Mean: Double);
var t : TLoopInteger;
begin
  for t:=0 to Count-1 do
      Self[t]:=Self[t]-Mean;
end;

procedure TDoubleArrayHelper.Initialize(const Value:Double);
var t : TLoopInteger;
begin
  for t:=0 to Count-1 do
      Self[t]:=Value;
end;

procedure TDoubleArrayHelper.Sort(const Ascending:Boolean; const Swap:TSwapProc);

const
  SortThreshold = 16;

  procedure DoubleInsertionSort_WithSwap(L, R: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var i, j, currentIndexToInsert: TInteger;
  begin
    if IsAscending then
    begin
      for i := L + 1 to R do
      begin
        currentIndexToInsert := i;
        j := i - 1;
        while (j >= L) and (Self[j] > Self[currentIndexToInsert]) do
        begin
          CurrentSwap(j, currentIndexToInsert);
          currentIndexToInsert := j;
          Dec(j);
        end;
      end;
    end
    else // Descending
    begin
      for i := L + 1 to R do
      begin
        currentIndexToInsert := i;
        j := i - 1;
        while (j >= L) and (Self[j] < Self[currentIndexToInsert]) do
        begin
          CurrentSwap(j, currentIndexToInsert);
          currentIndexToInsert := j;
          Dec(j);
        end;
      end;
    end;
  end;

  procedure DoubleHeapify_WithSwap(CurrentIndex, CurrentCount, LBoundary: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var
    RootToHeapify, LeftChild, RightChild: TInteger;
  begin
    RootToHeapify := CurrentIndex;
    LeftChild := 2 * CurrentIndex + 1;
    RightChild := 2 * CurrentIndex + 2;

    if IsAscending then
    begin
      if (LeftChild < CurrentCount) and (Self[LBoundary + LeftChild] > Self[LBoundary + RootToHeapify]) then
        RootToHeapify := LeftChild;
      if (RightChild < CurrentCount) and (Self[LBoundary + RightChild] > Self[LBoundary + RootToHeapify]) then
        RootToHeapify := RightChild;
    end
    else // Descending
    begin
      if (LeftChild < CurrentCount) and (Self[LBoundary + LeftChild] < Self[LBoundary + RootToHeapify]) then
        RootToHeapify := LeftChild;
      if (RightChild < CurrentCount) and (Self[LBoundary + RightChild] < Self[LBoundary + RootToHeapify]) then
        RootToHeapify := RightChild;
    end;

    if RootToHeapify <> CurrentIndex then
    begin
      CurrentSwap(LBoundary + CurrentIndex, LBoundary + RootToHeapify);
      DoubleHeapify_WithSwap(RootToHeapify, CurrentCount, LBoundary, IsAscending, CurrentSwap);
    end;
  end;

  procedure DoubleHeapSort_WithSwap(L, R: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var
    i, HeapSize: TInteger;
  begin
    HeapSize := R - L + 1;
    if HeapSize < 2 then Exit;

    for i := (HeapSize div 2) - 1 downto 0 do
      DoubleHeapify_WithSwap(i, HeapSize, L, IsAscending, CurrentSwap);

    for i := HeapSize - 1 downto 1 do
    begin
      CurrentSwap(L, L + i);
      DoubleHeapify_WithSwap(0, i, L, IsAscending, CurrentSwap);
    end;
  end;

  procedure PrivateSort(const l,r:TInteger; Depth: Integer);
  var
      pivotValue : Double;
      i, j, pivotIndex : TInteger;
      N, MaxDepthAllowed : TInteger;
  begin
    N := r - l + 1;
    if N <= 1 then Exit;

    if N > 0 then
      MaxDepthAllowed := 2 * Trunc(System.Math.Log2(N))
    else
      MaxDepthAllowed := 0;

    if Depth > MaxDepthAllowed then
    begin
      DoubleHeapSort_WithSwap(l, r, Ascending, Swap);
      Exit;
    end;

    if N <= SortThreshold then
    begin
      DoubleInsertionSort_WithSwap(l, r, Ascending, Swap);
      Exit;
    end;

    i := l;
    j := r;
    pivotIndex := (l + j) shr 1;
    pivotValue := Self[pivotIndex];

    while i <= j do
    begin
      if Ascending then
      begin
        while Self[i] < pivotValue do Inc(i);
        while Self[j] > pivotValue do Dec(j);
      end
      else // Descending
      begin
        while Self[i] > pivotValue do Inc(i);
        while Self[j] < pivotValue do Dec(j);
      end;

      if i <= j then
      begin
        if i <> j then
          Swap(i, j);
        Inc(i);
        Dec(j);
      end;
    end;

    if l < j then
       PrivateSort(l, j, Depth + 1);

    if i < r then
       PrivateSort(i, r, Depth + 1);
  end;

begin
  if Count>1 then
     PrivateSort(0,Count-1, 0);
end;


procedure TDoubleArrayHelper.Sort(const FromIndex, ToIndex: TInteger;
  const Ascending: Boolean);

  procedure PrivateSort(const l,r:TInteger);
  var i : TInteger;
      j : TInteger;
      x : TInteger;
      tmp : Double;
  begin
    if r-l<=SortThreshold then
    begin
      // Insertion Sort

      if Ascending then
         for i:=L+1 to R do
         begin
           tmp:=Self[i];
           j:=Pred(i);

           while (j>=L) and (Self[j]>tmp) do
           begin
             Self[j+1]:=Self[j];
             Dec(j);
           end;

           Self[j+1]:=tmp;
         end
      else
         for i:=L+1 to R do
         begin
           tmp:=Self[i];
           j:=Pred(i);

           while (j>=L) and (Self[j]<tmp) do
           begin
             Self[j+1]:=Self[j];
             Dec(j);
           end;

           Self[j+1]:=tmp;
         end;

      Exit;
    end;

    i:=l;
    j:=r;
    x:=(i+j) shr 1;

    while i<j do
    begin
      tmp:=Self[x];

      if Ascending then
      begin
        while Self[i]<tmp do inc(i);
        while tmp<Self[j] do dec(j);
      end
      else
      begin
        while Self[i]>tmp do inc(i);
        while tmp>Self[j] do dec(j);
      end;

      if i<j then
      begin
        tmp:=Self[i];
        Self[i]:=Self[j];
        Self[j]:=tmp;

        if i=x then
           x:=j
        else
        if j=x then
           x:=i;
      end;

      if i<=j then
      begin
        inc(i);
        dec(j)
      end;
    end;

    // Potential parallelization
    if l<j then
       PrivateSort(l,j);

    if i<r then
       PrivateSort(i,r);
  end;

begin
  if ToIndex-FromIndex>0 then
     PrivateSort(FromIndex,ToIndex);
end;

procedure TDoubleArrayHelper.Sort(const Ascending: Boolean=True);
begin
  if Count>1 then
     Sort(0,Count-1,Ascending);
end;

function TDoubleArrayHelper.Distribution(const Mean,StdDeviation:Double; const Exponent:Integer):Double;

  function SumOfDifferences:Double;
  var t : TLoopInteger;
  begin
    result:=0;

    for t:=0 to Count-1 do
        result:=result+IntPower((Self[t]-Mean),Exponent);
  end;

begin
  result:=(Count-1)*IntPower(StdDeviation,Exponent);

  if result<>0 then
     result:=SumOfDifferences/result;
end;

function TDoubleArrayHelper.SortedFind(const Value: Double; out Exists:Boolean):TNativeInteger;
var
  L, H : TInteger;
  // NO BETTER -> tmp : Double; opt
begin
  L := 0;
  H := High(Self);

  while L <= H do
  begin
    result := (L + H) shr 1;

    if Self[result]<Value then
       L := Succ(result)
    else
    if Self[result]=Value then
    begin
      Exists:=True;
      Exit;
    end
    else
       H := Pred(result);
  end;

  result:=L;
  Exists:=False;
end;

function TDoubleArrayHelper.Stats: TDoubleStats;
var t : TLoopInteger;
    tmpSum : Double;
begin
  result:=TDoubleStats.Create;

  if Count>0 then
  begin
    result.Min:=Self[0];
    result.Max:=Self[0];

    tmpSum:=Self[0];

    for t:=1 to Count-1 do
    begin
      if Self[t]<result.Min then
         result.Min:=Self[t]
      else
      if Self[t]>result.Max then
         result.Max:=Self[t];

      tmpSum:=tmpSum+Self[t];
    end;

    result.Range:=result.Max-result.Min;
    result.Mean:=tmpSum/Count;
    result.Sum:=tmpSum;

    result.Variance:=Variance(result.Mean);
    result.StdDeviation:=Sqrt(result.Variance);

    result.Skewness:=Distribution(result.Mean,result.StdDeviation, 3);
    result.Peakedness:=Distribution(result.Mean,result.StdDeviation, 4);
  end;
end;

function TDoubleArrayHelper.StdDeviation(const Mean: Double): Double;
begin
  result:=Sqrt(Variance(Mean));
end;

function TDoubleArrayHelper.Sum: Double;
var t : TLoopInteger;
begin
  result:=0;

  for t:=0 to Count-1 do
      result:=result+Self[t];
end;

function TDoubleArrayHelper.SumOfSquares: Double;
var t : TLoopInteger;
begin
  result:=0;

  for t:=0 to Count-1 do
      result:=result+Sqr(Self[t]);
end;

function TDoubleArrayHelper.Variance(const Mean: Double): Double;
var t : TLoopInteger;
    n : TInteger;
begin
  result:=0;

  n:=Count;

  if n>1 then
  begin
    for t:=0 to n-1 do
        result:=result+Sqr(Self[t]-Mean);

    result:=result/Pred(n);
  end;
end;

function TDoubleArrayHelper.CoVariance(const Y: TDoubleArray; const XMean,YMean: Double): Double;
var t : TLoopInteger;
    n : TInteger;
begin
  result:=0;

  n:=Count;

  if n>1 then
  begin
    if Y.Count<>n then
       DifferentArrayLength;

    for t:=0 to n-1 do
        result:=result+( (Self[t]-XMean)*(Y[t]-YMean) );

    result:=result/Pred(n);
  end;
end;

procedure TDoubleArrayHelper.Delete(const Index:TInteger; const ACount: TInteger=1);
{$IFNDEF DELETEARRAY}
var tmp : TInteger;
{$ENDIF}
begin
  {$IFDEF DELETEARRAY}
  System.Delete(Self,Index,ACount);
  {$ELSE}
  tmp:=Count-ACount;

  if tmp-Index>0 then
     System.Move(Self[Index+ACount],Self[Index],SizeOf(Double)*(tmp-Index));

  Resize(tmp);
  {$ENDIF}
end;

function TDoubleArrayHelper.ExistsBefore(const AIndex: TInteger): Boolean;
var t : TLoopInteger;
begin
  for t:=0 to AIndex-1 do
      if Self[t]=Self[AIndex] then
         Exit(True);

  result:=False;
end;

function TDoubleArrayHelper.GuessOrder:TDataOrder;

  function GuessStart(out Order:TDataOrder):TInteger;
  begin
    Order:=None;
    result:=1;

    repeat
      if Self[result]>Self[result-1] then
      begin
        Order:=Ascending;
        break;
      end
      else
      if Self[result]<Self[result-1] then
      begin
        Order:=Descending;
        break;
      end
      else
         Inc(result);

    until result>=Count;
  end;

var n,
    c,
    t : TInteger;
begin
  result:=None;

  c:=Count;

  if c=1 then
     result:=Ascending
  else
  if c>1 then
  begin
    n:=GuessStart(result);

    t:=n+1;

    if result=Ascending then
    begin
      while t<c do
      begin
        if Self[t]<Self[t-1] then
           Exit(None);

        Inc(t);
      end;
    end
    else
    if result=Descending then
      while t<c do
      begin
        if Self[t]>Self[t-1] then
           Exit(None);

        Inc(t);
      end;
  end;
end;

procedure TDoubleArrayHelper.Insert(const Index: TInteger; const Value: Double);
var tmp : TInteger;
begin
  tmp:=Count;
  Resize(tmp+1);

  if Index<tmp then
     System.Move(Self[Index],Self[Index+1],(tmp-Index)*SizeOf(Double));

  Self[Index]:=Value;
end;

procedure TDoubleArrayHelper.Append(const Value: TDoubleArray);
var tmp : TInteger;
begin
  if Value<>nil then
  begin
    tmp:=Count;
    Resize(tmp+Value.Count);
    Move(Value[Low(Value)],Self[tmp],Value.Count*SizeOf(Double));
  end;
end;

function TDoubleArrayHelper.Copy(const Missing: TBooleanArray): TDoubleArray;
var t : TLoopInteger;
    MissingCount : TInteger;
begin
  result:=nil;

  MissingCount:=Missing.Count;

  for t:=0 to Count-1 do
      if (MissingCount<=t) or (not Missing[t]) then
         result.Append(Self[t]);
end;

function TDoubleArrayHelper.Correlation(const Y: TDoubleArray; const XMean,YMean: Double): Double;
var tmp : Double;
begin
  tmp:=StdDeviation(XMean)*Y.StdDeviation(YMean);

  if tmp=0 then
     result:=0
  else
     result:=CoVariance(Y,XMean,YMean)/tmp;
end;

{$IFDEF CPUX86}

{ TExtendedArrayHelper }

function TExtendedArrayHelper.Count: TInteger;
begin
  result:=Length(Self);
end;

function TExtendedArrayHelper.Map(out Median,Mode: Extended): TExtendedMap;
var tmp : TExtendedArray;
    tmpMax : TInteger;
begin
  result:=TExtendedMap.Create;

  if Count>0 then
  begin
    result.FSorted:=GuessOrder;

    if result.Sorted<>Ascending then
    begin
      tmp:=Copy;
      tmp.Sort; // This calls the Sort(Ascending, Swap) overload.
                // If CPUX86 is defined, this will be our new IntroSort with Self.Swap.
    end
    else
      tmp:=Self;

    Median:=tmp[Count div 2];

    result.CreateMap(tmp);
  end;

  tmpMax:=result.Map.IndexOfMax;

  if tmpMax=-1 then
     Mode:=0
  else
     Mode:=result.Items[tmpMax];
end;

function TExtendedArrayHelper.Maximum: Extended;
var n : TInteger;
    t : TLoopInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
  begin
    result:=Self[0];

    for t:=1 to n-1 do
        if Self[t]>result then
           result:=Self[t];
  end;
end;

function TExtendedArrayHelper.Mean: Extended;
var n : TInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
     result:=Sum/n;
end;

function TExtendedArrayHelper.Minimum: Extended;
var n : TInteger;
    t : TLoopInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
  begin
    result:=Self[0];

    for t:=1 to n-1 do
        if Self[t]<result then
           result:=Self[t];
  end;
end;

procedure TExtendedArrayHelper.Normalize(const Mean: Extended);
var t : TLoopInteger;
begin
  for t:=0 to Count-1 do
      Self[t]:=Self[t]-Mean;
end;

procedure TExtendedArrayHelper.Resize(const ACount: TInteger);
begin
  SetLength(Self,ACount);
end;

procedure TExtendedArrayHelper.Reverse;
begin
  TArrayReverse<Extended>.Reverse(Self);
end;

procedure TExtendedArrayHelper.Initialize(const Value:Extended);
var t : TLoopInteger;
begin
  for t:=0 to Count-1 do
      Self[t]:=Value;
end;

procedure TExtendedArrayHelper.Sort(const Ascending:Boolean; const Swap:TSwapProc);

const
  SortThreshold = 16;

  procedure ExtendedInsertionSort_WithSwap(L, R: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var i, j, currentIndexToInsert: TInteger;
  begin
    if IsAscending then
    begin
      for i := L + 1 to R do
      begin
        currentIndexToInsert := i;
        j := i - 1;
        while (j >= L) and (Self[j] > Self[currentIndexToInsert]) do
        begin
          CurrentSwap(j, currentIndexToInsert);
          currentIndexToInsert := j;
          Dec(j);
        end;
      end;
    end
    else // Descending
    begin
      for i := L + 1 to R do
      begin
        currentIndexToInsert := i;
        j := i - 1;
        while (j >= L) and (Self[j] < Self[currentIndexToInsert]) do
        begin
          CurrentSwap(j, currentIndexToInsert);
          currentIndexToInsert := j;
          Dec(j);
        end;
      end;
    end;
  end;

  procedure ExtendedHeapify_WithSwap(CurrentIndex, CurrentCount, LBoundary: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var
    RootToHeapify, LeftChild, RightChild: TInteger;
  begin
    RootToHeapify := CurrentIndex;
    LeftChild := 2 * CurrentIndex + 1;
    RightChild := 2 * CurrentIndex + 2;

    if IsAscending then
    begin
      if (LeftChild < CurrentCount) and (Self[LBoundary + LeftChild] > Self[LBoundary + RootToHeapify]) then
        RootToHeapify := LeftChild;
      if (RightChild < CurrentCount) and (Self[LBoundary + RightChild] > Self[LBoundary + RootToHeapify]) then
        RootToHeapify := RightChild;
    end
    else // Descending
    begin
      if (LeftChild < CurrentCount) and (Self[LBoundary + LeftChild] < Self[LBoundary + RootToHeapify]) then
        RootToHeapify := LeftChild;
      if (RightChild < CurrentCount) and (Self[LBoundary + RightChild] < Self[LBoundary + RootToHeapify]) then
        RootToHeapify := RightChild;
    end;

    if RootToHeapify <> CurrentIndex then
    begin
      CurrentSwap(LBoundary + CurrentIndex, LBoundary + RootToHeapify);
      ExtendedHeapify_WithSwap(RootToHeapify, CurrentCount, LBoundary, IsAscending, CurrentSwap);
    end;
  end;

  procedure ExtendedHeapSort_WithSwap(L, R: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var
    i, HeapSize: TInteger;
  begin
    HeapSize := R - L + 1;
    if HeapSize < 2 then Exit;

    for i := (HeapSize div 2) - 1 downto 0 do
      ExtendedHeapify_WithSwap(i, HeapSize, L, IsAscending, CurrentSwap);

    for i := HeapSize - 1 downto 1 do
    begin
      CurrentSwap(L, L + i);
      ExtendedHeapify_WithSwap(0, i, L, IsAscending, CurrentSwap);
    end;
  end;

  procedure PrivateSort(const l,r:TInteger; Depth: Integer);
  var
      pivotValue : Extended;
      i, j, pivotIndex : TInteger;
      N, MaxDepthAllowed : TInteger;
  begin
    N := r - l + 1;
    if N <= 1 then Exit;

    if N > 0 then
      MaxDepthAllowed := 2 * Trunc(System.Math.Log2(N))
    else
      MaxDepthAllowed := 0;

    if Depth > MaxDepthAllowed then
    begin
      ExtendedHeapSort_WithSwap(l, r, Ascending, Swap);
      Exit;
    end;

    if N <= SortThreshold then
    begin
      ExtendedInsertionSort_WithSwap(l, r, Ascending, Swap);
      Exit;
    end;

    i := l;
    j := r;
    pivotIndex := (l + j) shr 1;
    pivotValue := Self[pivotIndex];

    while i <= j do
    begin
      if Ascending then
      begin
        while Self[i] < pivotValue do Inc(i);
        while Self[j] > pivotValue do Dec(j);
      end
      else // Descending
      begin
        while Self[i] > pivotValue do Inc(i);
        while Self[j] < pivotValue do Dec(j);
      end;

      if i <= j then
      begin
        if i <> j then
          Swap(i, j); // This is the TSwapProc parameter
        Inc(i);
        Dec(j);
      end;
    end;

    if l < j then
       PrivateSort(l, j, Depth + 1);

    if i < r then
       PrivateSort(i, r, Depth + 1);
  end;

begin
  if Count>1 then
     PrivateSort(0,Count-1, 0);
end;

procedure TExtendedArrayHelper.Sort(const Ascending: Boolean=True);
begin
  Sort(Ascending,Swap); // This now calls the new IntroSort with Self.Swap
end;

function TExtendedArrayHelper.Distribution(const Mean,StdDeviation:Extended; const Exponent:Integer):Extended;

  function SumOfDifferences:Extended;
  var t : TLoopInteger;
  begin
    result:=0;

    for t:=0 to Count-1 do
        result:=result+IntPower((Self[t]-Mean),Exponent);
  end;

begin
  result:=(Count-1)*IntPower(StdDeviation,Exponent);

  if result<>0 then
     result:=SumOfDifferences/result;
end;

function TExtendedArrayHelper.SortedFind(const Value: Extended; out Exists:Boolean):TNativeInteger;
var
  L, H : TInteger;
begin
  L := 0;
  H := High(Self);

  while L <= H do
  begin
    result := (L + H) shr 1;

    if Self[result]<Value then
       L := Succ(result)
    else
    if Self[result]=Value then
    begin
      Exists:=True;
      Exit;
    end
    else
       H := Pred(result);
  end;

  result:=L;
  Exists:=False;
end;

function TExtendedArrayHelper.Stats: TExtendedStats;
var t : TLoopInteger;
    tmpSum : Extended;
begin
  result:=TExtendedStats.Create;

  if Count>0 then
  begin
    result.Min:=Self[0];
    result.Max:=Self[0];

    tmpSum:=Self[0];

    for t:=1 to Count-1 do
    begin
      if Self[t]<result.Min then
         result.Min:=Self[t]
      else
      if Self[t]>result.Max then
         result.Max:=Self[t];

      tmpSum:=tmpSum+Self[t];
    end;

    result.Range:=result.Max-result.Min;
    result.Mean:=tmpSum/Count;
    result.Sum:=tmpSum;

    result.Variance:=Variance(result.Mean);
    result.StdDeviation:=Sqrt(result.Variance);

    result.Skewness:=Distribution(result.Mean,result.StdDeviation, 3);
    result.Peakedness:=Distribution(result.Mean,result.StdDeviation, 4);
  end;
end;

function TExtendedArrayHelper.StdDeviation(const Mean: Extended): Extended;
begin
  result:=Sqrt(Variance(Mean));
end;

function TExtendedArrayHelper.Sum: Extended;
var t : TLoopInteger;
begin
  result:=0;

  for t:=0 to Count-1 do
      result:=result+Self[t];
end;

function TExtendedArrayHelper.SumOfSquares: Extended;
var t : TLoopInteger;
begin
  result:=0;

  for t:=0 to Count-1 do
      result:=result+Sqr(Self[t]);
end;

procedure TExtendedArrayHelper.Swap(const A, B: TInteger);
var tmp : Extended;
begin
  tmp:=Self[A];
  Self[A]:=Self[B];
  Self[B]:=tmp;
end;

function TExtendedArrayHelper.Variance(const Mean: Extended): Extended;
var t : TLoopInteger;
    n : TInteger;
begin
  result:=0;

  n:=Count;

  if n>1 then
  begin
    for t:=0 to n-1 do
        result:=result+Sqr(Self[t]-Mean);

    result:=result/Pred(n);
  end;
end;

function TExtendedArrayHelper.CoVariance(const Y: TExtendedArray; const XMean,YMean: Extended): Extended;
var t : TLoopInteger;
    n : TInteger;
begin
  result:=0;

  n:=Count;

  if n>1 then
  begin
    if Y.Count<>n then
       DifferentArrayLength;

    for t:=0 to n-1 do
        result:=result+( (Self[t]-XMean)*(Y[t]-YMean) );

    result:=result/Pred(n);
  end;
end;

procedure TExtendedArrayHelper.Delete(const Index:TInteger; const ACount: TInteger=1);
{$IFNDEF DELETEARRAY}
var tmp : TInteger;
{$ENDIF}
begin
  {$IFDEF DELETEARRAY}
  System.Delete(Self,Index,ACount);
  {$ELSE}
  tmp:=Count-ACount;

  if tmp-Index>0 then
     System.Move(Self[Index+ACount],Self[Index],SizeOf(Extended)*(tmp-Index));

  Resize(tmp);
  {$ENDIF}
end;

procedure TExtendedArrayHelper.Empty;
begin
  Resize(0);
end;

function TExtendedArrayHelper.ExistsBefore(const AIndex:TInteger):Boolean;
var t : TLoopInteger;
begin
  for t:=0 to AIndex-1 do
      if Self[t]=Self[AIndex] then
         Exit(True);

  result:=False;
end;

function TExtendedArrayHelper.GuessOrder:TDataOrder;

  function GuessStart(out Order:TDataOrder):TInteger;
  begin
    Order:=None;
    result:=1;

    repeat
      if Self[result]>Self[result-1] then
      begin
        Order:=Ascending;
        break;
      end
      else
      if Self[result]<Self[result-1] then
      begin
        Order:=Descending;
        break;
      end
      else
         Inc(result);

    until result>=Count;
  end;

var n,
    c,
    t : TInteger;
begin
  result:=None;

  c:=Count;

  if c=1 then
     result:=Ascending
  else
  if c>1 then
  begin
    n:=GuessStart(result);

    t:=n+1;

    if result=Ascending then
    begin
      while t<c do
      begin
        if Self[t]<Self[t-1] then
           Exit(None);

        Inc(t);
      end;
    end
    else
    if result=Descending then
      while t<c do
      begin
        if Self[t]>Self[t-1] then
           Exit(None);

        Inc(t);
      end;
  end;
end;

procedure TExtendedArrayHelper.Insert(const Index: TInteger; const Value: Extended);
var tmp : TInteger;
begin
  tmp:=Count;
  Resize(tmp+1);

  if Index<tmp then
     System.Move(Self[Index],Self[Index+1],(tmp-Index)*SizeOf(Extended));

  Self[Index]:=Value;
end;

function TExtendedArrayHelper.Append(const Value: Extended):TInteger;
begin
  result:=Count;
  Resize(result+1);
  Self[result]:=Value;
end;

procedure TExtendedArrayHelper.Append(const Value:TExtendedArray);
var tmp : TInteger;
begin
  if Value<>nil then
  begin
    tmp:=Count;
    Resize(tmp+Value.Count);
    Move(Value[Low(Value)],Self[tmp],Value.Count*SizeOf(Extended));
  end;
end;

function TExtendedArrayHelper.Copy(const AIndex,ACount:TInteger): TExtendedArray;
begin
  result:=System.Copy(Self,AIndex,ACount);
end;

function TExtendedArrayHelper.Copy: TExtendedArray;
begin
  result:=Copy(0,Count);
end;

function TExtendedArrayHelper.Copy(const Missing:TBooleanArray): TExtendedArray;
var t : TLoopInteger;
    MissingCount : TInteger;
begin
  result:=nil;

  MissingCount:=Missing.Count;

  for t:=0 to Count-1 do
      if (MissingCount<=t) or (not Missing[t]) then
         result.Append(Self[t]);
end;

function TExtendedArrayHelper.Correlation(const Y: TExtendedArray; const XMean,YMean: Extended): Extended;
var tmp : Extended;
begin
  tmp:=StdDeviation(XMean)*Y.StdDeviation(YMean);

  if tmp=0 then
     result:=0
  else
     result:=CoVariance(Y,XMean,YMean)/tmp;
end;
{$ENDIF}

{ TFloatGridHelper }

function TFloatGridHelper.Count: TInteger;
begin
  result:=Length(Self);
end;

function TFloatGridHelper.CoVarianceMatrix(const Means: TDoubleArray): TSquareGrid;
var c1,c2 : Integer;
    n : TInteger;
begin
  n:=Count;

  if n>0 then
  begin
    {$IFDEF FPC}
    result:=nil; // <-- just to skip warning
    {$ENDIF}

    result.Resize(n);

    for c1:=0 to n-1 do
    begin
      result[c1,c1]:=Self[c1].Variance(Means[c1]);

      for c2:=c1+1 to n-1 do
      begin
        result[c1,c2]:=Self[c1].CoVariance(Self[c2],Means[c1],Means[c2]);

        result[c2,c1]:=result[c1,c2];
      end;
    end;
  end;
end;

function TFloatGridHelper.Means: TDoubleArray;
var t : TLoopInteger;
    n : TInteger;
begin
  n:=Count;

  if n>0 then
  begin
    {$IFDEF FPC}
    result:=nil; // <-- just to skip warning
    {$ENDIF}

    result.Resize(n);

    for t:=0 to n-1 do
        result[t]:=Self[t].Mean;
  end;
end;

procedure TFloatGridHelper.Normalize(const Means: TDoubleArray);
var t : TLoopInteger;
begin
  for t:=0 to Count-1 do
      Self[t].Normalize(Means[t]);
end;

procedure TFloatGridHelper.InitializeRandom;
var c,r: TLoopInteger;
begin
  Randomize;

  for c:=0 to High(Self) do
      for r:=0 to High(Self[c]) do
          Self[c,r]:=System.Random;
end;

procedure TFloatGridHelper.Resize(const Cols, Rows: TInteger);
begin
  SetLength(Self,Cols,Rows);
end;

procedure TFloatGridHelper.Initialize(const Value:Double);
var c,r : TLoopInteger;
begin
  for c:=0 to High(Self) do
      for r:=0 to High(Self[c]) do
          Self[c,r]:=Value;
end;

function TFloatGridHelper.ScatterMatrix(const CoVarianceMatrix:TSquareGrid):TSquareGrid;
var c1,c2 : TLoopInteger;
    n,m : TInteger;
begin
  n:=Count;

  if n>0 then
  begin
    {$IFDEF FPC}
    result:=nil; // <-- just to skip warning
    {$ENDIF}

    result.Resize(n);

    m:=Self[0].Count;

    for c1:=0 to n-1 do
        for c2:=0 to n-1 do
            result[c1,c2]:=CoVarianceMatrix[c1,c2]*(m-1);
  end;
end;

{ TSquareGridHelper }

function TSquareGridHelper.Count: TInteger;
begin
  result:=Length(Self);
end;

procedure TSquareGridHelper.MakeIdentity;
var t : TLoopInteger;
begin
  Initialize;

  for t:=0 to Count-1 do
      Self[t,t]:=1;
end;

procedure TSquareGridHelper.Resize(const ACount: TInteger);
begin
  SetLength(Self,ACount,ACount);
end;

procedure TSquareGridHelper.Initialize(const Value:Double);
var c,r : TLoopInteger;
begin
  for c:=0 to High(Self) do
      for r:=0 to High(Self[c]) do
          Self[c,r]:=Value;
end;

{ TInt32ArrayHelper }

// BIG WARNING: Inlined methods should be ABOVE other methods, AND in ORDER

function TInt32ArrayHelper.Count: TInteger;
begin
  result:=Length(Self);
end;

function TInt32ArrayHelper.Copy(const AIndex,ACount:TInteger): TInt32Array;
begin
  result:=System.Copy(Self,AIndex,ACount);
end;

function TInt32ArrayHelper.Copy: TInt32Array;
begin
  result:=Copy(0,Count);
end;

procedure TInt32ArrayHelper.Resize(const ACount: TInteger);
begin
  SetLength(Self,ACount);
end;

procedure TInt32ArrayHelper.Reverse;
begin
  TArrayReverse<Integer>.Reverse(Self);
end;

procedure TInt32ArrayHelper.Empty;
begin
  Resize(0);
end;

{$DEFINE USECOMPAREMEM}

function TInt32ArrayHelper.Equals(const Value: TInt32Array): Boolean;
{$IFNDEF USECOMPAREMEM}
var t : TInteger;
{$ENDIF}
begin
  result:=Count=Value.Count;

  if result then
     {$IFDEF USECOMPAREMEM}
     result:=CompareMem(@Self[0],@Value[0],SizeOf(Integer)*Count);
     {$ELSE}
     for t:=0 to Count-1 do
         if Self[t]<>Value[t] then
            Exit(False);
     {$ENDIF}
end;

procedure TInt32ArrayHelper.Swap(const A, B: TInteger);
var tmp : Integer;
begin
  tmp:=Self[A];
  Self[A]:=Self[B];
  Self[B]:=tmp;
end;

function TInt32ArrayHelper.Append(const Value: Integer):TInteger;
begin
  result:=Count;
  Resize(result+1);
  Self[result]:=Value;
end;

procedure TInt32ArrayHelper.Append(const Value: TInt32Array);
var tmp : TInteger;
begin
  if Value<>nil then
  begin
    tmp:=Count;
    Resize(tmp+Value.Count);
    Move(Value[Low(Value)],Self[tmp],Value.Count*SizeOf(Integer));
  end;
end;

function TInt32ArrayHelper.Copy(const Missing: TBooleanArray): TInt32Array;
var t : TLoopInteger;
    MissingCount : TInteger;
begin
  result:=nil;

  MissingCount:=Missing.Count;

  for t:=0 to Count-1 do
      if (MissingCount<=t) or (not Missing[t]) then
         result.Append(Self[t]);
end;

function TInt32ArrayHelper.Correlation(const Y: TInt32Array; const XMean,
  YMean: Double): Double;
var tmp : Double;
begin
  tmp:=StdDeviation(XMean)*Y.StdDeviation(YMean);

  if tmp=0 then
     result:=0
  else
     result:=CoVariance(Y,XMean,YMean)/tmp;
end;

function TInt32ArrayHelper.CoVariance(const Y: TInt32Array; const XMean,
  YMean: Double): Double;
var t : TLoopInteger;
    n : TInteger;
begin
  result:=0;

  n:=Count;

  if n>1 then
  begin
    if Y.Count<>n then
       DifferentArrayLength;

    for t:=0 to n-1 do
        result:=result+( (Self[t]-XMean)*(Y[t]-YMean) );

    result:=result/Pred(n);
  end;
end;

procedure TInt32ArrayHelper.Delete(const Index:TInteger; const ACount: TInteger=1);
{$IFNDEF DELETEARRAY}
var tmp : TInteger;
{$ENDIF}
begin
  {$IFDEF DELETEARRAY}
  System.Delete(Self,Index,ACount);
  {$ELSE}
  tmp:=Count-ACount;

  if tmp-Index>0 then
     System.Move(Self[Index+ACount],Self[Index],SizeOf(Integer)*(tmp-Index));

  Resize(tmp);
  {$ENDIF}
end;

function TInt32ArrayHelper.Distribution(const Mean, StdDeviation: Double;
  const Exponent: Integer): Double;

  function SumOfDifferences:Double;
  var t : TLoopInteger;
  begin
    result:=0;

    // IntPower is double slower than direct 1.0*v*v*v....

    for t:=0 to Count-1 do
        result:=result+IntPower((Self[t]-Mean),Exponent);
  end;

begin
  result:=(Count-1)*IntPower(StdDeviation,Exponent);

  if result<>0 then
     result:=SumOfDifferences/result;
end;

function TInt32ArrayHelper.ExistsBefore(const AIndex: TInteger): Boolean;
var t : TLoopInteger;
begin
  for t:=0 to AIndex-1 do
      if Self[t]=Self[AIndex] then
         Exit(True);

  result:=False;
end;

function TInt32ArrayHelper.IndexOf(const Value: Integer): TInteger;
var t : TLoopInteger;
begin
  // When the array is sorted ascending: SortedFind is much faster (binary search)
  for t:=0 to Count-1 do
      if Self[t]=Value then
         Exit(t);

  result:=-1;
end;

function TInt32ArrayHelper.IndexOfMax: TInteger;
var t : TLoopInteger;
    tmp : Integer;
begin
  if Count>0 then
  begin
    tmp:=Self[0];
    result:=0;

    for t:=1 to Count-1 do
        if Self[t]>tmp then
        begin
          tmp:=Self[t];
          result:=t;
        end;
  end
  else
    result:=-1;
end;

procedure TInt32ArrayHelper.Insert(const Index:TInteger; const Value: Integer);
var tmp : TInteger;
begin
  tmp:=Count;
  Resize(tmp+1);

  if Index<tmp then
     System.Move(Self[Index],Self[Index+1],(tmp-Index)*SizeOf(Integer));

  Self[Index]:=Value;
end;

function TInt32ArrayHelper.GuessOrder:TDataOrder;

  function GuessStart(out Order:TDataOrder):TInteger;
  var tmpCount : TInteger; // inline Count fail
  begin
    Order:=None;
    result:=1;

    tmpCount:=Count;

    repeat
      if Self[result]>Self[result-1] then
      begin
        Order:=Ascending;
        break;
      end
      else
      if Self[result]<Self[result-1] then
      begin
        Order:=Descending;
        break;
      end
      else
         Inc(result);

    until result>=tmpCount;
  end;

var P : PInteger;
    n,
    c,
    t : TInteger;
begin
  result:=None;

  c:=Count;

  if c=1 then
     result:=Ascending
  else
  if c>1 then
  begin
    n:=GuessStart(result);

    t:=n+1;

    P:=PInteger(Self);

    if result=Ascending then
    begin
      while t<c do
      begin
        if P[t]<P[t-1] then
           Exit(None);

        Inc(t);
      end;
    end
    else
    if result=Descending then
      while t<c do
      begin
        if P[t]>P[t-1] then
           Exit(None);

        Inc(t);
      end;
  end;
end;

function TInt32ArrayHelper.Map(out Median,Mode: Integer): TInt32Map;
var tmp : TInt32Array;
    tmpMax : TInteger;
begin
  result:=TInt32Map.Create;

  if Count>0 then
  begin
    result.FSorted:=GuessOrder;

    if result.Sorted<>Ascending then
    begin
      tmp:=Copy;
      tmp.Sort;
    end
    else
      tmp:=Self;

    Median:=tmp[Count div 2];

    result.CreateMap(tmp);
  end;

  tmpMax:=result.Map.IndexOfMax;

  if tmpMax=-1 then
     Mode:=0
  else
     Mode:=result.Items[tmpMax]; // <-- not using default result[] due to inline
end;

function TInt32ArrayHelper.Maximum: Integer;
var n : TInteger;
    t : TLoopInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
  begin
    result:=Self[0];

    for t:=1 to n-1 do
        if Self[t]>result then
           result:=Self[t];
  end;
end;

function TInt32ArrayHelper.Mean: Double;
var n : TInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
     result:=Sum/n;
end;

function TInt32ArrayHelper.Minimum: Integer;
var n : TInteger;
    t : TLoopInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
  begin
    result:=Self[0];

    for t:=1 to n-1 do
        if Self[t]<result then
           result:=Self[t];
  end;
end;

procedure TInt32ArrayHelper.RemoveValue(const Value: Integer);
var t : TInteger;
begin
  t:=0;

  while t<Count do
        if Self[t]=Value then
           Delete(t)
        else
           Inc(t);
end;

procedure TInt32ArrayHelper.Sort(const Ascending:Boolean; const Swap:TSwapProc); // Overload with TSwapProc

  // IntroSort Implementation for TInt32ArrayHelper with TSwapProc

  procedure InsertionSort_WithSwap(L, R: TInteger; IsAscending: Boolean; const SwapProc: TSwapProc);
  var i, j, currentIndexToInsert: TInteger;
  begin
    if IsAscending then
    begin
      for i := L + 1 to R do
      begin
        currentIndexToInsert := i;
        j := i - 1;
        // While element at j is greater than the element we're trying to insert (at currentIndexToInsert)
        while (j >= L) and (Self[j] > Self[currentIndexToInsert]) do
        begin
          SwapProc(j, currentIndexToInsert); // Swap them
          currentIndexToInsert := j; // The element to insert has now moved to index j
          Dec(j);
        end;
      end;
    end
    else // Descending
    begin
      for i := L + 1 to R do
      begin
        currentIndexToInsert := i;
        j := i - 1;
        // While element at j is less than the element we're trying to insert
        while (j >= L) and (Self[j] < Self[currentIndexToInsert]) do
        begin
          SwapProc(j, currentIndexToInsert);
          currentIndexToInsert := j;
          Dec(j);
        end;
      end;
    end;
  end;

  procedure Heapify_WithSwap(CurrentIndex, CurrentCount, LBoundary, RBoundary: TInteger; IsAscending: Boolean; const SwapProc: TSwapProc);
  var
    RootToHeapify, LeftChild, RightChild: TInteger;
  begin
    RootToHeapify := CurrentIndex; // This is the relative index within the current heap part LBoundary..RBoundary
    LeftChild := 2 * CurrentIndex + 1;
    RightChild := 2 * CurrentIndex + 2;

    // Determine the largest/smallest element among root, left child, right child
    if IsAscending then
    begin
      if (LeftChild < CurrentCount) and (Self[LBoundary + LeftChild] > Self[LBoundary + RootToHeapify]) then
        RootToHeapify := LeftChild;
      if (RightChild < CurrentCount) and (Self[LBoundary + RightChild] > Self[LBoundary + RootToHeapify]) then
        RootToHeapify := RightChild;
    end
    else // Descending
    begin
      if (LeftChild < CurrentCount) and (Self[LBoundary + LeftChild] < Self[LBoundary + RootToHeapify]) then
        RootToHeapify := LeftChild;
      if (RightChild < CurrentCount) and (Self[LBoundary + RightChild] < Self[LBoundary + RootToHeapify]) then
        RootToHeapify := RightChild;
    end;

    // If root is not the largest/smallest, swap with largest/smallest and heapify the affected sub-tree
    if RootToHeapify <> CurrentIndex then
    begin
      SwapProc(LBoundary + CurrentIndex, LBoundary + RootToHeapify); // Use absolute indices for swap
      Heapify_WithSwap(RootToHeapify, CurrentCount, LBoundary, RBoundary, IsAscending, SwapProc);
    end;
  end;

  procedure HeapSort_WithSwap(L, R: TInteger; IsAscending: Boolean; const SwapProc: TSwapProc);
  var
    i, HeapSize: TInteger;
  begin
    HeapSize := R - L + 1;
    if HeapSize < 2 then Exit; // Already sorted or empty

    // Build heap (rearrange array). Iterate from the last non-leaf node up to the root.
    // Indices are relative to the start of the L..R partition for heap logic.
    for i := (HeapSize div 2) - 1 downto 0 do
      Heapify_WithSwap(i, HeapSize, L, R, IsAscending, SwapProc);

    // One by one extract an element from heap
    for i := HeapSize - 1 downto 1 do // Iterate from the end of the heap
    begin
      // Move current root (Self[L], the largest/smallest) to the end of the current heap segment (Self[L+i])
      SwapProc(L, L + i);
      // Call heapify on the reduced heap (size i). Root is index 0 relative to L.
      Heapify_WithSwap(0, i, L, R, IsAscending, SwapProc);
    end;
  end;

  procedure PrivateSort(const l,r:TInteger; Depth: Integer); // Added Depth parameter
  var
      pivotValue : Int32; // Stores the value of the pivot element
      i : TInteger;       // Left scan index
      j : TInteger;       // Right scan index
      pivotIndex : TInteger; // Index of the pivot element
      N : TInteger;
      MaxDepthAllowed : TInteger;
  begin
    N := r - l + 1;

    if N <= 1 then Exit; // Already sorted or empty partition

    // Calculate max recursion depth: 2 * Floor(Log2(N))
    // Handle N=0 or N=1 for Log2 if it occurs, though N > 1 here.
    if N > 0 then // Ensure N is positive for Log2
      MaxDepthAllowed := 2 * Trunc(System.Math.Log2(N))
    else
      MaxDepthAllowed := 0; // Should not happen if N > 1 check is effective

    // Fallback to HeapSort if recursion depth limit is exceeded
    if Depth > MaxDepthAllowed then
    begin
      HeapSort_WithSwap(l, r, Ascending, Swap);
      Exit;
    end;

    // Fallback to InsertionSort for small partitions
    if N <= SortThreshold then
    begin
      InsertionSort_WithSwap(l, r, Ascending, Swap);
      Exit;
    end;

    // QuickSort partition logic
    i := l;
    j := r;
    pivotIndex := (l + j) shr 1; // Simple pivot selection, consider Median-of-Three
    pivotValue := Self[pivotIndex];

    while i <= j do // Changed from i < j to i <= j for correct partitioning
    begin
      if Ascending then
      begin
        while Self[i] < pivotValue do Inc(i);
        while pivotValue < Self[j] do Dec(j);
      end
      else // Descending
      begin
        while Self[i] > pivotValue do Inc(i);
        while pivotValue > Self[j] do Dec(j);
      end;

      if i <= j then // If pointers haven't crossed or met
      begin
        if i <> j then // Avoid swapping element with itself
            Swap(i, j); // Use the provided Swap procedure (parameter)

        // Crucial: if pivot was swapped, update its index.
        // This was a source of error in original code if pivotValue was not used consistently.
        // However, since we stored pivotValue, we don't need to track pivotIndex as rigorously
        // for value comparison, but it's good practice if we were to change pivotIndex.
        // For this implementation, pivotValue is fixed for the partition step.
        // The original code had `tmp := Self[x]` updates if x was swapped, which is not needed if pivotValue is used.

        Inc(i);
        Dec(j);
      end;
    end;

    // Recursive calls for sub-partitions
    if l < j then // If left partition exists
       PrivateSort(l, j, Depth + 1);

    if i < r then // If right partition exists
       PrivateSort(i, r, Depth + 1);
  end;

begin
  if Count > 1 then
     PrivateSort(0, Count - 1, 0); // Initial call with depth 0
end;

{.$DEFINE FASTSWAP} // No gain

{$IFDEF FASTSWAP}
procedure SwapItem(const A,B:PInteger); overload; inline;
var tmp : Integer;
begin
  tmp:=A^;
  A^:=B^;
  B^:=tmp;
end;
{$ENDIF}

// Duplicate code (not possible to use generics here),
// with the only difference is no Swap procedure is called (this is 35% faster)
procedure TInt32ArrayHelper.Sort(const FromIndex, ToIndex: TInteger; // This is the existing overload, should remain unchanged
  const Ascending: Boolean);

  procedure PrivateSort(const l,r:TInteger); // This is the PrivateSort for the overload WITHOUT TSwapProc
  var
      P : PInteger;

      {$IFNDEF FASTSWAP}
      Temp : Int32;
      {$ENDIF}
      tmpValue : Int32; // Renamed tmp to tmpValue to avoid conflict with pivot tmp in the other PrivateSort
      i : TInteger;
      j : TInteger;
      pivotIdx : TInteger; // Renamed x to pivotIdx
  begin
    P:=Pointer(Self);

    if r-l<=SortThreshold then
    begin
      // Insertion Sort - This is for the non-SwapProc version, uses direct assignment
      if Ascending then
         for i:=L+1 to R do
         begin
           tmpValue:=P[i];
           j:=Pred(i);

           while (j>=L) and (P[j]>tmpValue) do
           begin
             P[j+1]:=P[j];
             Dec(j);
           end;

           P[j+1]:=tmpValue;
         end
      else
         for i:=L+1 to R do
         begin
           tmpValue:=P[i];
           j:=Pred(i);

           while (j>=L) and (P[j]<tmpValue) do
           begin
             P[j+1]:=P[j];
             Dec(j);
           end;

           P[j+1]:=tmpValue;
         end;

      Exit;
    end;

    i:=l;
    j:=r;
    pivotIdx:=(i+j) shr 1; // median3 ?

    tmpValue:=P[pivotIdx]; // Pivot value

    // In this version of PrivateSort (without TSwapProc), the loop condition is i<j
    // and pivot update logic is slightly different. Keep it as is for this specific sort method.
    while i<j do
    begin
      if Ascending then
      begin
        while P[i]<tmpValue do inc(i);
        while tmpValue<P[j] do dec(j);
      end
      else
      begin
        while P[i]>tmpValue do inc(i);
        while tmpValue<P[j] do dec(j);
      end;

      if i<j then
      begin
        {$IFDEF FASTSWAP}
        SwapItem(@P[i],@P[j]);
        {$ELSE}
        Temp:=P[i];
        P[i]:=P[j];
        P[j]:=Temp;
        {$ENDIF}

        // If the pivot element itself was moved, its index `pivotIdx` needs to be updated.
        // And because `tmpValue` holds the *value* of the pivot, if the original pivot element
        // is moved, `tmpValue` still holds the correct comparison value.
        // The original logic here updated `tmpValue` if the pivot *element* was part of the swap
        // and its *index* changed. This is subtle.
        // If `P[pivotIdx]` is swapped, then `tmpValue` (which was `P[pivotIdx]` before swap)
        // needs to be compared against the new values at P[i] and P[j].
        // The original code:
        // if i=pivotIdx then pivotIdx:=j; tmpValue:=P[pivotIdx];
        // else if j=pivotIdx then pivotIdx:=i; tmpValue:=P[pivotIdx];
        // This re-reads tmpValue from the new pivot position. Let's keep this behavior for this specific sort.
        if i=pivotIdx then
        begin
          pivotIdx:=j;
          tmpValue:=P[pivotIdx];
        end
        else
        if j=pivotIdx then
        begin
          pivotIdx:=i;
          tmpValue:=P[pivotIdx];
        end;
      end;

      if i<=j then // This condition ensures progress even if i or j lands on the pivot
      begin
        inc(i);
        dec(j)
      end;
    end;

    // Potential parallelization
    if l<j then
       PrivateSort(l,j);

    if i<r then
       PrivateSort(i,r);
  end;

begin
  if ToIndex-FromIndex>0 then
     PrivateSort(FromIndex,ToIndex);
end;

procedure TInt32ArrayHelper.Sort(const Ascending: Boolean);
begin
  if Count>1 then
     Sort(0,Count-1,Ascending);
end;

// Assumption: Self is sorted ascending
function TInt32ArrayHelper.SortedFind(const Value: Integer; out Exists:Boolean):TNativeInteger;
var
  L, H : TInteger;
  tmp : Integer; // opt
begin
  L := 0;
  H := High(Self);

  while L <= H do
  begin
    result := (L + H) shr 1;

    tmp:=Self[result];

    if tmp<Value then
       L := Succ(result)
    else
    if tmp=Value then
    begin
      Exists:=True;
      Exit;
    end
    else
       H := Pred(result);
  end;

  result:=L;
  Exists:=False;
end;

function TInt32ArrayHelper.Stats: TInt32Stats;

  // 30% faster
  (*
  procedure CalcSkewPeak;
  var tmp3,
      tmp4,
      tmpDif,
      tmpDif3,
      Std,
      Std3 : Double;

      t : TLoopInteger;
  begin
    Std:=result.StdDeviation;
    Std3:=Std*Std*Std;

    result.Skewness:=(Count-1)*Std3;

    if result.Skewness<>0 then
    begin
      tmp3:=0;
      tmp4:=0;

      for t:=0 to Count-1 do
      begin
        tmpDif:=(Self[t]-result.Mean);
        tmpDif3:=tmpDif*tmpDif*tmpDif;

        tmp3:=tmp3+tmpDif3;
        tmp4:=tmp4+(tmpDif3*tmpDif);
      end;

      result.Skewness:=tmp3/result.Skewness;

      result.Peakedness:=(Count-1)*Std3*Std;

      if result.Peakedness<>0 then
         result.Peakedness:=tmp4/result.Peakedness;
    end;
  end;
  *)

//{$DEFINE TEMPINT32} // Faster in both x64 and 32bit

(*
var t : TLoopInteger;
    tmpSum : Double;

    {$IFDEF TEMPINT32}
    tmp : Int32;
    {$ENDIF}
*)
begin
  result:=TInt32Stats.Create;

  if Count>0 then
  begin
    result.CalcStats(Self);

    (*

    result.Min:=Self[0];
    result.Max:=Self[0];

    tmpSum:=Self[0];

    // SSE sum:
    // is array 16 aligned?
    // loop step 32:
    //  v0 := mk_128(Self[t])    mm_add_ps(v0, Self[t+4])
    //  v1 := mk_128(Self[t+8])  mm_add_ps(v0, Self[t+12])
    //  v2 := mk_128(Self[t+16]) mm_add_ps(v0, Self[t+20])
    //  v3 := mk_128(Self[t+24]) mm_add_ps(v0, Self[t+28])
    //
    //  v0 := mm_add_ps(v0,v1)
    //  v2 := mm_add_ps(v2,v3)
    //  v0 := mm_add_ps(v0,v2)
    //  mm_store_ps(v,v0);
    //  result:=result+v[0]+v[1]+v[2]+v[3]

    {$IFDEF TEMPINT32}
    for t:=1 to Count-1 do
    begin
      tmp:=Self[t];

      if tmp<result.Min then
         result.Min:=tmp
      else
      if tmp>result.Max then
         result.Max:=tmp;

      tmpSum:=tmpSum+tmp;
    end;
    {$ELSE}
    for t:=1 to Count-1 do
    begin
      if Self[t]<result.Min then
         result.Min:=Self[t]
      else
      if Self[t]>result.Max then
         result.Max:=Self[t];

      tmpSum:=tmpSum+Self[t];
    end;
    {$ENDIF}

    result.Mean:=tmpSum/Count;
    result.Sum:=tmpSum;

    result.Range:=result.Max-result.Min;
    *)

    result.Variance:=Variance(result.Mean);
    result.StdDeviation:=Sqrt(result.Variance);

    // CalcSkewPeak; // 30% faster, but Skewness value seems different

    result.Skewness:=Distribution(result.Mean,result.StdDeviation, 3);
    result.Peakedness:=Distribution(result.Mean,result.StdDeviation, 4);
  end;
end;

function TInt32ArrayHelper.StdDeviation(const Mean: Double): Double;
begin
  result:=Sqrt(Variance(Mean));
end;

function TInt32ArrayHelper.Sum: Double;
var t : TLoopInteger;
begin
  result:=0;

  for t:=0 to Count-1 do
      result:=result+Self[t];
end;

function TInt32ArrayHelper.SumOfSquares: Double;
var t : TLoopInteger;
begin
  result:=0;

  for t:=0 to Count-1 do
      result:=result+Sqr(Self[t]);
end;

function TInt32ArrayHelper.Variance(const Mean: TFloat): TFloat;
var t : TLoopInteger;
    n : TInteger;
begin
  result:=0;

  n:=Count-1;

  // Variant algorithms: Compensated, Online, Weighted

  if n>0 then
  begin
    for t:=0 to n do
        result:=result+Sqr(Self[t]-Mean);

    result:=result/n;
  end;
end;

procedure TInt32ArrayHelper.Initialize(const Value:Integer);
var t : TLoopInteger;
begin
  for t:=0 to Count-1 do
      Self[t]:=Value;
end;

{ TInt64ArrayHelper }

function TInt64ArrayHelper.Count: TInteger;
begin
  result:=Length(Self);
end;

procedure TInt64ArrayHelper.Resize(const ACount: TInteger);
begin
  SetLength(Self,ACount);
end;

procedure TInt64ArrayHelper.Reverse;
begin
  TArrayReverse<Int64>.Reverse(Self);
end;

procedure TInt64ArrayHelper.Empty;
begin
  Resize(0);
end;

function TInt64ArrayHelper.Equals(const Value: TInt64Array): Boolean;
{$IFNDEF USECOMPAREMEM}
var t : TInteger;
{$ENDIF}
begin
  result:=Count=Value.Count;

  if result then
     {$IFDEF USECOMPAREMEM}
     result:=CompareMem(@Self[0],@Value[0],SizeOf(Int64)*Count);
     {$ELSE}
     for t:=0 to Count-1 do
         if Self[t]<>Value[t] then
            Exit(False);
     {$ENDIF}
end;

procedure TInt64ArrayHelper.Swap(const A, B: TInteger);
var tmp : Int64;
begin
  tmp:=Self[A];
  Self[A]:=Self[B];
  Self[B]:=tmp;
end;

function TInt64ArrayHelper.Copy(const AIndex,ACount:TInteger): TInt64Array;
begin
  result:=System.Copy(Self,AIndex,ACount);
end;

function TInt64ArrayHelper.Copy: TInt64Array;
begin
  result:=Copy(0,Count);
end;

function TInt64ArrayHelper.Append(const Value: Int64):TInteger;
begin
  result:=Count;
  Resize(result+1);
  Self[result]:=Value;
end;

procedure TInt64ArrayHelper.Append(const Value: TInt64Array);
var tmp : TInteger;
begin
  if Value<>nil then
  begin
    tmp:=Count;
    Resize(tmp+Value.Count);
    Move(Value[Low(Value)],Self[tmp],Value.Count*SizeOf(Int64));
  end;
end;

function TInt64ArrayHelper.Copy(const Missing: TBooleanArray): TInt64Array;
var t : TLoopInteger;
    MissingCount : TInteger;
begin
  result:=nil;

  MissingCount:=Missing.Count;

  for t:=0 to Count-1 do
      if (MissingCount<=t) or (not Missing[t]) then
         result.Append(Self[t]);
end;

function TInt64ArrayHelper.Correlation(const Y: TInt64Array; const XMean,
  YMean: Double): Double;
var tmp : Double;
begin
  tmp:=StdDeviation(XMean)*Y.StdDeviation(YMean);

  if tmp=0 then
     result:=0
  else
     result:=CoVariance(Y,XMean,YMean)/tmp;
end;

function TInt64ArrayHelper.CoVariance(const Y: TInt64Array; const XMean,
  YMean: Double): Double;
var t : TLoopInteger;
    n : TInteger;
begin
  result:=0;

  n:=Count;

  if n>1 then
  begin
    if Y.Count<>n then
       DifferentArrayLength;

    for t:=0 to n-1 do
        result:=result+( (Self[t]-XMean)*(Y[t]-YMean) );

    result:=result/Pred(n);
  end;
end;

procedure TInt64ArrayHelper.Delete(const Index:TInteger; const ACount: TInteger=1);
{$IFNDEF DELETEARRAY}
var tmp : TInteger;
{$ENDIF}
begin
  {$IFDEF DELETEARRAY}
  System.Delete(Self,Index,ACount);
  {$ELSE}
  tmp:=Count-ACount;

  if tmp-Index>0 then
     System.Move(Self[Index+ACount],Self[Index],SizeOf(Int64)*(tmp-Index));

  Resize(tmp);
  {$ENDIF}
end;

function TInt64ArrayHelper.Distribution(const Mean, StdDeviation: Double;
  const Exponent: Integer): Double;

  function SumOfDifferences:Double;
  var t : TLoopInteger;
  begin
    result:=0;

    for t:=0 to Count-1 do
        result:=result+IntPower((Self[t]-Mean),Exponent);
  end;

begin
  result:=(Count-1)*IntPower(StdDeviation,Exponent);

  if result<>0 then
     result:=SumOfDifferences/result;
end;

function TInt64ArrayHelper.ExistsBefore(const AIndex: TInteger): Boolean;
var t : TLoopInteger;
begin
  for t:=0 to AIndex-1 do
      if Self[t]=Self[AIndex] then
         Exit(True);

  result:=False;
end;

function TInt64ArrayHelper.IndexOf(const Value: Int64): TInteger;
var t : TLoopInteger;
begin
  // When the array is sorted ascending: SortedFind is much faster (binary search)
  for t:=0 to Count-1 do
      if Self[t]=Value then
         Exit(t);

  result:=-1;
end;

function TInt64ArrayHelper.IndexOfMax: TInteger;
var t : TLoopInteger;
    tmp : Int64;
begin
  if Count>0 then
  begin
    tmp:=Self[0];
    result:=0;

    for t:=1 to Count-1 do
        if Self[t]>tmp then
        begin
          tmp:=Self[t];
          result:=t;
        end;
  end
  else
    result:=-1;
end;

procedure TInt64ArrayHelper.Insert(const Index:TInteger; const Value: Int64);
var tmp : TInteger;
begin
  tmp:=Count;
  Resize(tmp+1);

  if Index<tmp then
     System.Move(Self[Index],Self[Index+1],(tmp-Index)*SizeOf(Int64));

  Self[Index]:=Value;
end;

function TInt64ArrayHelper.GuessOrder:TDataOrder;

  function GuessStart(out Order:TDataOrder):TInteger;
  begin
    Order:=None;
    result:=1;

    repeat
      if Self[result]>Self[result-1] then
      begin
        Order:=Ascending;
        break;
      end
      else
      if Self[result]<Self[result-1] then
      begin
        Order:=Descending;
        break;
      end
      else
         Inc(result);

    until result>=Count;
  end;

var n,
    c,
    t : TInteger;
begin
  result:=None;

  c:=Count;

  if c=1 then
     result:=Ascending
  else
  if c>1 then
  begin
    n:=GuessStart(result);

    t:=n+1;

    if result=Ascending then
    begin
      while t<c do
      begin
        if Self[t]<Self[t-1] then
           Exit(None);

        Inc(t);
      end;
    end
    else
    if result=Descending then
      while t<c do
      begin
        if Self[t]>Self[t-1] then
           Exit(None);

        Inc(t);
      end;
  end;
end;

function TInt64ArrayHelper.Map(out Median,Mode: Int64): TInt64Map;
var tmp : TInt64Array;
    tmpMax : TInteger;
begin
  result:=TInt64Map.Create;

  if Count>0 then
  begin
    result.FSorted:=GuessOrder;

    if result.Sorted<>Ascending then
    begin
      tmp:=Copy;
      tmp.Sort;
    end
    else
      tmp:=Self;

    Median:=tmp[Count div 2];

    result.CreateMap(tmp);
  end;

  tmpMax:=result.Map.IndexOfMax;

  if tmpMax=-1 then
     Mode:=0
  else
     Mode:=result.Items[tmpMax];
end;

function TInt64ArrayHelper.Maximum: Int64;
var n : TInteger;
    t : TLoopInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
  begin
    result:=Self[0];

    for t:=1 to n-1 do
        if Self[t]>result then
           result:=Self[t];
  end;
end;

function TInt64ArrayHelper.Mean: Double;
var n : TInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
     result:=Sum/n;
end;

function TInt64ArrayHelper.Minimum: Int64;
var n : TInteger;
    t : TLoopInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
  begin
    result:=Self[0];

    for t:=1 to n-1 do
        if Self[t]<result then
           result:=Self[t];
  end;
end;

procedure TInt64ArrayHelper.RemoveValue(const Value: Int64);
var t : TInteger;
begin
  t:=0;

  while t<Count do
        if Self[t]=Value then
           Delete(t)
        else
           Inc(t);
end;

procedure TInt64ArrayHelper.Sort(const Ascending:Boolean; const Swap:TSwapProc);

const
  SortThreshold = 16;

  procedure Int64InsertionSort_WithSwap(L, R: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var i, j, currentIndexToInsert: TInteger;
  begin
    if IsAscending then
    begin
      for i := L + 1 to R do
      begin
        currentIndexToInsert := i;
        j := i - 1;
        while (j >= L) and (Self[j] > Self[currentIndexToInsert]) do
        begin
          CurrentSwap(j, currentIndexToInsert);
          currentIndexToInsert := j;
          Dec(j);
        end;
      end;
    end
    else // Descending
    begin
      for i := L + 1 to R do
      begin
        currentIndexToInsert := i;
        j := i - 1;
        while (j >= L) and (Self[j] < Self[currentIndexToInsert]) do
        begin
          CurrentSwap(j, currentIndexToInsert);
          currentIndexToInsert := j;
          Dec(j);
        end;
      end;
    end;
  end;

  procedure Int64Heapify_WithSwap(CurrentIndex, CurrentCount, LBoundary: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var
    RootToHeapify, LeftChild, RightChild: TInteger;
  begin
    RootToHeapify := CurrentIndex;
    LeftChild := 2 * CurrentIndex + 1;
    RightChild := 2 * CurrentIndex + 2;

    if IsAscending then
    begin
      if (LeftChild < CurrentCount) and (Self[LBoundary + LeftChild] > Self[LBoundary + RootToHeapify]) then
        RootToHeapify := LeftChild;
      if (RightChild < CurrentCount) and (Self[LBoundary + RightChild] > Self[LBoundary + RootToHeapify]) then
        RootToHeapify := RightChild;
    end
    else // Descending
    begin
      if (LeftChild < CurrentCount) and (Self[LBoundary + LeftChild] < Self[LBoundary + RootToHeapify]) then
        RootToHeapify := LeftChild;
      if (RightChild < CurrentCount) and (Self[LBoundary + RightChild] < Self[LBoundary + RootToHeapify]) then
        RootToHeapify := RightChild;
    end;

    if RootToHeapify <> CurrentIndex then
    begin
      CurrentSwap(LBoundary + CurrentIndex, LBoundary + RootToHeapify);
      Int64Heapify_WithSwap(RootToHeapify, CurrentCount, LBoundary, IsAscending, CurrentSwap);
    end;
  end;

  procedure Int64HeapSort_WithSwap(L, R: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var
    i, HeapSize: TInteger;
  begin
    HeapSize := R - L + 1;
    if HeapSize < 2 then Exit;

    for i := (HeapSize div 2) - 1 downto 0 do
      Int64Heapify_WithSwap(i, HeapSize, L, IsAscending, CurrentSwap);

    for i := HeapSize - 1 downto 1 do
    begin
      CurrentSwap(L, L + i);
      Int64Heapify_WithSwap(0, i, L, IsAscending, CurrentSwap);
    end;
  end;

  procedure PrivateSort(const l,r:TInteger; Depth: Integer);
  var
      pivotValue : Int64;
      i, j, pivotIndex : TInteger;
      N, MaxDepthAllowed : TInteger;
  begin
    N := r - l + 1;
    if N <= 1 then Exit;

    if N > 0 then
      MaxDepthAllowed := 2 * Trunc(System.Math.Log2(N))
    else
      MaxDepthAllowed := 0;

    if Depth > MaxDepthAllowed then
    begin
      Int64HeapSort_WithSwap(l, r, Ascending, Swap);
      Exit;
    end;

    if N <= SortThreshold then
    begin
      Int64InsertionSort_WithSwap(l, r, Ascending, Swap);
      Exit;
    end;

    i := l;
    j := r;
    pivotIndex := (l + j) shr 1;
    pivotValue := Self[pivotIndex];

    while i <= j do
    begin
      if Ascending then
      begin
        while Self[i] < pivotValue do Inc(i);
        while Self[j] > pivotValue do Dec(j);
      end
      else // Descending
      begin
        while Self[i] > pivotValue do Inc(i);
        while Self[j] < pivotValue do Dec(j);
      end;

      if i <= j then
      begin
        if i <> j then
          Swap(i, j);
        Inc(i);
        Dec(j);
      end;
    end;

    if l < j then
       PrivateSort(l, j, Depth + 1);

    if i < r then
       PrivateSort(i, r, Depth + 1);
  end;

begin
  if Count>1 then
     PrivateSort(0,Count-1, 0);
end;

// Duplicate code (not possible to use generics here),
// with the only difference is no Swap procedure is called (this is 35% faster)
procedure TInt64ArrayHelper.Sort(const FromIndex,ToIndex:TInteger; const Ascending:Boolean=True);

  procedure PrivateSort(const l,r:TInteger);
  var Temp,
      tmp : Int64;
      i : TInteger;
      j : TInteger;
      x : TInteger;
  begin
    if r-l<=SortThreshold then
    begin
      // Insertion Sort

      if Ascending then
         for i:=L+1 to R do
         begin
           tmp:=Self[i];
           j:=Pred(i);

           while (j>=L) and (Self[j]>tmp) do
           begin
             Self[j+1]:=Self[j];
             Dec(j);
           end;

           Self[j+1]:=tmp;
         end
      else
         for i:=L+1 to R do
         begin
           tmp:=Self[i];
           j:=Pred(i);

           while (j>=L) and (Self[j]<tmp) do
           begin
             Self[j+1]:=Self[j];
             Dec(j);
           end;

           Self[j+1]:=tmp;
         end;
      Exit;
    end;

    i:=l;
    j:=r;
    x:=(i+j) shr 1;

    tmp:=Self[x];

    while i<j do
    begin
      if Ascending then
      begin
        while Self[i]<tmp do inc(i);
        while tmp<Self[j] do dec(j);
      end
      else
      begin
        while Self[i]>tmp do inc(i);
        while tmp>Self[j] do dec(j);
      end;

      if i<j then
      begin
        Temp:=Self[i];
        Self[i]:=Self[j];
        Self[j]:=Temp;

        if i=x then
        begin
          x:=j;
          tmp:=Self[x];
        end
        else
        if j=x then
        begin
          x:=i;
          tmp:=Self[x];
        end;
      end;

      if i<=j then
      begin
        inc(i);
        dec(j)
      end;
    end;

    // Potential parallelization
    if l<j then
       PrivateSort(l,j);

    if i<r then
       PrivateSort(i,r);
  end;

begin
  if ToIndex-FromIndex>0 then
     PrivateSort(FromIndex,ToIndex);
end;

procedure TInt64ArrayHelper.Sort(const Ascending: Boolean);
begin
  if Count>1 then
     Sort(0,Count-1,Ascending);
end;

function TInt64ArrayHelper.SortedFind(const Value: Int64; out Exists:Boolean):TNativeInteger;
var
  L, H : TInteger;
  tmp : Int64; // opt
begin
  L := 0;
  H := High(Self);

  while L <= H do
  begin
    result := (L + H) shr 1;

    tmp:=Self[result];

    if tmp<Value then
       L := Succ(result)
    else
    if tmp=Value then
    begin
      Exists:=True;
      Exit;
    end
    else
       H := Pred(result);
  end;

  result:=L;
  Exists:=False;
end;

function TInt64ArrayHelper.Stats: TInt64Stats;
begin
  result:=TInt64Stats.Create;

  if Count>0 then
  begin
    result.CalcStats(Self);

    result.Variance:=Variance(result.Mean);
    result.StdDeviation:=Sqrt(result.Variance);

    result.Skewness:=Distribution(result.Mean,result.StdDeviation, 3);
    result.Peakedness:=Distribution(result.Mean,result.StdDeviation, 4);
  end;
end;

function TInt64ArrayHelper.StdDeviation(const Mean: Double): Double;
begin
  result:=Sqrt(Variance(Mean));
end;

function TInt64ArrayHelper.Sum: Double;
var t : TLoopInteger;
begin
  result:=0;

  for t:=0 to Count-1 do
      result:=result+Self[t];
end;

function TInt64ArrayHelper.SumOfSquares: Double;
var t : TLoopInteger;
begin
  result:=0;

  for t:=0 to Count-1 do
      result:=result+Sqr(Self[t]);
end;

function TInt64ArrayHelper.Variance(const Mean: Double): Double;
var t : TLoopInteger;
    n : TInteger;
begin
  result:=0;

  n:=Count;

  if n>1 then
  begin
    for t:=0 to n-1 do
        result:=result+Sqr(Self[t]-Mean);

    result:=result/Pred(n);
  end;
end;

procedure TInt64ArrayHelper.Initialize(const Value:Int64);
var t : TLoopInteger;
begin
  for t:=0 to Count-1 do
      Self[t]:=Value;
end;

{ TBooleanArrayHelper }

// This method should be above any others using it (inline)
procedure TBooleanArrayHelper.Resize(const ACount: TInteger);
begin
  SetLength(Self,ACount);
end;

procedure TBooleanArrayHelper.Reverse;
begin
  TArrayReverse<Boolean>.Reverse(Self);
end;

// This method should be above any others using it (inline)
function TBooleanArrayHelper.Count: TInteger;
begin
  result:=Length(Self);
end;

function TBooleanArrayHelper.Append(const Value: Boolean):TInteger;
begin
  result:=Count;
  Resize(result+1);
  Self[result]:=Value;
end;

procedure TBooleanArrayHelper.Append(const Value: TBooleanArray);
var tmp : TInteger;
begin
  if Value<>nil then
  begin
    tmp:=Count;
    Resize(tmp+Value.Count);
    Move(Value[Low(Value)],Self[tmp],Value.Count*SizeOf(Boolean));
  end;
end;

function TBooleanArrayHelper.Compare(const A, B: TInteger): ShortInt;
begin
  if Self[A]=Self[B] then result:=0
  else
  if Self[A] then result:=1
  else
     result:=-1;
end;

function TBooleanArrayHelper.Copy(const Missing: TBooleanArray): TBooleanArray;
var t : TLoopInteger;
    MissingCount : TInteger;
begin
  result:=nil;

  MissingCount:=Missing.Count;

  for t:=0 to Count-1 do
      if (MissingCount<=t) or (not Missing[t]) then
         result.Append(Self[t]);
end;

function TBooleanArrayHelper.Copy(const AIndex,ACount:TInteger): TBooleanArray;
begin
  result:=System.Copy(Self,AIndex,ACount);
end;

function TBooleanArrayHelper.Copy: TBooleanArray;
begin
  result:=Copy(0,Count);
end;

procedure TBooleanArrayHelper.Delete(const Index:TInteger; const ACount:TInteger=1);
{$IFNDEF DELETEARRAY}
var tmp : TInteger;
{$ENDIF}
begin
  {$IFDEF DELETEARRAY}
  System.Delete(Self,Index,ACount);
  {$ELSE}
  tmp:=Count-ACount;

  if tmp-Index>0 then
     System.Move(Self[Index+ACount],Self[Index],SizeOf(Boolean)*(tmp-Index));

  Resize(tmp);
  {$ENDIF}
end;

procedure TBooleanArrayHelper.Empty;
begin
  Resize(0);
end;

function TBooleanArrayHelper.Equals(const Value: TBooleanArray): Boolean;
{$IFNDEF USECOMPAREMEM}
var t : TInteger;
{$ENDIF}
begin
  result:=Count=Value.Count;

  if result then
     {$IFDEF USECOMPAREMEM}
     result:=CompareMem(@Self[0],@Value[0],SizeOf(Boolean)*Count);
     {$ELSE}
     for t:=0 to Count-1 do
         if Self[t]<>Value[t] then
            Exit(False);
     {$ENDIF}
end;

function TBooleanArrayHelper.ExistsBefore(const AIndex: TInteger): Boolean;
var t : TLoopInteger;
begin
  for t:=0 to AIndex-1 do
      if Self[t]=Self[AIndex] then
         Exit(True);

  result:=False;
end;

procedure TBooleanArrayHelper.Insert(const Index: TInteger; const Value: Boolean);
var tmp : TInteger;
begin
  tmp:=Count;
  Resize(tmp+1);

  if Index<tmp then
     System.Move(Self[Index],Self[Index+1],(tmp-Index)*SizeOf(Boolean));

  Self[Index]:=Value;
end;

function TBooleanArrayHelper.Map: TBooleanMap;
var t : TLoopInteger;
begin
  result:=TBooleanMap.Create;
  result.Map.Resize(2);

  for t:=0 to Count-1 do
      Inc(result.Map[Ord(Self[t])]);

  result.CachedCount:=result.Count;
end;

procedure TBooleanArrayHelper.Sort(const Ascending: Boolean;
  const Swap: TSwapProc);

  procedure PrivateSort(const l,r:TInteger);
  var i : TInteger;
      j : TInteger;
      x : TInteger;
  begin
    i:=l;
    j:=r;
    x:=(i+j) shr 1;

    while i<j do
    begin
      if Ascending then
      begin
        while Self[i]<Self[x] do inc(i);
        while Self[x]<Self[j] do dec(j);
      end
      else
      begin
        while Self[i]>Self[x] do inc(i);
        while Self[x]>Self[j] do dec(j);
      end;

      if i<j then
      begin
        {$IFDEF XE5BUG}Self.{$ENDIF}Swap(i,j);

        if i=x then
           x:=j
        else
        if j=x then
           x:=i;
      end;

      if i<=j then
      begin
        inc(i);
        dec(j)
      end;
    end;

    // Potential parallelization
    if l<j then
       PrivateSort(l,j);

    if i<r then
       PrivateSort(i,r);
  end;

begin
  if Count>1 then
     PrivateSort(0,Count-1);
end;

procedure TBooleanArrayHelper.Sort(const Ascending: Boolean);
begin
  Sort(Ascending,Swap);
end;

function TBooleanArrayHelper.Stats: TBooleanStats;
begin
  result:=TBooleanStats.Create;
end;

procedure TBooleanArrayHelper.Swap(const A, B: TInteger);
var tmp : Boolean;
begin
  tmp:=Self[A];
  Self[A]:=Self[B];
  Self[B]:=tmp;
end;

procedure TBooleanArrayHelper.Initialize(const Value:Boolean);
var t : TLoopInteger;
begin
  for t:=0 to Count-1 do
      Self[t]:=Value;
end;

{ TTextArrayHelper }

// BIG WARNING: Inlined methods should be ABOVE other methods, AND in ORDER

function TTextArrayHelper.Copy(const Missing: TBooleanArray): TTextArray;
var t : TLoopInteger;
    MissingCount : TInteger;
begin
  result:=nil;

  MissingCount:=Missing.Count;

  for t:=0 to Count-1 do
      if (MissingCount<=t) or (not Missing[t]) then
         result.Append(Self[t]);
end;

function TTextArrayHelper.Count: TInteger;
begin
  result:=Length(Self);
end;

procedure TTextArrayHelper.Resize(const ACount: TInteger);
begin
  SetLength(Self,ACount);
end;

procedure TTextArrayHelper.Reverse;
begin
  TArrayReverse<String>.Reverse(Self);
end;

procedure TTextArrayHelper.Empty;
begin
  Resize(0);
end;

function TTextArrayHelper.Equals(const Value: TTextArray): Boolean;
var t : TInteger;
begin
  result:=Count=Value.Count;

  if result then
     for t:=0 to Count-1 do
         if Self[t]<>Value[t] then
            Exit(False);
end;

procedure TTextArrayHelper.Swap(const A,B:TInteger);
var tmp : Pointer; //String;
begin
  // Fast String swap:
  tmp:=Pointer(Self[A]);
  Pointer(Self[A]):=Pointer(Self[B]);
  Pointer(Self[B]):=tmp;
end;

function TTextArrayHelper.Append(const Value: String):TInteger;
begin
  result:=Count;
  Resize(result+1);
  Self[result]:=Value;
end;

procedure TTextArrayHelper.Append(const Value: TTextArray);
var t : TLoopInteger;
    tmp : TInteger;
begin
  if Value<>nil then
  begin
    tmp:=Count;
    Resize(tmp+Value.Count);

    for t:=Low(Value) to High(Value) do
        Self[tmp+t-Low(Value)]:=Value[t];
  end;
end;


// Experimental:
{.$DEFINE FASTCOPY} // Speed hack !  Bypass System.Copy

{$IFDEF FASTCOPY}
type
  // WARNING:
  // StrRec record copied from System.pas unit. It should be IDENTICAL

  PStrRec = ^StrRec;
  StrRec = packed record
  {$IF defined(CPU64BITS)}
    _Padding: Integer; // Make 16 byte align for payload..
  {$ENDIF}
    codePage: Word;
    elemSize: Word;
    refCnt: Integer;
    length: Integer;
  end;

procedure CopyStringArray(const Source:TTextArray; out Dest:TTextArray;
                          const AIndex,ACount:TInteger);
var t : TLoopInteger;
begin
  Dest.Resize(ACount);

  Move(Source[AIndex],Dest[0],SizeOf(String)*ACount);

  for t:=0 to High(Dest) do
      if PByte(Dest[t])<>nil then
         AtomicIncrement(PStrRec(PByte(Dest[t]) - SizeOf(StrRec)).refCnt);
end;
{$ENDIF}

function TTextArrayHelper.Copy(const AIndex,ACount:TInteger): TTextArray;
{$IFNDEF FASTCOPY}
{$DEFINE LOOPCOPY}

{$IFDEF LOOPCOPY}
var t : TLoopInteger;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF FASTCOPY} // Speed hack !
  CopyStringArray(Self,result,AIndex,ACount);
  {$ELSE}

  // Direct copy is slightly faster than calling System.Copy
  {$IFDEF LOOPCOPY}

  {$IFDEF FPC}
  result:=nil; // <-- just to skip warning
  {$ENDIF}

  result.Resize(ACount);

  for t:=0 to ACount-1 do
      result[t]:=Self[AIndex+t];

  {$ELSE}
  result:=System.Copy(Self,AIndex,ACount);
  {$ENDIF}
  {$ENDIF}
end;

function TTextArrayHelper.Copy: TTextArray;
{$IFNDEF FASTCOPY}
{$IFDEF LOOPCOPY}
var t : TLoopInteger;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF FASTCOPY} // Speed hack !
  CopyStringArray(Self,result,0,Count);
  {$ELSE}

  // Direct copy is slightly faster than calling System.Copy
  {$IFDEF LOOPCOPY}

  {$IFDEF FPC}
  result:=nil; // <-- just to skip warning
  {$ENDIF}

  result.Resize(Count);

  for t:=0 to Count-1 do
      result[t]:=Self[t];

  {$ELSE}
  result:=System.Copy(Self);
  {$ENDIF}
  {$ENDIF}
end;

{.$DEFINE DELETESTRINGARRAY} // Use System unit Delete instead of our own

procedure TTextArrayHelper.Delete(const Index:TInteger; const ACount: TInteger=1);
{$IFNDEF DELETESTRINGARRAY}
var tmp : TInteger;
    t : Integer; // <-- WARNING: DX10 TLoopInteger 32bit dcc = compiler error
{$ENDIF}
begin
  {$IFDEF DELETESTRINGARRAY}
  {$IFDEF D21}
  {$IFNDEF LINUX}
  System.Delete(Self,Index,ACount);
  {$ENDIF}
  {$ENDIF}
  {$ELSE}
  tmp:=Count-ACount;

  (*
  // Safe, but Speed trap:
  for t:=Index to tmp-1 do
      Self[t]:=Self[t+1];
  *)

  // Refcounted Strings need Finalize before deleting them
  for t:=Index to Index+ACount-1 do
      Finalize(Self[t]);

  if tmp-Index>0 then
     System.Move(Self[Index+ACount],Self[Index],SizeOf(String)*(tmp-Index));

  // Last Refcounted Strings need to Initialize before deleting them because SetLength
  for t:=tmp to Count-1 do
      // Warning: Always prefix with System to not call our own Initialize method !
      System.Initialize(Self[t]);

  Resize(tmp);
  {$ENDIF}
end;

function TTextArrayHelper.GuessOrder:TDataOrder;

  function GuessStart(out Order:TDataOrder):TInteger;
  begin
    Order:=None;
    result:=1;

    repeat
      if Self[result]>Self[result-1] then
      begin
        Order:=Ascending;
        break;
      end
      else
      if Self[result]<Self[result-1] then
      begin
        Order:=Descending;
        break;
      end
      else
         Inc(result);

    until result>=Count;
  end;

var n,
    c,
    t : TInteger;
begin
  result:=None;

  c:=Count;

  if c=1 then
     result:=Ascending
  else
  if c>1 then
  begin
    n:=GuessStart(result);

    t:=n+1;

    if result=Ascending then
    begin
      while t<c do
      begin
        if Self[t]<Self[t-1] then
           Exit(None);

        Inc(t);
      end;
    end
    else
    if result=Descending then
      while t<c do
      begin
        if Self[t]>Self[t-1] then
           Exit(None);

        Inc(t);
      end;
  end;
end;

function TTextArrayHelper.IndexOf(const Value: String;
  const CaseSensitive: Boolean): TInteger;
var t : TLoopInteger;
begin
  // When the array is sorted ascending: SortedFind is much faster (binary search)

  if CaseSensitive then
  begin
    for t:=0 to Count-1 do
        if Self[t]=Value then
           Exit(t);
  end
  else
  begin
    for t:=0 to Count-1 do
        if SameText(Self[t],Value) then
           Exit(t);
  end;

  result:=-1;
end;

function TTextArrayHelper.IndexOf(const Value: String): TInteger;
begin
  result:=IndexOf(Value,True);
end;

procedure TTextArrayHelper.Insert(const Index: TInteger; const Value: String);
var tmp : TInteger;
begin
  {$IFDEF INSERTTEXTARRAY}
  System.Insert(Value,Self,Index);
  {$ELSE}

  tmp:=Count;
  Resize(tmp+1);

  Finalize(Self[tmp]);

  if Index<tmp then
     System.Move(Self[Index],Self[Index+1],(tmp-Index)*SizeOf(String));

  // Warning: Always prefix with System to not call our own Initialize method !
  System.Initialize(Self[Index]);

  Self[Index]:=Value;
  {$ENDIF}
end;

function TTextArrayHelper.ExistsBefore(const AIndex: TInteger): Boolean;
var t : TLoopInteger;
    tmp : String;
begin
  tmp:=Self[AIndex]; // <-- cached

  for t:=0 to AIndex-1 do
      if Self[t]=tmp then
         Exit(True);

  result:=False;
end;

function TTextArrayHelper.Map(const IgnoreCase:Boolean=False): TTextMap;
var tmp : TTextArray;
begin
  result:=TTextMap.Create;
  result.FIgnoreCase:=IgnoreCase;

  if Count>0 then
  begin
    result.FSorted:=GuessOrder;

    if result.Sorted<>Ascending then
    begin
      tmp:=Copy;
      tmp.Sort(True,IgnoreCase);
    end
    else
      tmp:=Self;

    //Median:=tmp[Count div 2];

    result.CreateMap(tmp);
  end;
end;

function TTextArrayHelper.MaxLength: Integer;
var t : TLoopInteger;
    tmp : Integer;
begin
  result:=0;

  for t:=0 to Count-1 do
  begin
    tmp:=Length(Self[t]);

    if tmp>result then
       result:=tmp;
  end;
end;

procedure TTextArrayHelper.Sort(const Ascending,IgnoreCase: Boolean; const Swap:TSwapProc);
var
  CompareFunc : TCompareProc; // Renamed from Compare to CompareFunc for clarity

const
  SortThreshold = 16; // Threshold for switching to InsertionSort

  procedure TextInsertionSort_WithSwap(L, R: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc; CurrentCompareFunc: TCompareProc);
  var i, j, currentIndexToInsert: TInteger;
  begin
    if IsAscending then
    begin
      for i := L + 1 to R do
      begin
        currentIndexToInsert := i;
        j := i - 1;
        while (j >= L) and (CurrentCompareFunc(Self[j], Self[currentIndexToInsert]) > 0) do
        begin
          CurrentSwap(j, currentIndexToInsert);
          currentIndexToInsert := j;
          Dec(j);
        end;
      end;
    end
    else // Descending
    begin
      for i := L + 1 to R do
      begin
        currentIndexToInsert := i;
        j := i - 1;
        while (j >= L) and (CurrentCompareFunc(Self[j], Self[currentIndexToInsert]) < 0) do
        begin
          CurrentSwap(j, currentIndexToInsert);
          currentIndexToInsert := j;
          Dec(j);
        end;
      end;
    end;
  end;

  procedure TextHeapify_WithSwap(CurrentIndex, CurrentCount, LBoundary: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc; CurrentCompareFunc: TCompareProc);
  var
    RootToHeapify, LeftChild, RightChild: TInteger;
  begin
    RootToHeapify := CurrentIndex;
    LeftChild := 2 * CurrentIndex + 1;
    RightChild := 2 * CurrentIndex + 2;

    if IsAscending then
    begin
      if (LeftChild < CurrentCount) and (CurrentCompareFunc(Self[LBoundary + LeftChild], Self[LBoundary + RootToHeapify]) > 0) then
        RootToHeapify := LeftChild;
      if (RightChild < CurrentCount) and (CurrentCompareFunc(Self[LBoundary + RightChild], Self[LBoundary + RootToHeapify]) > 0) then
        RootToHeapify := RightChild;
    end
    else // Descending
    begin
      if (LeftChild < CurrentCount) and (CurrentCompareFunc(Self[LBoundary + LeftChild], Self[LBoundary + RootToHeapify]) < 0) then
        RootToHeapify := LeftChild;
      if (RightChild < CurrentCount) and (CurrentCompareFunc(Self[LBoundary + RightChild], Self[LBoundary + RootToHeapify]) < 0) then
        RootToHeapify := RightChild;
    end;

    if RootToHeapify <> CurrentIndex then
    begin
      CurrentSwap(LBoundary + CurrentIndex, LBoundary + RootToHeapify);
      TextHeapify_WithSwap(RootToHeapify, CurrentCount, LBoundary, IsAscending, CurrentSwap, CurrentCompareFunc);
    end;
  end;

  procedure TextHeapSort_WithSwap(L, R: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc; CurrentCompareFunc: TCompareProc);
  var
    i, HeapSize: TInteger;
  begin
    HeapSize := R - L + 1;
    if HeapSize < 2 then Exit;

    for i := (HeapSize div 2) - 1 downto 0 do
      TextHeapify_WithSwap(i, HeapSize, L, IsAscending, CurrentSwap, CurrentCompareFunc);

    for i := HeapSize - 1 downto 1 do
    begin
      CurrentSwap(L, L + i);
      TextHeapify_WithSwap(0, i, L, IsAscending, CurrentSwap, CurrentCompareFunc);
    end;
  end;

  procedure PrivateSort(const l,r:TInteger; Depth: Integer; CurrentCompareFunc: TCompareProc);
  var
      pivotValue : String;
      i, j, pivotIndex : TInteger;
      N, MaxDepthAllowed : TInteger;
  begin
    N := r - l + 1;
    if N <= 1 then Exit;

    if N > 0 then
      MaxDepthAllowed := 2 * Trunc(System.Math.Log2(N))
    else
      MaxDepthAllowed := 0;

    if Depth > MaxDepthAllowed then
    begin
      TextHeapSort_WithSwap(l, r, Ascending, Swap, CurrentCompareFunc);
      Exit;
    end;

    if N <= SortThreshold then
    begin
      TextInsertionSort_WithSwap(l, r, Ascending, Swap, CurrentCompareFunc);
      Exit;
    end;

    i := l;
    j := r;
    pivotIndex := (l + j) shr 1;
    pivotValue := Self[pivotIndex]; // Pivot value is of type String

    while i <= j do
    begin
      if Ascending then
      begin
        while CurrentCompareFunc(Self[i], pivotValue) < 0 do Inc(i);
        while CurrentCompareFunc(Self[j], pivotValue) > 0 do Dec(j);
      end
      else // Descending
      begin
        while CurrentCompareFunc(Self[i], pivotValue) > 0 do Inc(i);
        while CurrentCompareFunc(Self[j], pivotValue) < 0 do Dec(j);
      end;

      if i <= j then
      begin
        if i <> j then
          Swap(i, j); // Use the main Sort's Swap parameter
        Inc(i);
        Dec(j);
      end;
    end;

    if l < j then
       PrivateSort(l, j, Depth + 1, CurrentCompareFunc);

    if i < r then
       PrivateSort(i, r, Depth + 1, CurrentCompareFunc);
  end;

begin
  if Count > 1 then
  begin
    if IgnoreCase then
       CompareFunc := CompareText // System.SysUtils.CompareText
    else
       CompareFunc := CompareStr; // System.SysUtils.CompareStr

    PrivateSort(0, Count - 1, 0, CompareFunc); // Pass CompareFunc
  end;
end;

procedure TTextArrayHelper.Sort(const Ascending:Boolean=True; const IgnoreCase:Boolean=False);
begin
  Sort(Ascending,IgnoreCase,Self.Swap);
end;

function TTextArrayHelper.SortedFind(const Value: String; out Exists:Boolean; const IgnoreCase:Boolean): TNativeInteger;
var
  L, H: TInteger;
  C : Integer;
  Compare : TCompareProc;
begin
  L := 0;
  H := High(Self);

  if IgnoreCase then
     Compare:=CompareText // case-insensitive
  else
     Compare:=CompareStr; // case-sensitive

  while L <= H do
  begin
    result:=(L + H) shr 1;

    C:=Compare(Self[result],Value);

    if C < 0 then
       L := Succ(result)
    else
    if C > 0 then
       H := Pred(result)
    else
    begin
      Exists:=True;
      Exit;
    end;
  end;

  result:=L;
  Exists:=False;
end;

function TTextArrayHelper.Stats: TTextStats;
begin
  result:=TTextStats.Create;

  // Should this method calculate the Min, Max etc for strings ? (unnecessary?)
end;

procedure TTextArrayHelper.Initialize(const Value:String);
var t : TLoopInteger;
begin
  for t:=0 to Count-1 do
      Self[t]:=Value;
end;

{ TDateTimeArrayHelper }

function TDateTimeArrayHelper.Copy(const Missing: TBooleanArray): TDateTimeArray;
var t : TLoopInteger;
    MissingCount : TInteger;
begin
  result:=nil;

  MissingCount:=Missing.Count;

  for t:=0 to Count-1 do
      if (MissingCount<=t) or (not Missing[t]) then
         result.Append(Self[t]);
end;

function TDateTimeArrayHelper.Count: TInteger;
begin
  result:=Length(Self);
end;

function TDateTimeArrayHelper.Copy(const AIndex,ACount:TInteger): TDateTimeArray;
begin
  result:=System.Copy(Self,AIndex,ACount);
end;

function TDateTimeArrayHelper.Copy: TDateTimeArray;
begin
  result:=Copy(0,Count);
end;

procedure TDateTimeArrayHelper.Resize(const ACount: TInteger);
begin
  SetLength(Self,ACount);
end;

procedure TDateTimeArrayHelper.Reverse;
begin
  TArrayReverse<TDateTime>.Reverse(Self);
end;

procedure TDateTimeArrayHelper.Empty;
begin
  Resize(0);
end;

function TDateTimeArrayHelper.Append(const Value: TDateTime):TInteger;
begin
  result:=Count;
  Resize(result+1);
  Self[result]:=Value;
end;

procedure TDateTimeArrayHelper.Append(const Value: TDateTimeArray);
var tmp : TInteger;
begin
  if Value<>nil then
  begin
    tmp:=Count;
    Resize(tmp+Value.Count);
    Move(Value[Low(Value)],Self[tmp],Value.Count*SizeOf(TDateTime));
  end;
end;

procedure TDateTimeArrayHelper.Delete(const Index:TInteger; const ACount:TInteger=1);
{$IFNDEF DELETEARRAY}
var tmp : TInteger;
{$ENDIF}
begin
  {$IFDEF DELETEARRAY}
  System.Delete(Self,Index,ACount);
  {$ELSE}
  tmp:=Count-ACount;

  if tmp-Index>0 then
     System.Move(Self[Index+ACount],Self[Index],SizeOf(TDateTime)*(tmp-Index));

  Resize(tmp);
  {$ENDIF}
end;

function TDateTimeArrayHelper.Distribution(const Mean: TDateTime;
  const StdDeviation: Double; const Exponent: Integer): Double;

  function SumOfDifferences:Double;
  var t : TLoopInteger;
  begin
    result:=0;

    for t:=0 to Count-1 do
        result:=result+IntPower((Self[t]-Mean),Exponent);
  end;

begin
  result:=(Count-1)*IntPower(StdDeviation,Exponent);

  if result<>0 then
     result:=SumOfDifferences/result;
end;

function TDateTimeArrayHelper.ExistsBefore(const AIndex: TInteger): Boolean;
var t : TLoopInteger;
begin
  for t:=0 to AIndex-1 do
      if Self[t]=Self[AIndex] then
         Exit(True);

  result:=False;
end;

function TDateTimeArrayHelper.GuessOrder:TDataOrder;

  function GuessStart(out Order:TDataOrder):TInteger;
  begin
    Order:=None;
    result:=1;

    repeat
      if Self[result]>Self[result-1] then
      begin
        Order:=Ascending;
        break;
      end
      else
      if Self[result]<Self[result-1] then
      begin
        Order:=Descending;
        break;
      end
      else
         Inc(result);

    until result>=Count;
  end;

var n,
    c,
    t : TInteger;
begin
  result:=None;

  c:=Count;

  if c=1 then
     result:=Ascending
  else
  if c>1 then
  begin
    n:=GuessStart(result);

    t:=n+1;

    if result=Ascending then
    begin
      while t<c do
      begin
        if Self[t]<Self[t-1] then
           Exit(None);

        Inc(t);
      end;
    end
    else
    if result=Descending then
      while t<c do
      begin
        if Self[t]>Self[t-1] then
           Exit(None);

        Inc(t);
      end;
  end;
end;

procedure TDateTimeArrayHelper.Insert(const Index: TInteger; const Value: TDateTime);
var tmp : TInteger;
begin
  tmp:=Count;
  Resize(tmp+1);

  if Index<tmp then
     System.Move(Self[Index],Self[Index+1],(tmp-Index)*SizeOf(TDateTime));

  Self[Index]:=Value;
end;

function TDateTimeArrayHelper.Map(out Median,Mode: TDateTime): TDateTimeMap;
var tmp : TDateTimeArray;
    tmpMax : TInteger;
begin
  result:=TDateTimeMap.Create;

  if Count>0 then
  begin
    result.FSorted:=GuessOrder;

    if result.Sorted<>Ascending then
    begin
      tmp:=Copy;
      tmp.Sort;
    end
    else
      tmp:=Self;

    Median:=tmp[Count div 2];

    result.CreateMap(tmp);
  end;

  tmpMax:=result.Map.IndexOfMax;

  if tmpMax=-1 then
     Mode:=0
  else
     Mode:=result.Items[tmpMax];
end;

function TDateTimeArrayHelper.Maximum: TDateTime;
var n : TInteger;
    t : TLoopInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
  begin
    result:=Self[0];

    for t:=1 to n-1 do
        if Self[t]>result then
           result:=Self[t];
  end;
end;

function TDateTimeArrayHelper.Mean: TDateTime;
var n : TInteger;
    t : TLoopInteger;
begin
  result:=0;

  n:=Count;

  if n>0 then
  begin
    for t:=0 to Count-1 do
        result:=result+Self[t];

    result:=result/n;
  end;
end;

function TDateTimeArrayHelper.Minimum: TDateTime;
var n : TInteger;
    t : TLoopInteger;
begin
  n:=Count;

  if n=0 then
     result:=0
  else
  begin
    result:=Self[0];

    for t:=1 to n-1 do
        if Self[t]<result then
           result:=Self[t];
  end;
end;

procedure TDateTimeArrayHelper.Sort(const Ascending:Boolean; const Swap:TSwapProc);

const
  SortThreshold = 16;

  procedure DateTimeInsertionSort_WithSwap(L, R: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var i, j, currentIndexToInsert: TInteger;
  begin
    if IsAscending then
    begin
      for i := L + 1 to R do
      begin
        currentIndexToInsert := i;
        j := i - 1;
        while (j >= L) and (Self[j] > Self[currentIndexToInsert]) do
        begin
          CurrentSwap(j, currentIndexToInsert);
          currentIndexToInsert := j;
          Dec(j);
        end;
      end;
    end
    else // Descending
    begin
      for i := L + 1 to R do
      begin
        currentIndexToInsert := i;
        j := i - 1;
        while (j >= L) and (Self[j] < Self[currentIndexToInsert]) do
        begin
          CurrentSwap(j, currentIndexToInsert);
          currentIndexToInsert := j;
          Dec(j);
        end;
      end;
    end;
  end;

  procedure DateTimeHeapify_WithSwap(CurrentIndex, CurrentCount, LBoundary: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var
    RootToHeapify, LeftChild, RightChild: TInteger;
  begin
    RootToHeapify := CurrentIndex;
    LeftChild := 2 * CurrentIndex + 1;
    RightChild := 2 * CurrentIndex + 2;

    if IsAscending then
    begin
      if (LeftChild < CurrentCount) and (Self[LBoundary + LeftChild] > Self[LBoundary + RootToHeapify]) then
        RootToHeapify := LeftChild;
      if (RightChild < CurrentCount) and (Self[LBoundary + RightChild] > Self[LBoundary + RootToHeapify]) then
        RootToHeapify := RightChild;
    end
    else // Descending
    begin
      if (LeftChild < CurrentCount) and (Self[LBoundary + LeftChild] < Self[LBoundary + RootToHeapify]) then
        RootToHeapify := LeftChild;
      if (RightChild < CurrentCount) and (Self[LBoundary + RightChild] < Self[LBoundary + RootToHeapify]) then
        RootToHeapify := RightChild;
    end;

    if RootToHeapify <> CurrentIndex then
    begin
      CurrentSwap(LBoundary + CurrentIndex, LBoundary + RootToHeapify);
      DateTimeHeapify_WithSwap(RootToHeapify, CurrentCount, LBoundary, IsAscending, CurrentSwap);
    end;
  end;

  procedure DateTimeHeapSort_WithSwap(L, R: TInteger; IsAscending: Boolean; const CurrentSwap: TSwapProc);
  var
    i, HeapSize: TInteger;
  begin
    HeapSize := R - L + 1;
    if HeapSize < 2 then Exit;

    for i := (HeapSize div 2) - 1 downto 0 do
      DateTimeHeapify_WithSwap(i, HeapSize, L, IsAscending, CurrentSwap);

    for i := HeapSize - 1 downto 1 do
    begin
      CurrentSwap(L, L + i);
      DateTimeHeapify_WithSwap(0, i, L, IsAscending, CurrentSwap);
    end;
  end;

  procedure PrivateSort(const l,r:TInteger; Depth: Integer);
  var
      pivotValue : TDateTime;
      i, j, pivotIndex : TInteger;
      N, MaxDepthAllowed : TInteger;
  begin
    N := r - l + 1;
    if N <= 1 then Exit;

    if N > 0 then
      MaxDepthAllowed := 2 * Trunc(System.Math.Log2(N))
    else
      MaxDepthAllowed := 0;

    if Depth > MaxDepthAllowed then
    begin
      DateTimeHeapSort_WithSwap(l, r, Ascending, Swap);
      Exit;
    end;

    if N <= SortThreshold then
    begin
      DateTimeInsertionSort_WithSwap(l, r, Ascending, Swap);
      Exit;
    end;

    i := l;
    j := r;
    pivotIndex := (l + j) shr 1;
    pivotValue := Self[pivotIndex];

    while i <= j do
    begin
      if Ascending then
      begin
        while Self[i] < pivotValue do Inc(i);
        while Self[j] > pivotValue do Dec(j);
      end
      else // Descending
      begin
        while Self[i] > pivotValue do Inc(i);
        while Self[j] < pivotValue do Dec(j);
      end;

      if i <= j then
      begin
        if i <> j then
          Swap(i, j);
        Inc(i);
        Dec(j);
      end;
    end;

    if l < j then
       PrivateSort(l, j, Depth + 1);

    if i < r then
       PrivateSort(i, r, Depth + 1);
  end;

begin
  if Count>1 then
     PrivateSort(0,Count-1, 0);
end;

procedure TDateTimeArrayHelper.Sort(const Ascending: Boolean);
begin
  Sort(Ascending,Swap);
end;

function TDateTimeArrayHelper.SortedFind(const Value: TDateTime; out Exists:Boolean): TNativeInteger;
var
  L, H : TInteger;
begin
  L := 0;
  H := High(Self);

  while L <= H do
  begin
    result := (L + H) shr 1;

    if Self[result]<Value then
       L := Succ(result)
    else
    if Self[result]=Value then
    begin
      Exists:=True;
      Exit;
    end
    else
       H := Pred(result);
  end;

  result:=L;
  Exists:=False;
end;

function TDateTimeArrayHelper.Stats: TDateTimeStats;
var tmpSum : Double;
    t : TLoopInteger;
    tmp : TDateTime;
begin
  result:=TDateTimeStats.Create;

  if Count>0 then
  begin
    tmp:=Self[0];

    result.Min:=tmp;
    result.Max:=tmp;

    tmpSum:=tmp;

    for t:=1 to Count-1 do
    begin
      if Self[t]<result.Min then
         result.Min:=Self[t]
      else
      if Self[t]>result.Max then
         result.Max:=Self[t];

      tmpSum:=tmpSum+Self[t];
    end;

    result.Range:=result.Max-result.Min;
    result.Mean:=tmpSum/Count;
    result.Sum:=tmpSum;

    result.Variance:=Variance(result.Mean);
    result.StdDeviation:=Sqrt(result.Variance);

    result.Skewness:=Distribution(result.Mean,result.StdDeviation, 3);
    result.Peakedness:=Distribution(result.Mean,result.StdDeviation, 4);
  end;
end;

function TDateTimeArrayHelper.StdDeviation(const Mean: TDateTime): Double;
begin
  result:=Sqrt(Variance(Mean));
end;

procedure TDateTimeArrayHelper.Swap(const A, B: TInteger);
var tmp : TDateTime;
begin
  tmp:=Self[A];
  Self[A]:=Self[B];
  Self[B]:=tmp;
end;

function TDateTimeArrayHelper.Variance(const Mean: TDateTime): Double;
var t : TLoopInteger;
    n : TInteger;
begin
  result:=0;

  n:=Count;

  if n>1 then
  begin
    for t:=0 to n-1 do
        result:=result+Sqr(Self[t]-Mean);

    result:=result/Pred(n);
  end;
end;

procedure TDateTimeArrayHelper.Initialize(const Value:TDateTime);
var t : TLoopInteger;
begin
  for t:=0 to Count-1 do
      Self[t]:=Value;
end;

{ TInt32Map }

function TInt32Map.AsString(const Index:TInteger):String;
begin
  result:=IntToStr(Items[Index]);
end;

procedure TInt32Map.Clear;
begin
  inherited;
  Items:=nil;
end;

procedure TInt32Map.CreateMap(const AItems: TInt32Array);
var tmp : TInteger;
    t : TLoopInteger;
    n : TInteger;
begin
  tmp:=AItems.Count;

  Items.Resize(tmp);
  Map.Resize(tmp);

  n:=0;
  t:=0;

  while t<tmp do
  begin
    Items[n]:=AItems[t];
    Map[n]:=1;

    Inc(t);
    while t<tmp do
    begin
      if AItems[t]=Items[n] then
      begin
        Inc(Map[n]);
        Inc(t);
      end
      else
        break;
    end;

    Inc(n);
  end;

  if n<>tmp then
  begin
    Items.Resize(n);
    Map.Resize(n);
  end;

  CachedCount:=Count;
end;

function TInt32Map.Find(const Value: Integer; out Exists:Boolean):TNativeInteger;
begin
  result:=Items.SortedFind(Value,Exists);
end;

function TInt32Map.Find(const Value:Integer; out AIndex:TNativeInteger):Boolean;
begin
  AIndex:=Find(Value,result);
end;

procedure TInt32Map.AddMap(const Value: Integer); // below Find due to inline
var tmp : TNativeInteger;
begin
  if Find(Value,tmp) then
     Inc(Map[tmp])
  else
  begin
    Items.Insert(tmp,Value);
    Map.Insert(tmp,1);
  end;
end;

function TInt32Map.GetItem(const Index: TInteger): Integer;
begin
  result:=Items[Index];
end;

{ TInt64Map }

procedure TInt64Map.AddMap(const Value: Int64);
var tmp : TNativeInteger;
begin
  if Find(Value,tmp) then
     Inc(Map[tmp])
  else
  begin
    Items.Insert(tmp,Value);
    Map.Insert(tmp,1);
  end;
end;

function TInt64Map.AsString(const Index:TInteger):String;
begin
  result:=IntToStr(Items[Index]);
end;

procedure TInt64Map.Clear;
begin
  inherited;
  Items:=nil;
end;

function TInt64Map.Find(const Value: Int64; out Exists:Boolean):TNativeInteger;
begin
  result:=Items.SortedFind(Value,Exists);
end;

function TInt64Map.Find(const Value:Int64; out Index: TNativeInteger):Boolean;
begin
  Index:=Find(Value,result);
end;

function TInt64Map.GetItem(const Index: TInteger): Int64;
begin
  result:=Items[Index];
end;

procedure TInt64Map.CreateMap(const AItems:TInt64Array);
var tmp : TInteger;
    t : TLoopInteger;
    n : TInteger;
begin
  tmp:=AItems.Count;

  Items.Resize(tmp);
  Map.Resize(tmp);

  n:=0;
  t:=0;
  while t<tmp do
  begin
    Items[n]:=AItems[t];
    Map[n]:=1;

    Inc(t);
    while t<tmp do
    begin
      if AItems[t]=Items[n] then
      begin
        Inc(Map[n]);
        Inc(t);
      end
      else
        break;
    end;

    Inc(n);
  end;

  if n<>tmp then
  begin
    Items.Resize(n);
    Map.Resize(n);
  end;

  CachedCount:=Count;
end;

{ TDataMap }

function TDataMap.Count: TInteger;
begin
  result:=Map.Count;
end;

procedure TDataMap.Clear;
begin
  CachedCount:=0;
  Map.Empty;
  FSorted:=None;
end;

{ TSingleMap }

procedure TSingleMap.AddMap(const Value: Single);
var tmp : TNativeInteger;
begin
  if Find(Value,tmp) then
     Inc(Map[tmp])
  else
  begin
    Items.Insert(tmp,Value);
    Map.Insert(tmp,1);
  end;
end;

function TSingleMap.AsString(const Index: TInteger): String;
begin
  result:=FloatToStr(Items[Index]);
end;

procedure TSingleMap.Clear;
begin
  inherited;
  Items:=nil;
end;

function TSingleMap.Find(const Value: Single; out Exists:Boolean): TNativeInteger;
begin
  result:=Items.SortedFind(Value,Exists);
end;

function TSingleMap.Find(const Value: Single; out Index: TNativeInteger): Boolean;
begin
  result:=Find(Value,Index);
end;

procedure TSingleMap.CreateMap(const AItems:TSingleArray);
var tmp : TInteger;
    t : TLoopInteger;
    n : TInteger;
begin
  tmp:=AItems.Count;

  Items.Resize(tmp);
  Map.Resize(tmp);

  n:=0;
  t:=0;

  while t<tmp do
  begin
    Items[n]:=AItems[t];
    Map[n]:=1;

    Inc(t);
    while t<tmp do
    begin
      if AItems[t]=Items[n] then
      begin
        Inc(Map[n]);
        Inc(t);
      end
      else
        break;
    end;

    Inc(n);
  end;

  if n<>tmp then
  begin
    Items.Resize(n);
    Map.Resize(n);
  end;

  CachedCount:=Count;
end;

function TSingleMap.GetItem(const Index: TInteger): Single;
begin
  result:=Items[Index];
end;

{ TDoubleMap }

procedure TDoubleMap.AddMap(const Value: Double);
var tmp : TNativeInteger;
begin
  if Find(Value,tmp) then
     Inc(Map[tmp])
  else
  begin
    Items.Insert(tmp,Value);
    Map.Insert(tmp,1);
  end;
end;

function TDoubleMap.AsString(const Index: TInteger): String;
begin
  result:=FloatToStr(Items[Index]);
end;

procedure TDoubleMap.Clear;
begin
  inherited;
  Items:=nil;
end;

function TDoubleMap.Find(const Value:Double; out Exists:Boolean):TNativeInteger;
begin
  result:=Items.SortedFind(Value,Exists);
end;

function TDoubleMap.Find(const Value:Double; out Index:TNativeInteger):Boolean;
begin
  Index:=Find(Value,result);
end;

function TDoubleMap.GetItem(const Index: TInteger): Double;
begin
  result:=Items[Index];
end;

procedure TDoubleMap.CreateMap(const AItems:TDoubleArray);
var tmp : TInteger;
    t : TLoopInteger;
    n : TInteger;
begin
  tmp:=AItems.Count;

  Items.Resize(tmp);
  Map.Resize(tmp);

  n:=0;
  t:=0;

  while t<tmp do
  begin
    Items[n]:=AItems[t];
    Map[n]:=1;

    Inc(t);
    while t<tmp do
    begin
      if AItems[t]=Items[n] then
      begin
        Inc(Map[n]);
        Inc(t);
      end
      else
        break;
    end;

    Inc(n);
  end;

  if n<>tmp then
  begin
    Items.Resize(n);
    Map.Resize(n);
  end;

  CachedCount:=Count;
end;

{$IFDEF CPUX86}

{ TExtendedMap }

procedure TExtendedMap.AddMap(const Value: Extended);
var tmp : TNativeInteger;
begin
  if Find(Value,tmp) then
     Inc(Map[tmp])
  else
  begin
    Items.Insert(tmp,Value);
    Map.Insert(tmp,1);
  end;
end;

function TExtendedMap.AsString(const Index:TInteger):String;
begin
  result:=FloatToStr(Items[Index]);
end;

procedure TExtendedMap.Clear;
begin
  inherited;
  Items:=nil;
end;

function TExtendedMap.Find(const Value:Extended; out Exists:Boolean):TNativeInteger;
begin
  result:=Items.SortedFind(Value,Exists);
end;

function TExtendedMap.Find(const Value:Extended; out Index:TNativeInteger):Boolean;
begin
  Index:=Find(Value,result);
end;

function TExtendedMap.GetItem(const Index: TInteger): Extended;
begin
  result:=Items[Index];
end;

procedure TExtendedMap.CreateMap(const AItems:TExtendedArray);
var tmp : TInteger;
    t : TLoopInteger;
    n : TInteger;
begin
  tmp:=AItems.Count;

  Items.Resize(tmp);
  Map.Resize(tmp);

  // Pending: consider Missing (remove from result.Items and result.Map)
  n:=0;
  t:=0;
  while t<tmp do
  begin
    Items[n]:=AItems[t];
    Map[n]:=1;

    Inc(t);
    while t<tmp do
    begin
      if AItems[t]=Items[n] then
      begin
        Inc(Map[n]);
        Inc(t);
      end
      else
        break;
    end;

    Inc(n);
  end;

  if n<>tmp then
  begin
    Items.Resize(n);
    Map.Resize(n);
  end;

  CachedCount:=Count;
end;

{$ENDIF}

{ TDataOrderHelper }

function TDataOrderHelper.ToString: String;
begin
  case Self of
         None: result:='No';
    Ascending: result:='Ascending';
  else
               result:='Descending';
  end;
end;

{ TDateTimeMap }

procedure TDateTimeMap.AddMap(const Value: TDateTime);
var tmp : TNativeInteger;
begin
  if Find(Value,tmp) then
     Inc(Map[tmp])
  else
  begin
    Items.Insert(tmp,Value);
    Map.Insert(tmp,1);
  end;
end;

function TDateTimeMap.AsString(const Index: TInteger): String;
begin
  result:=DateTimeToStr(Items[Index]);
end;

procedure TDateTimeMap.Clear;
begin
  inherited;
  Items:=nil;
end;

function TDateTimeMap.Find(const Value:TDateTime; out Exists:Boolean):TNativeInteger;
begin
  result:=Items.SortedFind(Value,Exists);
end;

function TDateTimeMap.Find(const Value:TDateTime; out Index: TNativeInteger):Boolean;
begin
  Index:=Find(Value,result);
end;

function TDateTimeMap.GetItem(const Index: TInteger): TDateTime;
begin
  result:=Items[Index];
end;

procedure TDateTimeMap.CreateMap(const AItems:TDateTimeArray);
var tmp : TInteger;
    t : TLoopInteger;
    n : TInteger;
begin
  tmp:=AItems.Count;

  Items.Resize(tmp);
  Map.Resize(tmp);

  n:=0;
  t:=0;
  while t<tmp do
  begin
    Items[n]:=AItems[t];
    Map[n]:=1;

    Inc(t);
    while t<tmp do
    begin
      if AItems[t]=Items[n] then
      begin
        Inc(Map[n]);
        Inc(t);
      end
      else
        break;
    end;

    Inc(n);
  end;

  if n<>tmp then
  begin
    Items.Resize(n);
    Map.Resize(n);
  end;

  CachedCount:=Count;
end;

{ TBooleanMap }

function TBooleanMap.AsString(const Index: TInteger): String;
begin
  result:=BoolToStr(Index>0,True);
end;

{ TTextMap }

procedure TTextMap.AddMap(const Value: String);
var tmp : TNativeInteger;
begin
  if Find(Value,tmp) then
     Inc(Map[tmp])
  else
  begin
    Items.Insert(tmp,Value);

    (*
    Items.Resize(Items.Count+1);

    if tmp<High(Items) then
       System.Move(Items[tmp],Items[tmp+1],(High(Items)-tmp)*SizeOf(String));

    // UniqueString ?
    Pointer(Items[tmp]):=nil; // AV

    Items[tmp]:=Value;
    *)

    Map.Insert(tmp,1);
  end;
end;

function TTextMap.AsString(const Index: TInteger): String;
begin
  result:=Items[Index];
end;

procedure TTextMap.Clear;
begin
  inherited;
  Items:=nil;
end;

function TTextMap.Find(const Value:String; out Exists:Boolean):TNativeInteger;
begin
  result:=Items.SortedFind(Value,Exists,IgnoreCase);
end;

function TTextMap.Find(const Value: String; out Index: TNativeInteger): Boolean;
begin
  Index:=Find(Value,result);
end;

function TTextMap.GetItem(const Index: TInteger): String;
begin
  result:=Items[Index];
end;

procedure TTextMap.CreateMap(const AItems:TTextArray);
var tmp : TInteger;
    t : TLoopInteger;
    n : TInteger;
begin
  tmp:=AItems.Count;

  Items.Resize(tmp);
  Map.Resize(tmp);

  n:=0;
  t:=0;

  while t<tmp do
  begin
    Items[n]:=AItems[t];
    Map[n]:=1;

    Inc(t);
    while t<tmp do
    begin
      if AItems[t]=Items[n] then
      begin
        Inc(Map[n]);
        Inc(t);
      end
      else
        break;
    end;

    Inc(n);
  end;

  if n<>tmp then
  begin
    Items.Resize(n);
    Map.Resize(n);
  end;

  CachedCount:=Count;
end;

{ TInt64Stats }

procedure TInt64Stats.CalcStats(const Data: TInt64Array);

{$IFDEF CPUX64}
{$DEFINE TEMPINT64} // Faster in x64, slower in 32bit
{$ENDIF}

var t : TLoopInteger;
    tmpSum : Double;

    {$IFDEF TEMPINT64}
    tmp : Int64;
    {$ENDIF}
begin
  Min:=Data[0];
  Max:=Data[0];

  tmpSum:=Data[0];

  for t:=1 to Data.Count-1 do
  begin
    {$IFDEF TEMPINT64}
    tmp:=Data[t];

    if tmp<Min then
       Min:=tmp
    else
    if tmp>Max then
       Max:=tmp;

    tmpSum:=tmpSum+tmp;
    {$ELSE}
    if Data[t]<Min then
       Min:=Data[t]
    else
    if Data[t]>Max then
       Max:=Data[t];

    tmpSum:=tmpSum+Data[t];
    {$ENDIF}
  end;

  Mean:=tmpSum/Data.Count;
  Sum:=tmpSum;

  // Promote expression to float to avoid possible overflow
  Range:=(1.0*Max)-Min;
end;

{ TInt32Stats }

procedure TInt32Stats.CalcStats(const Data: TInt32Array);

{$DEFINE TEMPINT32} // Faster in x64, slower in 32bit

var t : TLoopInteger;
    tmpSum : Double;

    {$IFDEF TEMPINT32}
    tmp : Int32;
    {$ENDIF}
begin
  Min:=Data[0];
  Max:=Data[0];

  tmpSum:=Data[0];

  // SSE sum:
  // is array 16 aligned?
  // loop step 32:
  //  v0 := mk_128(Self[t])    mm_add_ps(v0, Self[t+4])
  //  v1 := mk_128(Self[t+8])  mm_add_ps(v0, Self[t+12])
  //  v2 := mk_128(Self[t+16]) mm_add_ps(v0, Self[t+20])
  //  v3 := mk_128(Self[t+24]) mm_add_ps(v0, Self[t+28])
  //
  //  v0 := mm_add_ps(v0,v1)
  //  v2 := mm_add_ps(v2,v3)
  //  v0 := mm_add_ps(v0,v2)
  //  mm_store_ps(v,v0);
  //  result:=result+v[0]+v[1]+v[2]+v[3]

  {$IFDEF TEMPINT32}
  for t:=1 to Data.Count-1 do
  begin
    tmp:=Data[t];

    if tmp<Min then
       Min:=tmp
    else
    if tmp>Max then
       Max:=tmp;

    tmpSum:=tmpSum+tmp;
  end;
  {$ELSE}
  for t:=1 to Data.Count-1 do
  begin
    if Data[t]<Min then
       Min:=Data[t]
    else
    if Data[t]>Max then
       Max:=Data[t];

    tmpSum:=tmpSum+Data[t];
  end;
  {$ENDIF}

  Mean:=tmpSum/Data.Count;
  Sum:=tmpSum;

  // Promote expression to Int64 to avoid possible overflow
  Range:=Int64(Max)-Int64(Min);
end;

end.

