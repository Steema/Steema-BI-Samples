{*********************************************}
{  TeeBI Software Library                     }
{  Expression parser and evaluator            }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Expression;

{$SCOPEDENUMS ON}

interface

{.$DEFINE BIVARIANT} // Experimental, replacing Variant with a faster alternative

uses
  {$IFDEF BIVARIANT}
  BI.Variants,
  {$ELSE}
  System.Variants,
  {$ENDIF}
  System.SysUtils;

type
  EExpressionParse=class(Exception)
  public
    Position : Integer;

    Constructor Create(APos:Integer; const AMessage:String);
  end;

  TData={$IFDEF BIVARIANT}BIVariant{$ELSE}Variant{$ENDIF};

  TExpression=class;

  TResolveProc={$IFNDEF FPC}reference to{$ENDIF} function(const S:String; IsFunction:Boolean):TExpression;
  TErrorProc={$IFNDEF FPC}reference to{$ENDIF} function(const APos:Integer; const AMessage:String):Boolean;
  TExpressionProc=procedure(const Item:TExpression) of object;

  TExpression=class abstract
  public
    class var
      Null:TData;

    Constructor Create; overload; virtual;

    procedure Assign(const Source:TExpression); virtual; abstract;

    class function Clone(const AExpression:TExpression):TExpression; static;

    class function Evaluate(const S:String):TData; static;
    class function FromString(const S:String):TExpression; overload; static;
    class function FromString(const S:String;
                              const Resolve:TResolveProc):TExpression; overload; static;
    class function FromString(const S:String;
                              const Resolve:TResolveProc;
                              const Error:TErrorProc):TExpression; overload; static;

    procedure Traverse(const AProc:TExpressionProc); virtual;
    function Value:TData; virtual; abstract;
  end;

  TIntegerExpression=class(TExpression)
  public
    Number : Int64;

    Constructor Create(const AValue:Int64);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TFloatExpression=class(TExpression)
  public
    Number : Extended;

    Constructor Create(const AValue:Extended);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;

    class function E:TFloatExpression; static;
    class function Pi:TFloatExpression; static;
  end;

  TBooleanExpression=class(TExpression)
  public
    Logical : Boolean;

    Constructor Create(const AValue:Boolean);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TDateTimeExpression=class(TExpression)
  public
    DateTime : TDateTime;

    Constructor Create(const AValue:TDateTime);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;

    class function Date:TDateTimeExpression; static;
    class function Now:TDateTimeExpression; static;
    class function Time:TDateTimeExpression; static;

    class function FromData(const Value:TData):TDateTime; static;
  end;

  TTextExpression=class(TExpression)
  public
    Text : String;

    Constructor Create(const AValue:String);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TExpressions=Array of TExpression;

  TArrayExpression=class(TExpression)
  public
    Items : TExpressions;

    Constructor Create(const AValues:Array of TExpression); overload;
    Constructor Create(const AValues:Array of TData); overload;
    Destructor Destroy; override;

    procedure Add(const AValue:TExpression);
    procedure Assign(const Source:TExpression); override;
    procedure Clear;

    function Value:TData; override;
    function ToString:String; override;
    procedure Traverse(const AProc:TExpressionProc); override;
  end;

  TOperandExpression=class abstract (TExpression)
  protected
    function Part(const AExpression:TExpression):String;
  public
    Left  : TExpression;
    Right : TExpression;

    Destructor Destroy; override;

    procedure Assign(const Source:TExpression); override;
    procedure Traverse(const AProc:TExpressionProc); override;
  end;

  TArithmeticOperand=(Add,Subtract,Multiply,Divide,&Mod,Power);

  TArithmeticOperandHelper=record helper for TArithmeticOperand
  public
    class function FromString(const S:String; out Operand:TArithmeticOperand):Boolean; static;
    function ToString:String;
  end;

  TArithmeticExpression=class(TOperandExpression)
  private
     // do not inline
    function Add: TData;
    function Subtract: TData;
    function Multiply: TData;
    function Divide: TData;
    function Modulus: TData;
    function Power: TData;
  public
    Operand : TArithmeticOperand;

    Constructor Create(const ALeft:TExpression; const AOperand:TArithmeticOperand; const ARight:TExpression);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TBaseLogicalExpression=class(TOperandExpression)
  end;

  TLogicalOperand=(Equal,NotEqual,Greater,Lower,GreaterOrEqual,LowerOrEqual,&And,&Or,&In);

  TLogicalOperandHelper=record helper for TLogicalOperand
  public
    class function FromString(const S:String; out Operand:TLogicalOperand):Boolean; static;
    function ToString:String;
  end;

  TLogicalExpression=class(TBaseLogicalExpression)
  private
    function CalcAnd:Boolean; inline;
    function CalcOr:Boolean; inline;
    function IsEqual:Boolean; inline;
    function IsGreater:Boolean; inline;
    function IsGreaterOrEqual:Boolean; inline;
    function IsLower:Boolean; inline;
    function IsLowerOrEqual:Boolean; inline;
    function IsNotEqual:Boolean; inline;
    function LeftInRight:Boolean;

    type
      TBooleanFunc=function:Boolean of object;

    var
      Funcs:Array[TLogicalOperand] of TBooleanFunc;
  public
    Operand : TLogicalOperand;

    Constructor Create; override;
    Constructor Create(const ALeft:TExpression; const AOperand:TLogicalOperand;
                       const ARight:TExpression); overload;

    procedure Assign(const Source: TExpression); override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TUnaryExpression=class abstract(TExpression)
  public
    Expression : TExpression;

    Constructor Create(const AExpression:TExpression);
    Destructor Destroy; override;

    procedure Assign(const Source:TExpression); override;
    procedure Traverse(const AProc:TExpressionProc); override;
  end;

  TUnaryNotExpression=class(TUnaryExpression)
  public
    function Value:TData; override;
    function ToString:String; override;
  end;

  TUnaryDateTimeExpression=class(TUnaryExpression);

  TDateExpression=class(TUnaryDateTimeExpression)
  public
    class function FromData(const Value:TData):TDateTime; static;
    function ToString:String; override;
    function Value:TData; override;
  end;

  TTimeExpression=class(TUnaryDateTimeExpression)
  public
    class function FromData(const Value:TData):TDateTime; static;
    function ToString:String; override;
    function Value:TData; override;
  end;

  TMathOperand=(Sin,Cos,Tan,Sqr,Sqrt,Log,Ln,Exp,Round,Trunc);

  TMathOperandHelper=record helper for TMathOperand
  public
    class function FromString(const S:String; out Operand:TMathOperand):Boolean; static;
    function ToString:String;
  end;

  TMathExpression=class(TUnaryExpression)
  public
    Operand : TMathOperand;

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TDateTimeSpan=(None,
                 //Nanosecond,
                 //Microsecond,
                 Millisecond,
                 HundredsOfSecond,
                 TenthsOfSecond,
                 Second,
                 Minute,
                 QuarterHour,
                 Hour,
                 Day,
                 Week,
                 Month,
                 Quarter,
                 Year,
                 Decade,
                 Century,
                 Millennium);

  TDateTimePart=(None,
                 //Nanosecond,
                 //Microsecond,
                 Millisecond,
                 HundredsOfSecond,
                 TenthsOfSecond,
                 Second,
                 Minute,
                 QuarterHour,
                 Hour,
                 DayOfMonth,
                 DayOfYear,
                 WeekOfYear,
                 WeekDay,
                 ShortWeekDayName,
                 LongWeekDayName,
                 Month,
                 ShortMonthName,
                 LongMonthName,
                 Quarter,
                 Year,
                 Decade,
                 DecadeOfYear,
                 Century,
                 Millennium);

  TDateTimePartHelper=record helper for TDateTimePart
  public
    class
      var QuarterFormat:String;

    class function AllToText:String; static;
    class function Max:Integer; static;
    function AsString(const Index:Integer):String;
    function ToString:String; overload;
    class function ToString(const Index:Integer):String; overload; inline; static;
  end;

  TDateTimePartExpression=class(TUnaryExpression)
  private
    function AsInteger(const AValue:TDateTime):Integer;
  public
    Part : TDateTimePart;

    procedure Assign(const Source:TExpression); override;
    class function FromString(const S:String; out APart:TDateTimePart):Boolean; static;

    function Value:TData; override;
    function ToString:String; override;
  end;

  TTextUnaryOperand=(Lower,Upper,IsEmpty,Length,Trim);

  TTextUnaryOperandHelper=record helper for TTextUnaryOperand
  public
    class function FromString(const S:String; out Operand:TTextUnaryOperand):Boolean; static;
    function ToString:String;
  end;

  TTextUnaryExpression=class(TUnaryExpression)
  public
    Operand : TTextUnaryOperand;

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TTextLogicalOperand=(Starts,Ends,Contains);

  TTextLogicalOperandHelper=record helper for TTextLogicalOperand
  public
    class function FromString(const S:String; out Operand:TTextLogicalOperand):Boolean; static;
    function ToString:String;
  end;

  TTextLogicalExpression=class(TBaseLogicalExpression)
  public
    Operand : TTextLogicalOperand;

    Constructor Create(const ALeft:TExpression; const AOperand:TTextLogicalOperand; const ARight:TExpression);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  (*
  // Pending: Supoprt for multiple parameters in expression calls:  Foo(a,b,c...)
  TTextOperand=(IndexOf,Pad,Split,Insert,Remove,Replace,CountChars,Format,SubString);

  TTextOperandExpression=class(TExpression)
  public
    Operand : TTextOperand;

    Constructor Create(const AOperand:TTextOperand);

    function Value:TData; override;
    function ToString:String; override;
  end;
  *)

implementation
