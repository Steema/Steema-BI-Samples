{*********************************************}
{  TeeBI Software Library                     }
{  Expression parser and evaluator            }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Expression;

{$SCOPEDENUMS ON}

interface

uses
  System.Variants, System.SysUtils;

type
  EExpressionParse=class(Exception)
  public
    Position : Integer;

    Constructor Create(APos:Integer; const AMessage:String);
  end;

  TData=Variant;

  TExpression=class;

  TResolveProc={$IFNDEF FPC}reference to{$ENDIF} function(const S:String; IsFunction:Boolean):TExpression;
  TErrorProc={$IFNDEF FPC}reference to{$ENDIF} function(const APos:Integer; const AMessage:String):Boolean;
  TExpressionProc=procedure(const Item:TExpression) of object;

  TExpression=class abstract
  public
    class var
      Null:TData;

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

    function Value:TData; override;
    function ToString:String; override;
  end;

  TFloatExpression=class(TExpression)
  public
    Number : Extended;

    Constructor Create(const AValue:Extended);

    function Value:TData; override;
    function ToString:String; override;

    class function E:TFloatExpression; static;
    class function Pi:TFloatExpression; static;
  end;

  TBooleanExpression=class(TExpression)
  public
    Logical : Boolean;

    Constructor Create(const AValue:Boolean);

    function Value:TData; override;
    function ToString:String; override;
  end;

  TDateTimeExpression=class(TExpression)
  public
    DateTime : TDateTime;

    Constructor Create(const AValue:TDateTime);

    function Value:TData; override;
    function ToString:String; override;

    class function Now:TDateTimeExpression; static;
  end;

  TTextExpression=class(TExpression)
  public
    Text : String;

    Constructor Create(const AValue:String);

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

    function Value:TData; override;
    function ToString:String; override;
    procedure Traverse(const AProc:TExpressionProc); override;
  end;

  TOperandExpression=class abstract (TExpression)
  protected
    function Part(const AExpression:TExpression):String;
  public
    Left,
    Right : TExpression;

    Destructor Destroy; override;

    procedure Traverse(const AProc:TExpressionProc); override;
  end;

  TArithmeticOperand=(Add,Subtract,Multiply,Divide,&Mod,Power);

  TArithmeticOperandHelper=record helper for TArithmeticOperand
  public
    class function FromString(const S:String; out Operand:TArithmeticOperand):Boolean; static;
    function ToString:String;
  end;

  TArithmeticExpression=class(TOperandExpression)
  public
    Operand : TArithmeticOperand;

    Constructor Create(const ALeft:TExpression; const AOperand:TArithmeticOperand; const ARight:TExpression);

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
  public
    Operand : TLogicalOperand;

    Constructor Create(const ALeft:TExpression; const AOperand:TLogicalOperand; const ARight:TExpression);
    function Value:TData; override;
    function ToString:String; override;
  end;

  TUnaryExpression=class abstract(TExpression)
  public
    Expression : TExpression;

    Constructor Create(const AExpression:TExpression);
    Destructor Destroy; override;

    procedure Traverse(const AProc:TExpressionProc); override;
  end;

  TUnaryNotExpression=class(TUnaryExpression)
  public
    function Value:TData; override;
    function ToString:String; override;
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

    function Value:TData; override;
    function ToString:String; override;
  end;

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
                 Century,
                 Millennium);

  TDateTimePartHelper=record helper for TDateTimePart
  public
    class
      var QuarterFormat:String;

    class function AllToText:String; static;
    function AsString(const Index:Integer):String;
    function ToString:String;
  end;

  TDateTimePartExpression=class(TUnaryExpression)
  public
    Part : TDateTimePart;

    class function FromString(const S:String; out APart:TDateTimePart):Boolean; static;
    class function FromData(const Value:TData):TDateTime; static;

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
