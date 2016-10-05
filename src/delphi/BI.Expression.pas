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

  TResolveProc=
    {$IFDEF FPC}
    function(const S:String; IsFunction:Boolean):TExpression of object;
    {$ELSE}
    reference to function(const S:String; IsFunction:Boolean):TExpression;
    {$ENDIF}

  TErrorProc={$IFNDEF FPC}reference to{$ENDIF}
              function(const APos:Integer; const AMessage:String):Boolean
                {$IFDEF FPC}of object{$ENDIF};

  TExpressionProc=procedure(const Item:TExpression) of object;

  TExpression=class abstract
  private

    {$IFDEF FPC}
    function DoError(const APos:Integer; const AMessage:String):Boolean;
    function NilFunction(const S:String; IsFunction:Boolean):TExpression;
    {$ELSE}
    class function DoError(const APos:Integer; const AMessage:String):Boolean; static;
    {$ENDIF}

  public
    class var
      Null:TData;

    procedure Assign(const Source:TExpression); virtual; abstract;
    function AsString:String; virtual;

    class function Clone(const AExpression:TExpression):TExpression; overload; static;

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

  TExpressionClass=class of TExpression;

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
    class function DateSpan(const ADelta:TDateTime):TDateTimeExpression; static;
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

  TExpressionsHelper=record helper for TExpressions
  public
    procedure Add(const AExpression:TExpression); overload;
    procedure Add(const AExpressions:Array of TExpression); overload;
    function Count:Integer; inline;
  end;

  TArrayExpression=class(TExpression)
  private
    function Get(const Index: Integer): TExpression; inline;
    procedure Put(const Index: Integer; const Value: TExpression); inline;
  public
    Items : TExpressions;
    IsParams : Boolean;

    Constructor Create(const AValues:Array of TExpression); overload;
    Constructor Create(const AValues:Array of TData); overload;
    Destructor Destroy; override;

    procedure Add(const AValue:TExpression);
    procedure Assign(const Source:TExpression); override;
    procedure Clear;
    function Count:Integer; inline;

    function ToString:String; override;
    procedure Traverse(const AProc:TExpressionProc); override;
    function Value:TData; override;

    property Item[const Index:Integer]:TExpression read Get write Put; default;
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
  private
    const
      Texts : Array[TArithmeticOperand] of String=(
                '+','-','*','/','mod','^'
                );
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

  TLogicalOperand=(Equal,NotEqual,Greater,Lower,GreaterOrEqual,LowerOrEqual,&And,&Or,&In);

  TLogicalOperandHelper=record helper for TLogicalOperand
  private
    const
      Texts : Array[TLogicalOperand] of String=(
                '=','<>','>','<','>=','<=','and','or','in'
                );
  public
    class function FromString(const S:String; out Operand:TLogicalOperand):Boolean; static;
    function ToString:String;
  end;

  TLogicalExpression=class(TOperandExpression)
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

    procedure Init;
  public
    Operand : TLogicalOperand;

    Constructor Create; overload;

    Constructor Create(const ALeft:TExpression;
                       const AOperand:TLogicalOperand;
                       const ARight:TExpression); overload;

    procedure Assign(const Source: TExpression); override;
    class function Join(const AOld,ANew:TLogicalExpression;
                        const AOperand:TLogicalOperand=TLogicalOperand.&And):TLogicalExpression; static;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TUnaryExpression=class abstract(TExpression)
  private
    FExpression: TExpression;
  protected
    procedure SetExpression(const Value:TExpression); virtual;
  public
    Constructor Create(const AExpression:TExpression); overload;
    Destructor Destroy; override;

    procedure Assign(const Source:TExpression); override;
    procedure Traverse(const AProc:TExpressionProc); override;

    property Expression : TExpression read FExpression write SetExpression;
  end;

  TUnaryNotExpression=class(TUnaryExpression)
  public
    function Value:TData; override;
    function ToString:String; override;
  end;

  TParameterExpression=class;

  TParameterExpressionClass=class of TParameterExpression;

  TParameterExpressions=Array of TParameterExpressionClass;

  TParameterExpressionsHelper=record helper for TParameterExpressions
  public
    procedure Add(const AClass:TParameterExpressionClass);
    function IndexOf(const AClass:TParameterExpressionClass):Integer;
    procedure Remove(const AClass:TParameterExpressionClass);
  end;

  TParameterExpression=class(TUnaryExpression)
  private
    class function Guess(const Token:String):TParameterExpression; static;
  protected
    FParams : TArrayExpression;

    function ResultClass:TExpressionClass; virtual; abstract;
    procedure SetExpression(const Value:TExpression); override;
  public
    class var
      Registered : TParameterExpressions;

    Constructor Create(const AParameters:Array of TData); overload;

    class function Call(const AParameters:TExpressions):TExpression; static;

    class function FromString(const S:String):TParameterExpression; virtual;
    function ToString:String; override;

    property Parameters:TArrayExpression read FParams;
  end;

  TUnaryDateTimeExpression=class(TParameterExpression)
  protected
    function ResultClass:TExpressionClass; override;
  end;

  TDateExpression=class(TUnaryDateTimeExpression)
  public
    class function FromData(const Value:TData):TDateTime; static;
    class function FromString(const S:String):TParameterExpression; override;
    function ToString:String; override;
    function Value:TData; override;

    class function Today:TDateExpression; static;
    class function Tomorrow:TDateExpression; static;
    class function Yesterday:TDateExpression; static;
  end;

  TTimeExpression=class(TUnaryDateTimeExpression)
  public
    class function FromData(const Value:TData):TDateTime; static;
    class function FromString(const S:String):TParameterExpression; override;
    function ToString:String; override;
    function Value:TData; override;
  end;

  TMathOperand=(Abs,Sin,Cos,Tan,Sqr,Sqrt,Log,Ln,Exp,Round,Trunc,Power,Sign);

  TMathOperandHelper=record helper for TMathOperand
  private
    const
      Texts : Array[TMathOperand] of String=(
                'Abs','Sin','Cos','Tan','Sqr','Sqrt','Log','Ln','Exp',
                'Round','Trunc','Power','Sign'
              );
  public
    class function FromString(const S:String; out Operand:TMathOperand):Boolean; static;
    function ToString:String;
  end;

  TMathExpression=class(TParameterExpression)
  protected
    function ResultClass:TExpressionClass; override;
    procedure SetExpression(const Value:TExpression); override;
  public
    Operand : TMathOperand;

    procedure Assign(const Source:TExpression); override;
    class function FromString(const S:String):TParameterExpression; override;
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
  private
    const
      Texts : Array[TDateTimePart] of String=(
               'None',
               //Nanosecond,
               //Microsecond,
               'Millisecond',
               'Hundred of second',
               'Tenth of second',
               'Second',
               'Minute',
               'Quarter-Hour',
               'Hour',
               'Day',
               'Day of Year',
               'Week',
               'Weekday',
               'Weekday short',
               'Weekday long',
               'Month',
               'Month short',
               'Month long',
               'Quarter',
               'Year',
               'Decade',
               'Decade of Year',
               'Century',
               'Millennium'
             );
  public
    class
      var QuarterFormat:String;

    class function AllToText:String; static;
    function AsString(const Index:Integer):String;
    class function FromString(const S:String; out APart:TDateTimePart):Boolean; static;
    function High:Integer;
    function Low: Integer;
    class function Max:Integer; static;
    function ToString:String; overload;
    class function ToString(const Index:Integer):String; overload; inline; static;
    class function ToCode(const Index:Integer):String; static;
  end;

  TDateTimePartExpression=class(TParameterExpression)
  private
    function AsInteger(const AValue:TDateTime):Integer;
  protected
    function ResultClass:TExpressionClass; override;
  public
    Part : TDateTimePart;

    procedure Assign(const Source:TExpression); override;
    class function FromString(const S:String):TParameterExpression; override; //out APart:TDateTimePart):Boolean; static;

    function Value:TData; override;
    function ToString:String; override;
  end;

  TTextUnaryOperand=(Lower,Upper,IsEmpty,Length,Trim);

  TTextUnaryOperandHelper=record helper for TTextUnaryOperand
  private
    const
      Texts : Array[TTextUnaryOperand] of String=(
              'Lower',
              'Upper',
              'IsEmpty',
              'Length',
              'Trim'
              );
  public
    class function FromString(const S:String; out Operand:TTextUnaryOperand):Boolean; static;
    function ToString:String;
  end;

  TUnaryTextExpression=class(TParameterExpression)
  protected
    function ResultClass:TExpressionClass; override;
  public
    Operand : TTextUnaryOperand;

    procedure Assign(const Source:TExpression); override;
    class function FromString(const S:String):TParameterExpression; override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TTextLogicalOperand=(Starts,Ends,Contains);

  TTextLogicalOperandHelper=record helper for TTextLogicalOperand
  private
    const
      Texts : Array[TTextLogicalOperand] of String=(
               'Starts',
               'Ends',
               'Contains'
              );
  public
    class function FromString(const S:String; out Operand:TTextLogicalOperand):Boolean; static;
    function ToString:String;
  end;

  TTextLogicalExpression=class(TOperandExpression)
  private
    FCase : Boolean;

    function DoContains(const A,B:String):Boolean;
    function EndsWith(const A,B:String):Boolean;
    function StartsWith(const A,B:String):Boolean;
  public
    Operand : TTextLogicalOperand;

    Constructor Create(const ALeft:TExpression; const AOperand:TTextLogicalOperand; const ARight:TExpression);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;

    property CaseSensitive:Boolean read FCase write FCase default False;
  end;

  TTextOperand=(IndexOf,Pad,Split,Insert,Remove,Replace,Count,SubString);

  TTextOperandHelper=record helper for TTextOperand
  private
    const
      Texts : Array[TTextOperand] of String=(
               'IndexOf',
               'Pad',
               'Split',
               'Insert',
               'Remove',
               'Replace',
               'Count',
               'SubString'
              );

    function ParameterCount:Integer;
  public
    class function FromString(const S:String; out Operand:TTextOperand):Boolean; static;
    function ToString:String;
  end;

  TTextOperandExpression=class(TParameterExpression)
  private
    class function Split(const S,Delimiter:String):TData;
    function ValueFrom(const Params:TArrayExpression):TData;
  protected
    function ResultClass:TExpressionClass; override;
  public
    Operand : TTextOperand;

    procedure Assign(const Source:TExpression); override;
    function AsString:String; override;

    class function FromString(const S:String):TParameterExpression; override;

    function Value:TData; override;
    function ToString:String; override;
  end;

  // Value := Condition ? Then : Else
  TIfExpression=class(TExpression)
  public
    Condition : TLogicalExpression;

    ThenExpression,
    ElseExpression : TExpression;

    Constructor Create(const ACondition:TLogicalExpression; const AThen,AElse:TExpression);
    Destructor Destroy; override;

    procedure Assign(const Source:TExpression); override;

    function ToString:String; override;
    function Value:TData; override;
  end;

implementation
