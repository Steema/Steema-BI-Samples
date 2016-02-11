// Experimental
unit BI.Variants;

interface

uses
  System.SysUtils,
  BI.Arrays;

type
  TBIVariantKind=(VInteger, VInt64, VSingle, VDouble, VExtended, VDateTime, VString, VBoolean, VArray);

  TBIArrayRange=record
    High,
    Low : TInteger;
  end;

  BIVariant=record
  private
    function Get(const Index: TInteger): BIVariant; inline;
    procedure Put(const Index: TInteger; const Value: BIVariant); inline;
  public
    function ArrayHigh(const ADimension:Integer):TInteger; inline;
    function ArrayLow(const ADimension:Integer):TInteger; inline;

    function IsArray:Boolean; inline;
    function IsFloat:Boolean; inline;
    function IsOrdinal:Boolean; inline;
    function IsString:Boolean; inline;

    property Item[const Index:TInteger]:BIVariant read Get write Put; default;

    class operator Add(const Source,Dest:BIVariant):BIVariant; inline;
    class operator Divide(const Source,Dest:BIVariant):BIVariant; inline;
    class operator Modulus(const Source,Dest:BIVariant):BIVariant; inline;
    class operator Multiply(const Source,Dest:BIVariant):BIVariant; inline;
//    class operator Positive(const Source:BIVariant):Double; inline;
    class operator Round(const Source:BIVariant):BIVariant; inline;
    class operator Subtract(const Source,Dest:BIVariant):BIVariant; inline;
    class operator Trunc(const Source:BIVariant):BIVariant; inline;

    class operator Equal(const Source,Dest:BIVariant):Boolean; inline;
    class operator GreaterThan(const Source,Dest:BIVariant):Boolean; inline;
    class operator GreaterThanOrEqual(const Source,Dest:BIVariant):Boolean; inline;
    class operator LessThan(const Source,Dest:BIVariant):Boolean; inline;
    class operator LessThanOrEqual(const Source,Dest:BIVariant):Boolean; inline;
    class operator LogicalAnd(const Source,Dest:BIVariant):Boolean; inline;
    class operator LogicalNot(const Source:BIVariant):Boolean; inline;
    class operator LogicalOr(const Source,Dest:BIVariant):Boolean; inline;
    class operator NotEqual(const Source,Dest:BIVariant):Boolean; inline;

    class operator Implicit(const Value:BIVariant):String; inline;
    class operator Implicit(const Value:BIVariant):TDateTime; inline;
    class operator Implicit(const Value:BIVariant):Integer; inline;
    class operator Implicit(const Value:BIVariant):Int64; inline;
    class operator Implicit(const Value:BIVariant):Single; inline;
    class operator Implicit(const Value:BIVariant):Double; inline;
    class operator Implicit(const Value:BIVariant):Extended; inline;
    class operator Implicit(const Value:BIVariant):Real; inline;
    class operator Implicit(const Value:BIVariant):Boolean; inline;

    class operator Implicit(const Value:Boolean):BIVariant; inline;
    class operator Implicit(const Value:Integer):BIVariant; inline;
    class operator Implicit(const Value:Int64):BIVariant; inline;
    class operator Implicit(const Value:Single):BIVariant; inline;
    class operator Implicit(const Value:Double):BIVariant; inline;
    class operator Implicit(const Value:Extended):BIVariant; inline;
    class operator Implicit(const Value:String):BIVariant; inline;
    class operator Implicit(const Value:Array of BIVariant):BIVariant;

    class operator Explicit(const Value:Single):BIVariant; inline;
    class operator Explicit(const Value:Double):BIVariant; inline;
    class operator Explicit(const Value:Extended):BIVariant; inline;
    class operator Explicit(const Value:BIVariant):Double; inline;

    class function Null:BIVariant; static; inline;
  private
    FKind : TBIVariantKind;
    FNull : Boolean;

    FItems : Array of BIVariant;
    FRanges : Array of TBIArrayRange;

    case TBIVariantKind of
      VInteger: (FInteger : Integer);
      VInt64  : (FInt64 : Int64);
      VSingle : (FSingle : Single);
      VDouble : (FDouble : Double);
      VExtended : (FExtended : Extended);
      VDateTime: (FDateTime : TDateTime);
      VString : (FString : PString);
      VBoolean: (FBoolean : Boolean);
  end;

implementation
