{*********************************************}
{  TeeBI Software Library                     }
{  Expressions and Data "Hops"                }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.Expressions;

interface

uses
  BI.Arrays, BI.Data, BI.Expression;

type
  // Represents a route (a list of "jumps") to enable lookups from a detail
  // table to a master table (ie: Orders->Products->Categories)
  THops=class
  private

  type
    // Represents a single jump or step in the route
    THop=class
    private
      IData : TDataItem; // Cached
      IsBool : Boolean;
      Inverted : Boolean;
      Index : TNativeIntArray;

      Data : TDataItem;
      Items : TDataArray; // Multiple

      function CreateInvertedIndex(const AIndex:TNativeIntArray):TNativeIntArray;
      function IsEqual(const Value:THop):Boolean;
      procedure Prepare;
    public
      Constructor Create(const AData:TDataItem); overload; // single field relationship
      Constructor Create(const AData:TDataArray); overload; // multiple field relationship

      function ToString:String; override;
    end;

    // List of simple hops (jumps)
    THopArray=Array of THop;

    THopArrayHelper=record helper for THopArray
    private
      function Begins(const AItems:THopArray):Boolean;
      procedure FreeAll;
    public
      procedure Add(const Hop:THop);
      procedure AddTop(const Hop:THop);
      function Count:Integer; inline;
      procedure Delete(const AIndex:Integer);
      function Find(const Index:TInteger):TInteger;
    end;

    function InternalFind(const Start,Dest:TDataItem; var Visited:TDataArray):THops.THopArray;
    procedure Invalidate(const AIndex:TInteger);

  public
  var
    Items : THopArray;

    //[Weak]
    Parent : THops;

    RealSource : TDataItem;
    SourceIndex : TInteger;
    Valid : Boolean;

    Destructor Destroy; override;

    function Index:TInteger;
    procedure Init(const ADetail,AMaster:TDataItem);
  end;

  // List of hops (routes) from a Main table to an array of Data masters
  TDataHops=class
  private
    IChangeMain : Boolean;

    function IsDetail(const ADetail,AMaster:TDataItem):Boolean;
    procedure TraverseExpression(const Item:TExpression);
    procedure TryReduce(const AIndex:Integer);
  public
    Data : TDataArray;
    Main : TDataItem;
    Hops : Array of THops;

    Destructor Destroy; override;

    procedure Add(const AExpression:TExpression; const ChangeMain:Boolean); overload;
    function Add(const AData:TDataItem):Integer; overload;
    procedure Init;
    procedure Invalidate(const AIndex:TInteger);
    function Valid:Boolean;
  end;

  // Simple Expression that refers to a Data TDataItem and position (row)
  TDataItemExpression=class(TExpression)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FData : TDataItem;

    FHops : THops;

    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FMain : TDataItem;

    KeepData : Boolean;
  protected
    procedure Notify(const AEvent:TBIEvent);
    class function ValueOf(const AData:TDataItem; const AIndex:TInteger):TData; static;
  public
    Constructor Create(const AData:TDataItem; const FreeData:Boolean=False; const AMain:TDataItem=nil);
    Destructor Destroy; override;

    procedure Assign(const Source:TExpression); override;
    function Kind:TDataKind;
    function IsLogical:Boolean; inline;
    class procedure LookupFill(const ASource,ADest:TDataItem); static;
    class function NewLookup(const AName:String; const ADetail:TDataItem; const AMaster:TDataItem):TDataItem;
    function ToString:String; override;
    function Value:TData; override;

    property Data:TDataItem read FData;
    property Hops:THops read FHops;
  end;

  // Expression to return True or False for a given Data Missing value.
  // Equivalent to sql "isnull"
  TIsNullData=class(TParameterExpression)
  private
    FData : TDataItemExpression;
  protected
    function ResultClass:TExpressionClass; override;
    procedure SetExpression(const Value:TExpression); override;
  public
    class function FromString(const S:String):TParameterExpression; override;
    function Value:TData; override;
  end;

  // Abstract class to define a data column with values calculated using an Expression
  TColumnExpression=class(TUnaryExpression)
  protected
    procedure Calculate(const Hops:TDataHops; const Dest:TDataItem); virtual; abstract;
    function Kind:TDataKind; virtual; abstract;
    class function TryParse(const S:String):TColumnExpression; virtual; abstract;
  end;

  TColumnExpressionClass=class of TColumnExpression;

  // List of "registered" column expression classes so they can be used
  // inside other expressions
  TDataFunctions=record
  private
    class function IndexOf(const AClass:TColumnExpressionClass):Integer; static;
  public
    class var
      Count : Integer;
      Items : Array of TColumnExpressionClass;

    class procedure Register(const AClass:TColumnExpressionClass); static;
    class function TryParse(const S:String):TExpression; static;
    class procedure UnRegister(const AClass:TColumnExpressionClass); static;
  end;

  // A special TDataItem that is filled with values obtained calling an Expression
  TExpressionColumn=class(TDataItem)
  private
    procedure LoadData(const Item:TExpression);
    procedure SetExpression(const Value: TExpression);
  protected
    FExpression : TExpression;

    procedure SetValue(const ADest:TDataItem; const AIndex:TInteger);
  public
    Data : TDataItem;

    Constructor Create(const AExpression:TExpression); overload;
    Constructor Create(const AData:TDataItem; const AExpression:String); overload;

    procedure Fill(const AParent:TDataItem=nil);

    class function From(const AExpression:TExpression;
                        const Fill:Boolean=True;
                        const AName:String=''):TDataItem; overload; static;

    class function From(const AParent:TDataItem;
                        const AExpression: String;
                        const Fill:Boolean=True;
                        const AName: String='';
                        const Error:TErrorProc=nil): TDataItem; overload; static;

    procedure Load(const Children:Boolean=False); override;
    function Sort(const Ascending:Boolean=True):TNativeIntArray;

    property Expression:TExpression read FExpression write SetExpression;
  end;

  // List of sorting specifiers (for multiple order-by)
  TSortItemArray=Array of TSortItem;

  TSortItemArrayHelper=record helper for TSortItemArray
  public
    function NotInOrder(const A,B:TInteger):Boolean;
  end;

  // Helper methods to order a TDataItem using multiple sorting criteria
  TSortItems=record
  private
    class procedure CheckExpression(const AItem:TSortItem; const AParent:TDataItem); static;
    class function DataCount(const AData:TDataItem):TInteger; static;
    procedure VerifySort(const AData:TDataItem);
  public
    Items : TSortItemArray;

    function ActiveCount:Integer;

    procedure Add(const AData:TDataItem; const Ascending:Boolean=True; const IgnoreTextCase:Boolean=False); overload;

    procedure Clear;
    function Count:Integer; inline;

    procedure Delete(const AIndex:Integer);
    procedure Exchange(const A,B:Integer);

    function IndexOf(const AData:TDataItem):Integer;

    // Switches sort order from ascending to descending and viceversa:
    function InvertOrder(const AData:TDataItem):Boolean;

    // Calculates and returns the sorted array of indexes,
    // without reordering AData rows:
    function Sort(const AData:TDataItem): TNativeIntArray; overload;
    function Sort(const AData:TDataItem; const AIndex:TNativeIntArray): TNativeIntArray; overload;

    // Orders AData, reordering its rows:
    procedure SortData(const AData:TDataItem);

    // Sort Items, Human-readable
    function ToString: String;
  end;

  // Helper class to enable using data names in expressions.
  // The expression parser calls "Resolver" when attempting to recognize
  // undefined string variables
  TDataExpression=class
  public
    type
      TResolver={$IFNDEF FPC}reference to{$ENDIF} function(const AData:TDataItem;
                                      const AExpression: String;
                                      const Error:TErrorProc=nil): TExpression;

  private
    class var
      FResolver : TResolver;

    {$IFDEF FPC}
    type
      TDataExpressionClass=class of TDataExpression;

    class var
      IResolveError : TErrorProc;
      IData : TDataItem;
      IClass : TDataExpressionClass;

    function ResolveFunction(const S:String; IsFunction:Boolean):TExpression;
    class function CallError(const Sender:TObject; const Text:String):Boolean;
    {$ENDIF}
  protected
    class function Resolve(const AData:TDataItem; const AText:String;
                           const Error:TErrorProc):TExpression; virtual;
  public
    class function FromString(const AData:TDataItem; const AExpression: String; const Error:TErrorProc=nil): TExpression;
    class function KindOf(const Expression:TExpression):TDataKind; static;

    class property Resolver:TResolver read FResolver write FResolver;
  end;

  // Helper methods to parse an expression string to be used as a "filter"
  // (equivalent to sql "where" clause)
  TDataFilter=record
  public
    class function FromString(const AData:TDataItem; const AExpression: String;
                              const Error:TErrorProc=nil): TExpression; static;

    class function VerifyLogical(const AExpression:TExpression;
                                 const AText: String;
                                 const Error:TErrorProc): TExpression; static;
  end;

  // Small method to sort AData
  TDataItemSort=record
  public
    class procedure By(const AData: TDataItem;
                       const AExpression:String;
                       const Ascending: Boolean=True;
                       const IgnoreTextCase:Boolean=False); overload; static;

    class procedure By(const AData: TDataItem;
                       const AExpression:TExpression;
                       const Ascending: Boolean=True;
                       const IgnoreTextCase:Boolean=False); overload; static;
  end;

  TColumnOperand=(Percent,PercentChange,Cumulative
                  //,MovingAverage <-- pending until support for more than one parameter in TExpression parser
                  );

  TColumnOperandHelper=record helper for TColumnOperand
  public
    class function FromString(const S:String; out Operand:TColumnOperand):Boolean; static;
    function ToString:String;
  end;

  // Expression to Aggregate data using all items in a column
  TColumnFunction=class(TColumnExpression)
  protected
    procedure Calculate(const Hops:TDataHops; const Dest:TDataItem); override;
    function Kind:TDataKind; override;
    class function TryParse(const S:String):TColumnExpression; override;
  public
    Operand : TColumnOperand;

    function ToString:String; override;
    function Value:TData; override;
  end;

  TMovingAverageColumn=class(TColumnExpression)
  protected
    procedure Calculate(const Hops:TDataHops; const Dest:TDataItem); override;
    function Kind:TDataKind; override;
    class function TryParse(const S:String):TColumnExpression; override;
  public
    Period : Integer;

    Constructor Create(const AExpression:TExpression; const APeriod:Integer);

    function ToString:String; override;
    function Value:TData; override;
  end;

implementation
