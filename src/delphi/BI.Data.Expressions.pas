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
  THops=class
  private
  type
    THop=class
    private
      IData : TDataItem; // Cached
      IsBool : Boolean;
      Inverted : Boolean;
      Index : TNativeIntArray;

      Data : TDataItem;
      Items : TDataArray; // Multiple

      function CreateInvertedIndex(const AIndex:TNativeIntArray):TNativeIntArray;
      procedure Prepare;
    public
      Constructor Create(const AData:TDataItem); overload;
      Constructor Create(const AData:TDataArray); overload;

      function ToString:String; override;
    end;

    THopArray=Array of THop;

    THopArrayHelper=record helper for THopArray
    private
      procedure FreeAll;
    public
      procedure Add(const Hop:THop);
      procedure AddTop(const Hop:THop);
      function Find(const Index:TInteger):TInteger;
    end;

    function InternalFind(const Start,Dest:TDataItem; var Visited:TDataArray):THops.THopArray;
    procedure Invalidate(const AIndex:TInteger);
  public
  var
    Items : THopArray;
    RealSource : TDataItem;
    Valid : Boolean;
    SourceIndex : TInteger;

    Destructor Destroy; override;

    function Index:TInteger;
    procedure Init(const ADetail,AMaster:TDataItem);
  end;

  TDataHops=class
  private
    IChangeMain : Boolean;

    function IsDetail(const ADetail,AMaster:TDataItem):Boolean;
    procedure TraverseExpression(const Item:TExpression);
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
  public
    Constructor Create(const AData:TDataItem; const FreeData:Boolean=False; const AMain:TDataItem=nil);
    Destructor Destroy; override;

    procedure Assign(const Source:TExpression); override;
    function IsLogical:Boolean;
    class procedure LookupFill(const ASource,ADest:TDataItem); static;
    class function NewLookup(const AName:String; const ADetail:TDataItem; const AMaster:TDataItem):TDataItem;
    function ToString:String; override;
    function Value:TData; override;

    property Data:TDataItem read FData;
    property Hops:THops read FHops;
  end;

  TColumnExpression=class(TUnaryExpression)
  protected
    procedure Calculate(const Hops:TDataHops; const Dest:TDataItem); virtual; abstract;
    function KindOf:TDataKind; virtual; abstract;
    class function TryParse(const S:String):TColumnExpression; virtual; abstract;
  end;

  TIsNullData=class(TColumnExpression)
  protected
    procedure Calculate(const Hops:TDataHops; const Dest:TDataItem); override;
    function KindOf:TDataKind; override;
    class function TryParse(const S:String):TColumnExpression; override;
  public
    function Value:TData; override;
  end;

  TColumnExpressionClass=class of TColumnExpression;

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

    Destructor Destroy; override;

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

  TSortItemArray=Array of TSortItem;

  TSortItemArrayHelper=record helper for TSortItemArray
  public
    function NotInOrder(const A,B:TInteger):Boolean;
  end;

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

  TDataFilter=record
  public
    class function FromString(const AData:TDataItem; const AExpression: String;
                              const Error:TErrorProc=nil): TExpression; static;

    class function VerifyLogical(const AExpression:TExpression;
                                      const AText: String;
                                      const Error:TErrorProc): TExpression; static;
  end;

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
    function KindOf:TDataKind; override;
    class function TryParse(const S:String):TColumnExpression; override;
  public
    Operand : TColumnOperand;

    function ToString:String; override;
    function Value:TData; override;
  end;

  TMovingAverageColumn=class(TColumnExpression)
  protected
    procedure Calculate(const Hops:TDataHops; const Dest:TDataItem); override;
    function KindOf:TDataKind; override;
    class function TryParse(const S:String):TColumnExpression; override;
  public
    Period : Integer;

    Constructor Create(const AExpression:TExpression; const APeriod:Integer);

    function ToString:String; override;
    function Value:TData; override;
  end;

implementation
