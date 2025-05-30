{*********************************************}
{  TeeBI Software Library                     }
{  Expressions and Data "Hops"                }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Expressions;

interface

uses
  BI.Arrays, BI.DataItem, BI.Expression;

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
  private
    const
      FunctionName='MovingAverage';

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

uses
  {System.}SysUtils,
  BI.Persist, BI.Languages.English;

{ TDataHops }

Destructor TDataHops.Destroy;
var t : Integer;
begin
  for t:=0 to High(Hops) do
      Hops[t].Free;

  inherited;
end;

// Try to "merge" existing hops with AIndex, if they are identical in their
// starting items.
// This is to accelerate lookups, using "Parent" as it will be already calculated
procedure TDataHops.TryReduce(const AIndex:Integer);
var t,
    tt : Integer;
    tmp : THops;
begin
  tmp:=Hops[AIndex];

  for t:=0 to AIndex-1 do
      if tmp.Items.Begins(Hops[t].Items) then
      begin
        // Found a valid route, set to Parent
        tmp.Parent:=Hops[t];

        // Delete all route steps that are common with Parent
        for tt:=0 to Hops[t].Items.Count-1 do
            tmp.Items.Delete(0);

        // Stop searching.
        // Note: A better algorithm will be searching for all possible solutions
        // and then choose the "longest" route to merge, instead of just stopping at
        // the first one found.
        break;
      end;
end;

// Initialize all Hops, searching the best route from Main to each Hop Data
procedure TDataHops.Init;
var t : Integer;
begin
  for t:=0 to High(Hops) do
  begin
    Hops[t].Init(Main,Data[t]);

    if t>0 then
       TryReduce(t); // <-- route optimization
  end;
end;

// Returns True when all Hops are Valid (there is a valid route from Main to Hop Data)
function TDataHops.Valid:Boolean;
var t : Integer;
begin
  result:=Main<>nil;

  if result then
     for t:=0 to High(Hops) do
         if (Data[t]<>Main) and (Hops[t].Items=nil) and (Hops[t].Parent=nil) then
            Exit(False);
end;

// When changing a master row index (AIndex), mark all route hops as invalid.
procedure TDataHops.Invalidate(const AIndex: TInteger);
var t : Integer;
begin
  for t:=0 to High(Hops) do
      Hops[t].Invalidate(AIndex);
end;

// Returns True when there is a route to go from ADetail to AMaster
function TDataHops.IsDetail(const ADetail,AMaster:TDataItem):Boolean;
var H : THops;
begin
  H:=THops.Create;
  try
    H.Init(ADetail,AMaster);
    result:=Length(H.Items)>0;
  finally
    H.Free;
  end;
end;

procedure TDataHops.TraverseExpression(const Item:TExpression);

  procedure CannotAccessData(const AData:TDataItem);
  begin
    raise EBIException.CreateFmt(BIMsg_CannotAccessData,[AData.Name,Main.Name]);
  end;

  function TrySetCommonParent(const AData:TDataItem):Boolean;
  begin
    result:=True;

    if Main.Parent=AData then
       Main:=AData
    else
    if Main.Parent=AData.Parent then
       Main:=Main.Parent
    else
       result:=False;
  end;

var tmp : TDataItem;
    tmpHop : Integer;
    tmpCol : TDataItem;
begin
  if Item is TDataItemExpression then
  begin
    tmpCol:=TDataItemExpression(Item).Data;

    if tmpCol=nil then
       CannotAccessData(Main);

    if tmpCol.AsTable then
       tmp:=tmpCol // <-- Problem in Select_Queries test 21 (> select Average)
    else
       tmp:=tmpCol.Parent;

    tmpHop:=Data.IndexOf(tmp);

    if tmpHop=-1 then
    begin
      tmpHop:=Add(tmp);

      if IChangeMain or (Main=nil) then
      begin
        if Main=nil then
           Main:=tmp
        else
        if IsDetail(tmp,Main) then
           Main:=tmp
        else
        if not IsDetail(Main,tmp) then
           if not TrySetCommonParent(tmp) then
              CannotAccessData(tmp);
      end
      else
      if (Main<>tmp) and (tmp<>nil) and
         (not tmp.IsChildOf(Main)) then // <-- also consider when tmp has items (sub-Items of Main)
      begin
        if Main.Parent=tmp then
           Main:=tmp
        else
        begin
          Hops[tmpHop].Init(Main,tmp);

          if Hops[tmpHop].Items=nil then
             CannotAccessData(tmp);
        end;
      end;

      TDataItemExpression(Item).Data.Load;
    end;

    TDataItemExpression(Item).FHops:=Hops[tmpHop];
  end;
end;

procedure TDataHops.Add(const AExpression: TExpression; const ChangeMain: Boolean);
begin
  IChangeMain:=ChangeMain;
  AExpression.Traverse(TraverseExpression);
end;

// Adds a new data as a "hop" in the router
function TDataHops.Add(const AData:TDataItem):Integer;
begin
  result:=Length(Hops); //Data.Count;
  Data.Add(AData);

  SetLength(Hops,result+1);
  Hops[result]:=THops.Create;
end;

{ TDataExpression }

function LocalToData(const AData:TDataItem; const S:String; const Error:TBIErrorProc=nil):TDataItem;

  function TryFindItem(const AData:TDataItem; const AName:String):TDataItem;

    procedure TryRaiseError;
    var tmp : String;
    begin
      tmp:=Format(BIMsg_CannotAccessData,[AName,AData.Name]);

      if not Error(nil,tmp) then
         raise EBIException.Create(tmp);
    end;

  begin
    if Assigned(Error) then
    begin
      result:=AData.Items.Find(AName);

      if result=nil then
         TryRaiseError;
    end
    else
      result:=AData[AName];
  end;

var tmpColName,
    tmpDataName : String;
    tmpItem,
    tmpData : TDataItem;
    i : Integer;
begin
  tmpData:=AData;
  tmpColName:=S;

  repeat
    i:=Pos('.',tmpColName);

    if i=0 then
       break
    else
    begin
      tmpDataName:=Trim(Copy(tmpColName,1,i-1));

      // Delete
      tmpColName:=Trim(Copy(tmpColName,i+1,Length(tmpColName)));

      if tmpData=nil then
         tmpItem:=nil
      else
      if tmpData.Parent=nil then
         tmpItem:=TryFindItem(tmpData,tmpDataName)
      else
         tmpItem:=TryFindItem(tmpData.Parent,tmpDataName);

      if tmpItem=nil then
         Exit(nil)
      else
         tmpData:=tmpItem;
    end;

  until tmpColName='';

  if tmpColName<>'' then
     result:=TryFindItem(tmpData,tmpColName)
  else
     result:=nil;
end;

{$IFDEF FPC}
function TDataExpression.ResolveFunction(const S:String; IsFunction:Boolean):TExpression;
var tmp : String;
begin
  if IsFunction then
  begin
    result:=TDataFunctions.TryParse(S);

    if result=nil then
    begin
      tmp:=Format(BIMsg_ExpressionFunctionMissing,[S]);

      if Assigned(IResolveError) then
         IResolveError(-1,tmp)
      else
         raise EBIException.CreateFmt(BIMsg_ExpressionError,[tmp,-1]);
    end;
  end
  else
    result:=IClass.Resolve(IData,S,IResolveError);
end;
{$ENDIF}

class function TDataExpression.FromString(const AData:TDataItem;
        const AExpression: String; const Error:TErrorProc): TExpression;
begin
  {$IFDEF FPC}
  IResolveError:=Error;
  IData:=AData;
  IClass:=Self;
  {$ENDIF}

  result:=TExpression.FromString(AExpression,
    {$IFDEF FPC}
    ResolveFunction
    {$ELSE}
    function(const S:String; IsFunction:Boolean):TExpression

      procedure DoError(APos:Integer; const AMessage:String);
      begin
        if Assigned(Error) then
           Error(APos,AMessage)
        else
           raise EBIException.CreateFmt(BIMsg_ExpressionError,[AMessage,APos]);
      end;

    begin
      if IsFunction then
      begin
        result:=TDataFunctions.TryParse(S);

        if result=nil then
           DoError(-1,Format(BIMsg_ExpressionFunctionMissing,[S]));
      end
      else
        result:=Resolver(AData,S,Error);
    end
    {$ENDIF}
    ,Error);
end;

{$IFDEF FPC}
var
  IError : TErrorProc=nil;

class function TDataExpression.CallError(const Sender:TObject; const Text:String):Boolean;
begin
  result:=Assigned(IError) and IError(-1,Text);
end;
{$ENDIF}

class function TDataExpression.Resolve(const AData:TDataItem; const AText:String;
                                       const Error:TErrorProc):TExpression;
var tmp : TDataItem;
begin
  {$IFDEF FPC}
  IError:=Error;
  {$ENDIF}

  if Pos('.',AText)>0 then
     // Simplified format: "table.column"
     tmp:=LocalToData(AData,AText,
            {$IFDEF FPC}
            CallError
            {$ELSE}
            function(const Sender:TObject; const Text:String):Boolean
            begin
              result:=Assigned(Error) and Error(-1,Text);
            end
            {$ENDIF}
            )
  else
     // Origin format (absolute or relative "..|xxx|yyy|zzz")
     tmp:=TStore.OriginToData(AData,'',AText,
            {$IFDEF FPC}
            CallError
            {$ELSE}
            function(const Sender:TObject; const Text:String):Boolean
            begin
              result:=Assigned(Error) and Error(-1,Text);
            end
            {$ENDIF}
            );

  if tmp=nil then
     result:=nil
  else
     result:=TDataItemExpression.Create(tmp,False,AData);
end;

type
  TParameterExpressionAccess=class(TParameterExpression);

class function TDataExpression.KindOf(const Expression:TExpression):TDataKind;

  function KindOf(const AClass:TExpressionClass):TDataKind; overload;
  begin
    if AClass=TBooleanExpression then
       result:=dkBoolean
    else
    if AClass=TTextExpression then
       result:=dkText
    else
    if AClass=TFloatExpression then
       result:=dkSingle
    else
    if AClass=TIntegerExpression then
       result:=dkInt32
    else
    if AClass=TDateTimeExpression then
       result:=dkDateTime
    else
       result:=dkUnknown;
  end;

begin
  if Expression=nil then
     result:=dkUnknown
  else
  if Expression is TLogicalExpression then
     result:=dkBoolean
  else
  if Expression is TArithmeticExpression then
     result:=TDataExpression.KindOf(TArithmeticExpression(Expression).Left) // Right?
  else
  if Expression is TUnaryNotExpression then
     result:=dkBoolean
  else
  if Expression is TDataItemExpression then
     result:=TDataItemExpression(Expression).Kind
  else
  if Expression is TColumnExpression then
     result:=TColumnExpression(Expression).Kind
  else
  if Expression is TParameterExpression then
     result:=KindOf(TParameterExpressionAccess(Expression).ResultClass)
  else
     result:=KindOf(TExpressionClass(Expression.ClassType));
end;

{ TDataFilter }

class function TDataFilter.VerifyLogical(const AExpression:TExpression;
                                         const AText: String;
                                         const Error:TErrorProc): TExpression;
  procedure DoError(const AText:String);
  begin
    if (not Assigned(Error)) or (not Error(-1,AText)) then
       raise EExpressionParse.Create(-1,AText);
  end;

begin
  if AExpression=nil then
  begin
    result:=nil;
    DoError(Format(BIMsg_ExpressionEmpty,[AText]));
  end
  else
  begin
    if TDataExpression.KindOf(AExpression)=TDataKind.dkBoolean then
       result:=AExpression
    else
    begin
      AExpression.{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};
      result:=nil;

      DoError(Format(BIMsg_ExpressionNotLogical,[AText]));
    end;
  end;
end;

class function TDataFilter.FromString(const AData: TDataItem;
                                      const AExpression: String;
                                      const Error:TErrorProc): TExpression;
begin
  result:=VerifyLogical(TDataExpression.FromString(AData,AExpression,Error),AExpression,Error);
end;

{ TDataItemExpression }

type
  TDataAccess=class(TDataItem);

Constructor TDataItemExpression.Create(const AData:TDataItem;
                                       const FreeData:Boolean=False;
                                       const AMain:TDataItem=nil);
begin
  inherited Create;

  FData:=AData;

  if FData<>nil then
     TDataAccess(FData).FConsumers.Add(Notify);

  if AMain=nil then
  begin
    if FData<>nil then
       FMain:=FData.Parent;
  end
  else
     FMain:=AMain;

  KeepData:=not FreeData;
end;

Destructor TDataItemExpression.Destroy;
begin
  if FData<>nil then
  begin
    TDataAccess(FData).FConsumers.Remove(Notify);

    if not KeepData then
      FData.Free;
  end;

  inherited;
end;

procedure TDataItemExpression.Assign(const Source: TExpression);
var tmp : TDataItemExpression;
begin
  if Source is TDataItemExpression then
  begin
    tmp:=TDataItemExpression(Source);

    FData:=tmp.FData;

    FHops:=nil;

    FMain:=tmp.FMain;

    KeepData:=True;
  end
//  else
//    inherited;
end;

function TDataItemExpression.Kind: TDataKind;
begin
  if Data=nil then
     result:=TDataKind.dkUnknown
  else
     result:=Data.Kind;
end;

function TDataItemExpression.IsLogical: Boolean;
begin
  result:=(Kind=TDataKind.dkBoolean);
end;

function TDataItemExpression.ToString: String;
begin
  if Data=nil then
     result:=''
  else
  begin
    result:=Data.Name;

    if result='' then
    begin
      if Data.Provider<>nil then
         result:='('+Data.Provider.ToString+')';
    end
    else
    if Data.Parent<>FMain then
       if Data.Parent<>nil then
          result:=Data.Parent.Name+'.'+result;
  end;
end;

class function TDataItemExpression.ValueOf(const AData:TDataItem; const AIndex:TInteger):TData;
begin
  if (AIndex=-1) or AData.Missing[AIndex] then
     result:=TExpression.Null
  else
  case AData.Kind of
     dkInt32: result:=AData.Int32Data[AIndex];
     dkInt64: result:=AData.Int64Data[AIndex];
    dkSingle: result:=AData.SingleData[AIndex];
    dkDouble: result:=AData.DoubleData[AIndex];
  dkExtended: result:=AData.ExtendedData[AIndex];
      dkText: result:=AData.TextData[AIndex];
  dkDateTime: result:=AData.DateTimeData[AIndex];
   dkBoolean: result:=AData.BooleanData[AIndex];
  end;
end;

function TDataItemExpression.Value: TData;
begin
  if Data.Kind=TDataKind.dkUnknown then
     // Temporary solution, use the first Data Item, first row value:
     if Data.Items.Count=0 then
        result:=TExpression.Null
     else
        result:=ValueOf(Data.Items[0],0)
  else
  if Hops=nil then
     result:=Null
  else
     result:=ValueOf(Data,Hops.Index);
end;

class function TDataItemExpression.NewLookup(const AName:String; const ADetail:TDataItem; const AMaster:TDataItem):TDataItem;
begin
  result:=ADetail.Items.New(AName,AMaster.Kind);
  TDataAccess(result).FParent:=ADetail;

  TDataItemExpression.LookupFill(AMaster,result);
end;

procedure TDataItemExpression.Notify(const AEvent: TBIEvent);
begin
  if AEvent=TBIEvent.Destroyed then
     FData:=nil;
end;

// Pending: Replace ADest with a TExpression (same as summary measure source)
class procedure TDataItemExpression.LookupFill(const ASource,ADest:TDataItem);
var t : TLoopInteger;
    tmp : TInteger;
    tmpHops : THops;
    Source : TDataItem;
begin
  TDataAccess(ADest).FKind:=ASource.Kind;

  tmpHops:=THops.Create;
  try
    tmpHops.Init(ADest.Parent,ASource.Parent);

    if ADest.Parent<>ASource.Parent then
       if tmpHops.Items=nil then
          raise EBIException.CreateFmt(BIMsg_CannotAccessData,[ASource.Parent.Name,ADest.Parent.Name]);

    ADest.Resize(ADest.Parent.Count);

    Source:=tmpHops.RealSource;

    if Source=nil then
       Source:=ASource;

    ASource.Load;

    if Source.Missing.Count>0 then
       ADest.Missing.Items.Resize(ADest.Count);

    for t:=0 to ADest.Count-1 do
    begin
      if Source.Missing[t] then
         ADest.Missing[t]:=True
      else
      begin
        tmp:=tmpHops.Items.Find(t);

        if tmp=-1 then
           ADest.Missing[t]:=True
        else
           ADest.CopyFrom(t,ASource,tmp);
      end;
    end;
  finally
    tmpHops.Free;
  end;
end;

{ TExpressionColumn }

Constructor TExpressionColumn.Create(const AExpression:TExpression);
begin
  inherited Create;
  Expression:=AExpression;
end;

Constructor TExpressionColumn.Create(const AData:TDataItem; const AExpression: String);
begin
  Create(TDataExpression.FromString(AData,AExpression));
  Data:=AData;
end;

class function TExpressionColumn.From(const AExpression:TExpression;
                                      const Fill:Boolean;
                                      const AName:String):TDataItem;
begin
  if AExpression=nil then
     result:=nil
  else
  begin
    result:=TExpressionColumn.Create(AExpression);
    result.Name:=AName;

    if Fill then
       TExpressionColumn(result).Fill;
  end;
end;

class function TExpressionColumn.From(const AParent:TDataItem;
                                      const AExpression: String;
                                      const Fill:Boolean;
                                      const AName: String;
                                      const Error:TErrorProc): TDataItem;
begin
  result:=From(TDataExpression.FromString(AParent,AExpression,Error),False,AName);

  if Fill then
     TExpressionColumn(result).Fill(AParent);
end;

procedure TExpressionColumn.LoadData(const Item:TExpression);
begin
  if Item is TDataItemExpression then
     TDataItemExpression(Item).Data.Load;
end;

procedure TExpressionColumn.Load(const Children:Boolean=False);
begin
  inherited;

  if Expression<>nil then
     Expression.Traverse(LoadData);
end;

procedure TExpressionColumn.SetExpression(const Value: TExpression);
begin
  if FExpression<>Value then
  begin
    FExpression.Free;
    FExpression:=Value;

    FKind:=TDataExpression.KindOf(FExpression);
  end;
end;

// Speed bottleneck due to usage of Expression.Value, which is a Variant type
// and forces VarClear finalization
procedure TExpressionColumn.SetValue(const ADest:TDataItem; const AIndex:TInteger);

  procedure SetInt32;
  begin
    ADest.Int32Data[AIndex]:=Expression.Value;
  end;

  procedure SetInt64;
  begin
    ADest.Int64Data[AIndex]:=Expression.Value;
  end;

  procedure SetSingle;
  begin
    ADest.SingleData[AIndex]:=Expression.Value;
  end;

  procedure SetDouble;
  begin
    ADest.DoubleData[AIndex]:=Expression.Value;
  end;

  procedure SetExtended;
  begin
    ADest.ExtendedData[AIndex]:=Expression.Value;
  end;

  procedure SetText;
  begin
    ADest.TextData[AIndex]:=Expression.Value;
  end;

  procedure SetDateTime;
  begin
    ADest.DateTimeData[AIndex]:=Expression.Value;
  end;

  procedure SetBoolean;
  begin
    ADest.BooleanData[AIndex]:=Expression.Value;
  end;

begin
  // Each Kind has a local proc to skip compiler FinalizeArray
  case Kind of
    dkInt32: SetInt32;
    dkInt64: SetInt64;
   dkSingle: SetSingle;
   dkDouble: SetDouble;
 dkExtended: SetExtended;
     dkText: SetText;
 dkDateTime: SetDateTime;
  dkBoolean: SetBoolean;
  end;
end;

procedure TExpressionColumn.Fill(const AParent:TDataItem);
var t : Integer;
    Hops : TDataHops;
    tmp : TDataItem;
begin
  if AParent=nil then
     tmp:=Parent
  else
     tmp:=AParent;

  if tmp=nil then
     Clear
  else
  begin
    Resize(tmp.Count);

    Hops:=TDataHops.Create;
    try
      Hops.Main:=tmp;
      Hops.Add(Expression,False);

      Load;

      if Expression is TColumnExpression then
         TColumnExpression(Expression).Calculate(Hops,Self)
      else
      begin
        for t:=0 to Count-1 do
        begin
          Hops.Invalidate(t);
          SetValue(Self,t);
        end;
      end;
    finally
      Hops.Free;
    end;
  end;
end;

function TExpressionColumn.Sort(const Ascending:Boolean=True): TNativeIntArray;
var tmp : TSortItems;
begin
  tmp.Add(Self,Ascending);
  Load;

  result:=tmp.Sort(Self);
end;

{ THops }

Destructor THops.Destroy;
begin
  Items.FreeAll;
  inherited;
end;

// Has AData any item which Master is AData again? (One-To-One link)
function AnyOneToOne(const AData:TDataItem):TDataItem;
var tmp : TDataItem;
begin
  for tmp in AData.Items.AsArray do
      if TDataAccess(tmp).HasMaster then
         if tmp.Master.Master=tmp then
            Exit(tmp.Master.Parent);

  result:=nil;
end;

// Try to do a full search to obtain a route from ADetail to AMaster
procedure THops.Init(const ADetail, AMaster:TDataItem);
var
  Visited : TDataArray;

  // Special case for bi-directional one-to-one relationships
  procedure TryFindInOneToOne;
  var tmp,
      tmpItem : TDataItem;
      tmpHops,
      tmpHops2,
      tmpHops3 : THops.THopArray;

      tmpVis : TDataArray;
      t : Integer;
  begin
    tmp:=AnyOneToOne(ADetail);

    if tmp<>nil then
    begin
      tmpVis:=nil;

      // For all data items that have tmp as master, try to find a link to AMaster

      if (tmp.Parent<>nil) and (not tmp.Parent.AsTable) then
      begin
        for tmpItem in tmp.Parent.Items.AsArray do
        if not Visited.Exists(tmpItem) then
        begin
          tmpHops:=InternalFind(tmpItem,tmp,tmpVis);

          if tmpHops<>nil then
          begin
            // Found an item that can route to "tmp" (the other branch master).

            // Now lets see if we can go from that other branch, to our desired
            // master:
            tmpVis:=nil;
            tmpHops2:=InternalFind(tmpItem,AMaster,tmpVis);

            if tmpHops2<>nil then
            begin
              // Yes, so we can add first a hop from ADetail to "tmp":
              tmpVis:=nil;
              tmpHops3:=InternalFind(ADetail,tmp,tmpVis);

              for t:=0 to High(tmpHops3) do
                  Items.Add(tmpHops3[t]);

              // And then from "tmp" to the final AMaster:

              for t:=0 to High(tmpHops) do
              begin
                tmpHops[t].Inverted:=True;
                Items.Add(tmpHops[t]);
              end;

              for t:=0 to High(tmpHops2) do
                  Items.Add(tmpHops2[t]);

              break;
            end;
          end;
        end;
      end;
    end;
  end;

var t : Integer;
begin
  if ADetail=AMaster then
  begin
    Items.FreeAll;
    Items:=nil;
  end
  else
  begin
    {$IFDEF FPC}
    Visited:=nil;
    {$ENDIF}

    // Search lookup link in same data:
    if Items=nil then // <-- might not be nil, added from Filter
       Items:=InternalFind(ADetail,AMaster,Visited);

    // Special case. When no route from master to detail can be found in the
    // normal tree branch, examine if there are any links between data items
    // in the "one-to-one" mode.
    // For each one-to-one link, try to use it as a "jumping" point between
    // tree branches, and then following that branch to obtain a link to the desired
    // goal.
    if Items=nil then
       TryFindInOneToOne;

    for t:=0 to High(Items) do
        Items[t].Prepare;
  end;
end;

// Returns the Master index position for a given Detail SourceIndex position
function THops.Index: TInteger;
var tmp : TInteger;
begin
  if not Valid then
  begin
    // Calculate full route, to obtain SourceIndex

    if Parent=nil then
       tmp:=SourceIndex
    else
       tmp:=Parent.Index;

    SourceIndex:=Items.Find(tmp);

    // Flag as cached, to avoid later recalc
    Valid:=True;
  end;

  result:=SourceIndex;
end;

// Returns the index in AList for any Master equal to ACol.
// ie: Is ACol a master of any of the items in AList?
function IndexOfMaster(const ACol:TDataItem; const AList:TDataArray):Integer;
var t : Integer;
begin
  for t:=0 to High(AList) do
      if AList[t].Master=ACol then
         Exit(t);

  result:=-1;
end;

// Multiple items lookup.
// Returns the array of data items in Start that have Dest as Parent.
function GuessMultiItems(const Start,Dest:TDataItem):TDataArray;
var tmp : TDataItem;
begin
  result:=nil;

  for tmp in Start.Items.AsArray do
      if TDataAccess(tmp).HasMaster and (tmp.Master.Parent=Dest) then
      begin
        result.Add(tmp);
        tmp.Load;
      end;
end;

// Try to find a route from Start -> to Dest
function THops.InternalFind(const Start,Dest:TDataItem; var Visited:TDataArray):THopArray;

  procedure FindHops(const Start:TDataItem);
  var t : Integer;
      Col : TDataItem;
      tmpItems : TDataArray;
  begin
    if not Visited.Exists(Start) then
    begin
      Visited.Add(Start);

      if Start.AsTable then
      for t:=0 to Start.Items.Count-1 do
      begin
        Col:=Start.Items[t];

        if TDataAccess(Col).HasMaster then
        begin
          if Col.Master.Parent=Dest then
          begin
            tmpItems:=GuessMultiItems(Start,Col.Master.Parent);

            if Length(tmpItems)>1 then
               result.Add(THop.Create(tmpItems))
            else
            begin
              result.Add(THop.Create(Col));
              RealSource:=Col;
            end;

            Exit;
          end
          else
          begin
            FindHops(Col.Master.Parent);

            if result<>nil then
            begin
              tmpItems:=GuessMultiItems(Start,Col.Master.Parent);

              if Length(tmpItems)>1 then
                 result.AddTop(THop.Create(tmpItems))
              else
                 result.AddTop(THop.Create(Col));

              Exit;
            end;
          end;
        end;
      end;
    end;
  end;

begin
  result:=nil;
  FindHops(Start);
end;

// Reset route, initialize SourceIndex as starting index position
procedure THops.Invalidate(const AIndex: TInteger);
begin
  Valid:=(Items=nil) and (Parent=nil); // FPC issue, Length(Items) works fine
  SourceIndex:=AIndex;
end;

{ THops.THop }

Constructor THops.THop.Create(const AData:TDataArray);
begin
  inherited Create;
  Items:=AData;
end;

Constructor THops.THop.Create(const AData:TDataItem);
begin
  inherited Create;
  Data:=AData;
end;

function THops.THop.ToString: String;
var tmp : TDataItem;
begin
  result:='';

  if Items=nil then
     result:=Data.Parent.Name+'.'+Data.Name
  else
  for tmp in Items do
      result:=result+tmp.Parent.Name+'.'+tmp.Name+' ';
end;

type
  // Simple class to "invert" a Map index
  TInvertedIndex=class({$IFDEF CPUX64}TInt64Map{$ELSE}TInt32Map{$ENDIF})
  private
    Sequence : TNativeIntArray;

    function Calculate:TNativeIntArray;
    procedure Swap(const A,B:TInteger);
  public
    Constructor Create(const AIndex:TNativeIntArray);
  end;

{ TInvertedIndex }

Constructor TInvertedIndex.Create(const AIndex:TNativeIntArray);
var t : TLoopInteger;
begin
  inherited Create;

  Items:=AIndex;

  Sequence.Resize(AIndex.Count);

  for t:=0 to High(Sequence) do
      Sequence[t]:=t;

  Items.Sort(True,Swap);
end;

procedure TInvertedIndex.Swap(const A, B: TInteger);
begin
  Items.Swap(A,B);
  Sequence.Swap(A,B);
end;

function TInvertedIndex.Calculate:TNativeIntArray;
var t : TLoopInteger;
    tmp : TNativeInteger;
begin
  {$IFDEF FPC}
  result:=nil;
  {$ENDIF}

  result.Resize(Sequence.Count);

  for t:=0 to High(result) do
      if Find(t,tmp) then // <-- what to do if Find returns False here?
         result[t]:=Sequence[tmp]
      else
         result[t]:=-1; // ?? raise EBIException !!! (internal error here)
end;

// Bottleneck !!! (special case one-to-one links)
function THops.THop.CreateInvertedIndex(const AIndex:TNativeIntArray):TNativeIntArray;
var tmp : TInvertedIndex;
begin
  tmp:=TInvertedIndex.Create(AIndex.Copy);
  try
    result:=tmp.Calculate;
  finally
    tmp.Free;
  end;
end;

// Returns True when all Items of Value are identical to Self Items
function THops.THop.IsEqual(const Value: THop): Boolean;
begin
  result:=Inverted=Value.Inverted;

  if result then
     if IData=nil then
        result:=Items.Equal(Value.Items)
     else
        result:=(IData=Value.IData);
end;

// Create the Index this hop will use to do lookups (detail -> master)
procedure THops.THop.Prepare;
begin
  if Items=nil then
  begin
    // Single field index
    IData:=Data;

    if TDataAccess(IData).IMaster.Index=nil then
       IData.CreateMasterIndex;
  end
  else
  begin
    // Multiple fields index, use first field as holder
    IData:=Items[0];

    if TDataAccess(IData).IMaster.Index=nil then
       TDataAccess(IData).IMaster.Index:=IData.CreateIndexMulti(Items);
  end;

  IsBool:=IData.Master.Kind=dkBoolean;

  // Check for special "inverted" index (one-to-one lookups)
  if not IsBool then
     if Inverted then
        Index:=CreateInvertedIndex(TDataAccess(IData).IMaster.Index)
     else
        Index:=TDataAccess(IData).IMaster.Index;
end;

{ THops.THopArrayHelper }

procedure THops.THopArrayHelper.Add(const Hop:THop);
var L : Integer;
begin
  L:=Length(Self);
  SetLength(Self,L+1);
  Self[L]:=Hop;
end;

{$IFNDEF FPC}
{$IF CompilerVersion>27}
{$DEFINE INSERTARRAY}
{$ENDIF}
{$ENDIF}

// Add Hop as first item in array
procedure THops.THopArrayHelper.AddTop(const Hop:THop);
{$IFNDEF INSERTARRAY}
var L, t : Integer;
{$ENDIF}
begin
  {$IFDEF INSERTARRAY}
  Insert(Hop,Self,0);
  {$ELSE}
  L:=Length(Self);
  SetLength(Self,L+1);

  for t:=L-1 downto 0 do
      Self[t+1]:=Self[t];

  Self[0]:=Hop;
  {$ENDIF}
end;

// Returns True when all items in AItems are an exact copy of this Items
function THops.THopArrayHelper.Begins(const AItems: THopArray): Boolean;
var t : Integer;
begin
  result:=(Count>0) and (AItems.Count>0) and (Count>=AItems.Count);

  if result then
     for t:=0 to AItems.Count-1 do
         if not AItems[t].IsEqual(Self[t]) then
            Exit(False);
end;

function THops.THopArrayHelper.Count: Integer;
begin
  result:=Length(Self);
end;

procedure THops.THopArrayHelper.Delete(const AIndex: Integer);
var t : Integer;
begin
  Assert((AIndex>=0) and (AIndex<Count),'Index '+IntToStr(AIndex)+' out of bounds deleting THops.Items');

  Self[AIndex].Free;

  for t:=AIndex to High(Self)-1 do
      Self[t]:=Self[t+1];

  SetLength(Self,High(Self));
end;

// Search from Detail.Index -> to Master.Index -> ... Master.Index (follow route)
function THops.THopArrayHelper.Find(const Index:TInteger):TInteger;
var t : Integer;
begin
  result:=Index;

  for t:=0 to High(Self) do
  begin
    // inlined: result:=Self[t].Find(result);

    if Self[t].IsBool then
       result:=Ord(Self[t].IData.BooleanData[result])
    else
       result:=Self[t].Index[result]; // lookup using Index

    // Stop route jumps due to bad data: (no master for a given detail)
    if result=-1 then
       break;
  end;
end;

procedure THops.THopArrayHelper.FreeAll;
var tmp : THops.THop;
begin
  for tmp in Self do
      tmp.{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};
end;

{ TDataFunctions }

class function TDataFunctions.IndexOf(const AClass: TColumnExpressionClass): Integer;
var t : Integer;
begin
  for t:=0 to Count-1 do
      if Items[t]=AClass then
         Exit(t);

  result:=-1;
end;

class procedure TDataFunctions.Register(const AClass: TColumnExpressionClass);
begin
  if IndexOf(AClass)=-1 then
  begin
    SetLength(Items,Count+1);
    Items[Count]:=AClass;
    Inc(Count);
  end;
end;

class function TDataFunctions.TryParse(const S: String): TExpression;
var t : Integer;
begin
  for t:=0 to Count-1 do
  begin
    result:=Items[t].TryParse(S);

    if result<>nil then
       Exit;
  end;

  result:=nil;
end;

class procedure TDataFunctions.UnRegister(const AClass: TColumnExpressionClass);
var t,
    tmp : Integer;
begin
  tmp:=IndexOf(AClass);

  if tmp<>-1 then
  begin
    for t:=tmp to Count-2 do
        Items[t]:=Items[t+1];

    Dec(Count);
    SetLength(Items,Count);
  end;
end;

{ TIsNullData }

function TIsNullData.ResultClass: TExpressionClass;
begin
  result:=TBooleanExpression;
end;

procedure TIsNullData.SetExpression(const Value: TExpression);
begin
  inherited;

  if Expression is TDataItemExpression then
     FData:=TDataItemExpression(Expression)
  else
     FData:=nil;
end;

class function TIsNullData.FromString(const S:String):TParameterExpression;
begin
  if SameText(S,'ISNULL') then
     result:=TIsNullData.Create
  else
     result:=nil;
end;

function TIsNullData.Value:TData;
begin
  if FData=nil then
     result:=TExpression.Null
  else
     result:=FData.Data.Missing[FData.Hops.Index];
end;

{ TSortItemArray }

function TSortItemArrayHelper.NotInOrder(const A,B:TInteger):Boolean;
var t : Integer;
    tmp : Integer;
begin
  for t:=0 to High(Self) do
  if Self[t].Active then
  begin
    tmp:=Self[t].Data.Compare(A,B,Self[t].IgnoreTextCase);

    if tmp<>0 then
       if Self[t].Descending then
          Exit(tmp>0)
       else
          Exit(tmp<0);
  end;

  result:=False; // all equal
end;

{ TSortItems }

function TSortItems.IndexOf(const AData: TDataItem): Integer;
var t : Integer;
begin
  for t:=0 to High(Items) do
      if Items[t].Data=AData then
         Exit(t);

  result:=-1;
end;

function TSortItems.InvertOrder(const AData:TDataItem): Boolean;
var tmp : Integer;
begin
  tmp:=IndexOf(AData);

  result:=tmp<>-1;

  if result then
     Items[tmp].Descending:=not Items[tmp].Descending;
end;

class procedure TSortItems.CheckExpression(const AItem:TSortItem; const AParent:TDataItem);
var tmp : TExpressionColumn;
begin
  if AItem.Data is TExpressionColumn then
  begin
    tmp:=TExpressionColumn(AItem.Data);

    if tmp.Count=0 then
    begin
      if tmp.Data=nil then
         TDataAccess(AItem.Data).FParent:=AParent
      else
         TDataAccess(AItem.Data).FParent:=tmp.Data;

      tmp.Fill;
      AParent.Items.Add(AItem.Data);
    end;
  end;
end;

// Checks all sort items are valid
procedure TSortItems.VerifySort(const AData:TDataItem);
var t : Integer;
begin
  // Safety checks
  if ActiveCount<=0 then
     raise EBIException.Create(BIMsg_SortNoItems);

  for t:=Low(Items) to High(Items) do
    if Items[t].Active then
       if Items[t].Data=nil then
          raise EBIException.Create(BIMsg_SortNoData)
       else
       begin
         CheckExpression(Items[t],AData);

         if AData<>nil then
            if (Items[t].Data<>AData) and (not Items[t].Data.IsChildOf(AData)) then
               raise EBIException.CreateFmt(BIMsg_SortWrongParent,[Items[t].Data.FullName,AData.Name]);
       end;
end;

// This will NOT reorder the rows. It will just return an array with the
// rows indexes according to the sort process.
function TSortItems.Sort(const AData:TDataItem): TNativeIntArray;
begin
  result:=Sort(AData,nil);
end;

class function TSortItems.DataCount(const AData:TDataItem):TInteger;
begin
  if AData.AsTable or (AData.Kind<>TDataKind.dkUnknown) then
     result:=AData.Count
  else
     result:=AData.Items.Count;
end;

function TSortItems.Sort(const AData:TDataItem; const AIndex:TNativeIntArray): TNativeIntArray;

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
      while Items.NotInOrder(result[i],result[x]) do inc(i);
      while Items.NotInOrder(result[x],result[j]) do dec(j);

      if i<j then
      begin
        result.Swap(i,j);

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

    if l<j then
       PrivateSort(l,j);

    if i<r then
       PrivateSort(i,r);
  end;

var t : TLoopInteger;
    tmp : TInteger;
begin
  VerifySort(AData);

  if AIndex=nil then
  begin
    tmp:=DataCount(AData);

    {$IFDEF FPC}
    result:=nil;
    {$ENDIF}

    result.Resize(tmp);

    // Sequence
    for t:=0 to tmp-1 do
        result[t]:=t;
  end
  else
  begin
    tmp:=AIndex.Count;
    result:=AIndex;
  end;

  if (tmp>1) and (ActiveCount>0) then
//     result.Sort(True,Items.NotInOrder);
     PrivateSort(0,tmp-1);
end;

// This WILL reorder the AData rows.
procedure TSortItems.SortData(const AData:TDataItem);
var
  OldSort : TDataArray;

  procedure PreserveSortData;
  var t : Integer;
  begin
    SetLength(OldSort,Count);

    for t:=0 to Count-1 do
        OldSort[t]:=Items[t].Data;
  end;

  procedure RestoreSortData;
  var t : Integer;
  begin
    for t:=0 to Count-1 do
        Items[t].Data:=OldSort[t];
  end;

  // Try replacing possible SortBy items with the new result items:
  procedure ReplaceSortData(const AData:TDataItem);

    function FindTagObject(const ATag,ASource:TDataItem):TDataItem;
    var t : Integer;
        tmp : TDataItem;
    begin
      for t:=0 to ASource.Items.Count-1 do
      begin
        tmp:=ASource.Items[t];

        if ATag=TDataAccess(tmp).TagObject then
           Exit(tmp)
        else
        if tmp.AsTable then
        begin
          result:=FindTagObject(ATag,tmp);

          if result<>nil then
             Exit;
        end;
      end;

      result:=nil;
    end;

  var t : Integer;
      tmp : TDataItem;
  begin
    for t:=Low(Items) to High(Items) do
    begin
      tmp:=FindTagObject(Items[t].Data,AData);

      if tmp<>nil then
         Items[t].Data:=tmp;
    end;
  end;

var
  RowItems : TSortItemArray;

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
      while RowItems.NotInOrder(i,x) do inc(i);
      while RowItems.NotInOrder(x,j) do dec(j);

      if i<j then
      begin
        TDataAccess(AData).SwapRows(i,j);

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

    if l<j then
       PrivateSort(l,j);

    if i<r then
       PrivateSort(i,r);
  end;

  procedure ClearExpression(const AData:TDataItem);
  begin
    AData.Parent.Items.Delete(AData);
    TDataAccess(AData).FParent:=nil; // <-- move to Items.Delete
    AData.Resize(0);
  end;

  function GetRowItems:TSortItemArray;
  var L,
      t : Integer;
  begin
    result:=nil;

    L:=0;

    for t:=0 to Count-1 do
        if Items[t].Active and (not Items[t].Data.AsTable) then
        begin
          SetLength(result,L+1);
          result[L]:=Items[t];
          Inc(L);
        end;
  end;

  procedure SortTableItems;
  var t : Integer;
      tmp : TDataItem;
  begin
    for t:=0 to Count-1 do
        if Items[t].Active and Items[t].Data.AsTable then
        begin
          // Ascending sort is not necessary (it is already sorted ascending)
          if Items[t].Descending then
          begin
            tmp:=Items[t].Data;

            while tmp.AsTable and (tmp.Items.Count=1) do
                  tmp:=tmp.Items[0];

            if tmp.Items.Count>1 then
               if TDataItem(TDataAccess(tmp).TagObject).Kind=TDataKind.dkText then
                  tmp.Items.SortByName(False,Items[t].IgnoreTextCase)
               else
                  tmp.Items.Reverse;
          end;
        end;
  end;

var tmp : TInteger;
    t,
    L : Integer;
begin
  L:=ActiveCount;

  if L>0 then
  begin
    PreserveSortData; // <-- remember all "real" sort data items
    ReplaceSortData(AData); // <-- try replacing sort items with AData items
    try
      VerifySort(AData);

      RowItems:=GetRowItems;

      tmp:=DataCount(AData);

      if (tmp>1) and (RowItems<>nil) then
         // GenericSort(0,tmp-1,RowItems.NotInOrder,TDataAccess(AData).SwapRows)
         PrivateSort(0,tmp-1);

      SortTableItems;

      for t:=Low(Items) to High(Items) do
          if Items[t].Active and (Items[t].Data is TExpressionColumn) then
             ClearExpression(Items[t].Data);
    finally
      RestoreSortData; // <-- restore back all "real" data items
    end;
  end;
end;

procedure TSortItems.Add(const AData:TDataItem; const Ascending:Boolean=True; const IgnoreTextCase:Boolean=False);
var L : Integer;
begin
  if IndexOf(AData)=-1 then
  begin
    L:=Length(Items);
    SetLength(Items,L+1);

    Items[L].Active:=True;
    Items[L].Data:=AData;
    Items[L].Descending:=not Ascending;
    Items[L].IgnoreTextCase:=IgnoreTextCase;
  end;
end;

procedure TSortItems.Clear;
//var tmp : TSortItem;
begin
  (*
  for tmp in Items do
      if tmp.Data is TExpressionColumn then
         tmp.Data.{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};
  *)
  Items:=nil;
end;

function TSortItems.Count: Integer;
begin
  result:=Length(Items);
end;

function TSortItems.ActiveCount: Integer;
var t : Integer;
begin
  result:=0;

  for t:=Low(Items) to High(Items) do
      if Items[t].Active then
         Inc(result);
end;

procedure TSortItems.Delete(const AIndex: Integer);
var t : Integer;
begin
  Assert((AIndex>=0) and (AIndex<Count),'Index '+IntToStr(AIndex)+' out of bounds deleting TSortItems');

  for t:=AIndex to Count-2 do
      Items[t]:=Items[t+1];

  SetLength(Items,Count-1);
end;

procedure TSortItems.Exchange(const A, B: Integer);
var tmp : TSortItem;
begin
  tmp:=Items[A];
  Items[A]:=Items[B];
  Items[B]:=tmp;
end;

function TSortItems.ToString: String;
var t,
    tmpCount : Integer;
begin
  if Items=nil then
     result:=''
  else
  begin
    result:='';

    tmpCount:=0;

    for t:=Low(Items) to High(Items) do
    if Items[t].Active then
    begin
      if tmpCount>0 then
         result:=result+', ';

      if Items[t].Data is TExpressionColumn then
         result:=Items[t].Data.ToString
      else
         result:=Items[t].Data.Name;

      if Items[t].Descending then
         result:=result+':Descending ';

      Inc(tmpCount);
    end;
  end;
end;

{ TDataItemSort }

class procedure TDataItemSort.By(const AData: TDataItem;
                                 const AExpression: TExpression;
                                 const Ascending: Boolean=True;
                                 const IgnoreTextCase:Boolean=False);
var tmp : TDataItem;
begin
  tmp:=TExpressionColumn.From(AExpression);
  try
    AData.Items.Add(tmp);
    AData.SortBy(tmp,Ascending,IgnoreTextCase);
  finally
    tmp.Free;
  end;
end;

class procedure TDataItemSort.By(const AData: TDataItem;
                                 const AExpression:String;
                                 const Ascending: Boolean=True;
                                 const IgnoreTextCase:Boolean=False);
var tmp : TDataItem;
begin
  tmp:=TExpressionColumn.From(AData,AExpression,True);
  try
    AData.Items.Add(tmp);
    AData.SortBy(tmp,Ascending,IgnoreTextCase);
  finally
    tmp.Free;
  end;
end;

{ TColumnFunction }

class function TColumnFunction.TryParse(const S:String):TColumnExpression;
var tmpOp : TColumnOperand;
begin
  if TColumnOperand.FromString(S,tmpOp) then
  begin
    result:=TColumnFunction.Create(nil);
    TColumnFunction(result).Operand:=tmpOp;
  end
  else
    result:=nil;
end;

function TColumnFunction.Kind:TDataKind;
var tmp : TDataKind;
begin
  if Expression=nil then
     result:=dkUnknown
  else
  if Operand=TColumnOperand.Cumulative then
  begin
    tmp:=TDataExpression.KindOf(Expression);

    if tmp=dkInt32 then
       result:=dkInt64
    else
       result:=dkSingle;
  end
  else
    result:=dkSingle;
end;

{ TColumnOperandHelper }

class function TColumnOperandHelper.FromString(const S: String;
  out Operand: TColumnOperand): Boolean;
begin
  result:=True;

  if SameText(S,'PERCENT') then Operand:=TColumnOperand.Percent else
  if SameText(S,'PERCENTCHANGE') then Operand:=TColumnOperand.PercentChange else
  if SameText(S,'CUMULATIVE') then Operand:=TColumnOperand.Cumulative else

//  if SameText(S,TMovingAverageColumn.FunctionName) then Operand:=TColumnOperand.MovingAverage else
     result:=False;
end;

function TColumnOperandHelper.ToString: String;
begin
  case Self of
         Percent: result:='Percent';
   PercentChange: result:='PercentChange';
      Cumulative: result:='Cumulative';
//   MovingAverage: result:=TMovingAverageColumn.FunctionName;
  end;
end;

{ TColumnFunction }

procedure TColumnFunction.Calculate(const Hops:TDataHops; const Dest: TDataItem);

  procedure CalcPercent;
  var t : TLoopInteger;
      Sum : TFloat;
  begin
    Sum:=0;

    for t:=0 to Dest.Count-1 do
    begin
      Hops.Invalidate(t);
      Sum:=Sum+Expression.Value;
    end;

    if Sum<>0 then
    begin
      Sum:=100/Sum;

      for t:=0 to Dest.Count-1 do
      begin
        Hops.Invalidate(t);
        Dest.SingleData[t]:=Expression.Value*Sum;
      end;
    end;
  end;

  procedure CalcPercentChange;
  var Old,
      tmpNew,
      AbsOld : TFloat;
      t : TLoopInteger;
  begin
    Dest.SingleData[0]:=100;
    Hops.Invalidate(0);

    Old:=Expression.Value;
    AbsOld:=Abs(Old);

    for t:=1 to Dest.Count-1 do
    begin
      Hops.Invalidate(t);

      tmpNew:=Expression.Value;

      if AbsOld=0 then
         AbsOld:=tmpNew;

      Dest.SingleData[t]:=100*(tmpNew-Old)/AbsOld;
    end;
  end;

  procedure CalcCumulative;
  var t : TLoopInteger;
  begin
    Hops.Invalidate(0);

    case Dest.Kind of
       dkInt64: Dest.Int64Data[0]:=Expression.Value;
      dkSingle: Dest.SingleData[0]:=Expression.Value;
    end;

    for t:=1 to Dest.Count-1 do
    begin
      Hops.Invalidate(t);

      case Dest.Kind of
         dkInt64: Dest.Int64Data[t]:=Dest.Int64Data[t-1]+Expression.Value;
        dkSingle: Dest.SingleData[t]:=Dest.SingleData[t-1]+Expression.Value;
      end;
    end;
  end;

begin
  if Expression<>nil then
     case Operand of
            Percent: CalcPercent;

      PercentChange: if Dest.Count>0 then
                        CalcPercentChange;

         Cumulative: if Dest.Count>0 then
                        CalcCumulative;
    end;
end;

function TColumnFunction.ToString: String;
begin
  result:=Operand.ToString+'('+Expression.ToString+')';
end;

function TColumnFunction.Value: TData;
begin
  {$IFDEF FPC}
  result:=TExpression.Null;
  {$ENDIF}

  raise EBIException.Create('Expression Column Function cannot be evaluated');
end;

{$IFDEF FPC}
function ResolveDataExpression(const AData:TDataItem;
                               const AExpression: String;
                               const Error:TErrorProc): TExpression;
begin
  result:=TDataExpression.Resolve(AData,AExpression,Error);
end;
{$ENDIF}

{ TMovingAverageColumn }

Constructor TMovingAverageColumn.Create(const AExpression: TExpression;
  const APeriod: Integer);
begin
  inherited Create(AExpression);
  Period:=APeriod;
end;

function TMovingAverageColumn.Kind: TDataKind;
begin
  if Expression=nil then
     result:=dkUnknown
  else
     result:=dkSingle;
end;

procedure TMovingAverageColumn.Calculate(const Hops: TDataHops;
  const Dest: TDataItem);

var
  Items : TSingleArray;
  Sum : Single;

  procedure Accumulate(const AIndex,AItem:Integer);
  begin
    Hops.Invalidate(AIndex);
    Items[AItem]:=Expression.Value;
    Sum:=Sum+Items[AItem];
  end;

var t : TLoopInteger;
begin
  if Expression<>nil then
  begin
    if (Period>0) and (Dest.Count>=Period) then
    begin
      Items.Resize(Period);

      Sum:=0;

      for t:=0 to Period-2 do
      begin
        Accumulate(t,t);
        Dest.Missing[t]:=True;
      end;

      for t:=Period-1 to Dest.Count-1 do
      begin
        Accumulate(t,Period-1);
        Dest.SingleData[t]:=Sum/Period;

        Sum:=Sum-Items[0];

        System.Move(Items[1],Items[0],SizeOf(Single)*(Period-1));
      end;
    end;
  end;
end;

function TMovingAverageColumn.ToString: String;
begin
  result:=FunctionName+'('+Expression.ToString+')';
end;

class function TMovingAverageColumn.TryParse(const S: String): TColumnExpression;
begin
  if SameText(S,FunctionName) then
     result:=TMovingAverageColumn.Create(nil,10)
  else
     result:=nil;
end;

function TMovingAverageColumn.Value: TData;
begin
  {$IFDEF FPC}
  result:=TExpression.Null;
  {$ENDIF}

  raise EBIException.Create('Expression Moving Average Function cannot be evaluated');
end;

initialization
  TParameterExpression.Registered.Add(TIsNullData);
  TDataFunctions.Register(TColumnFunction);
  TDataFunctions.Register(TMovingAverageColumn);

  {$IFDEF FPC}
  TDataExpression.Resolver:=ResolveDataExpression;
  {$ELSE}
  TDataExpression.Resolver:=function(const AData:TDataItem;
                                     const AExpression: String;
                                     const Error:TErrorProc): TExpression
  begin
    result:=TDataExpression.Resolve(AData,AExpression,Error);
  end;
  {$ENDIF}
finalization
  TDataFunctions.UnRegister(TMovingAverageColumn);
  TDataFunctions.UnRegister(TColumnFunction);
  TParameterExpression.Registered.Remove(TIsNullData);
end.
