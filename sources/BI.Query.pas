{*********************************************}
{  TeeBI Software Library                     }
{  TBIQuery Component                         }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Query;

interface

{
  Main query component TBIQuery to do sql-like queries, summaries, filtering,
  sorting etc.
}

uses
  {System.}Classes, BI.DataItem, BI.DataSource, BI.Summary, BI.Expression,
  BI.Persist, BI.Expression.Filter, BI.CollectionItem,
  BI.Expressions;

{
  TBIQuery is a component capable of executing queries against TDataItem objects.

  The BIQuery Dimensions and Measures properties contain the desired output items.

  Each item defines a data item (field or table) and optional parameters like
  aggregation kind for Measures.

  BIQuery automatically determines if the calculation must be done using
  a TSummary class (because there is at least one item with aggregation),
  or a TDataSelect class (a normal "select" query without any "Group By").

  Items can also define expressions (ie: "sum(a+b)" or "Upper(ProductName)" ) and
  can refer to data items from multiple databases or tables without the need to
  specify the links between them (no "join" clauses).

  The BIQuery Filter property can be set as a string:

   Filter.Text:= 'City="London"';

  or as an expression object:

   Filter.Custom:= MyExpression;

  and can also refer to any data item, even if it is not included in the
  query output.

  The BIQuery SortBy property is a collection of TQuerySortItem objects.
  They define the sort order of the Query output rows and columns.
}

type
  TBIQuery=class;

  // Base class for Dimensions and Measures
  TQueryItem=class(TDataCollectionItem)
  private
    FEnabled: Boolean;

    procedure DoChanged(Sender:TObject);
    function GetQuery: TBIQuery;
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure Changed; override;
  public
    Constructor Create(Collection: TCollection); override;

    procedure Assign(Source:TPersistent); override;

    property Query:TBIQuery read GetQuery;
  published
    property Enabled:Boolean read FEnabled write SetEnabled default True;
  end;

  TDimensionStyle=(Automatic,Row,Column);

  TQueryDimension=class(TQueryItem)
  private
    FGroupBy : TGroupBy;
    FStyle: TDimensionStyle;

    function GetDatePart:TDateTimePart;
    function GetExpression: TExpression;
    function GetHistogram: THistogram;

    procedure SetDatePart(const Value: TDateTimePart);
    procedure SetExpression(const Value: TExpression);
    procedure SetHistogram(const Value: THistogram);
    procedure SetStyle(const Value: TDimensionStyle);
  protected
    property GroupBy:TGroupBy read FGroupBy;
    function Layout:TGroupByLayout;
    procedure SetData(const Value: TDataItem); override;
  public
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    function RealData:TDataItem;
    function RealStyle:TDimensionStyle;
    function ToString:String; override;

    property Expression:TExpression read GetExpression write SetExpression;
  published
    property DatePart:TDateTimePart read GetDatePart write SetDatePart default TDateTimePart.None;
    property Histogram:THistogram read GetHistogram write SetHistogram;
    property Style:TDimensionStyle read FStyle write SetStyle default TDimensionStyle.Automatic;
  end;

  TQueryMeasure=class(TQueryItem)
  private
    FMeasure : TMeasure;

    function GetAggregate:TAggregate;
    function GetCalculation: TMeasureCalculation;
    function GetExpression: TExpression;
    function GetMissing: TMeasureMissing;

    procedure SetAggregate(const Value: TAggregate);
    procedure SetCalculation(const Value: TMeasureCalculation);
    procedure SetExpression(const Value: TExpression);
    procedure SetMissing(const Value: TMeasureMissing);
  protected
    property Measure:TMeasure read FMeasure;
    procedure SetData(const Value: TDataItem); override;
  public
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    property Expression:TExpression read GetExpression write SetExpression;
    function RealData:TDataItem;
    function ToString:String; override;
  published
    property Aggregate:TAggregate read GetAggregate write SetAggregate default TAggregate.Count;
    property Calculation : TMeasureCalculation read GetCalculation write SetCalculation;
    property Missing : TMeasureMissing read GetMissing write SetMissing;
  end;

  // Base class for Dimension and Measure collections
  TQueryCollection=class(TOwnedCollection)
  private
    IUpdating : Boolean;

    procedure DoChanged;
    function GetQuery: TBIQuery;
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    property Query:TBIQuery read GetQuery;
  end;

  // Zero or more Dimensions (to select or group-by)
  TQueryDimensions=class(TQueryCollection)
  private
    function AddItem(const AData: TDataItem;
                     const AStyle: TDimensionStyle):TQueryDimension;
    function EnabledCount:Integer;
    function Get(const Index: Integer): TQueryDimension;
    procedure Put(const Index: Integer; const Value: TQueryDimension);
    procedure Removed(const AComponent:TComponent);
  protected
    function Add(const AGroupBy:TGroupBy):TQueryDimension; overload;
  public
    Constructor Create(AOwner: TPersistent);

    function Add(const AData:TDataItem;
                 const AStyle:TDimensionStyle=TDimensionStyle.Automatic;
                 const IsActive:Boolean=True): TQueryDimension; overload;

    function Add(const AData:TDataItem;
                 const AExpression:String;
                 const AStyle:TDimensionStyle=TDimensionStyle.Automatic): TQueryDimension; overload;

    procedure Exchange(const A,B:TQueryDimension);
    function IndexOf(const ADimension:TQueryDimension):Integer;
    procedure Swap;

    property Items[const Index:Integer]:TQueryDimension read Get write Put; default;
  end;

  // Zero or more Aggregations (Sum, Count, Average, Min, Max)
  TQueryMeasures=class(TQueryCollection)
  private
    function Get(const Index: Integer): TQueryMeasure;
    procedure Put(const Index: Integer; const Value: TQueryMeasure);
    procedure Removed(const AComponent:TComponent);
  protected
    function Add(const AMeasure:TMeasure):TQueryMeasure; overload;
    function EnabledCount:Integer;
  public
    Constructor Create(AOwner: TPersistent);

    function Add(const AData:TDataItem;
                 const AMeasure:TAggregate;
                 const IsActive:Boolean=True): TQueryMeasure; overload;

    procedure Exchange(const A,B:TQueryMeasure);
    function IndexOf(const AMeasure:TQueryMeasure):Integer;

    property Items[const Index:Integer]:TQueryMeasure read Get write Put; default;
  end;

  TQuerySortItem=class(TQueryItem)
  private
    FAscending: Boolean;
    FIgnoreTextCase: Boolean; // Default is False (ie: case-sensitive text order)

    procedure SetAscending(const Value: Boolean);
    procedure SetIgnoreTextCase(const Value: Boolean);
  public
    Constructor Create(ACollection:TCollection); override;

    procedure Assign(Source:TPersistent); override;
  published
    property Ascending:Boolean read FAscending write SetAscending default True;
    property IgnoreTextCase: Boolean read FIgnoreTextCase write SetIgnoreTextCase default True;
  end;

  TQuerySort=class(TQueryCollection)
  private
    procedure CopyFrom(const ASort:TSortItems);
    function Get(const Index: Integer): TQuerySortItem;
    procedure Put(const Index: Integer; const Value: TQuerySortItem);
  protected
    function Sort:TSortItems;
  public
    Constructor Create(AOwner: TPersistent);

    function AddSort(const AData:TDataItem;
                     const Ascending:Boolean=True;
                     const IgnoreTextCase:Boolean=True):TQuerySortItem;

    function ItemOf(const AData:TDataItem):TQuerySortItem;

    property Items[const Index:Integer]:TQuerySortItem read Get write Put; default;
  end;

  TQueryStyle=(Unknown,Select,Summary);

  {$IFNDEF FPC}
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(TeeAllComponentPlatformIDs)]
  {$ENDIF}
  {$ENDIF}
  TBIQuery=class(TBaseDataImporter)
  private
    FDimensions: TQueryDimensions;
    FDistinct : Boolean;
    FFilter : TBIFilter;
    FMax : Int64;
    FMeasures: TQueryMeasures;
    FOnError : TErrorProc;
    FRemoveMissing : TRemoveMissing;
    FSort : TQuerySort;
    FStart : Int64;

    IMain : TDataItem;
    ILoading : Boolean;
    IClearing : Boolean;

    procedure AddDimensions(const ASummary:TSummary);
    procedure AddMeasures(const ASummary:TSummary);
    procedure DoChanged(Sender:TObject);
    procedure DoClearData(const AData:TDataItem);
    procedure SetDimensions(const Value: TQueryDimensions);
    procedure SetDistinct(const Value: Boolean);
    procedure SetFilter(const Value: TBIFilter);
    procedure SetMax(const Value: Int64);
    procedure SetMeasures(const Value: TQueryMeasures);
    procedure SetRemoveMissing(const Value: TRemoveMissing);
    procedure SetStartRow(const Value: Int64);
    procedure SetSort(const Value: TQuerySort);
    function IsDimensionsStored: Boolean;
    function IsMeasuresStored: Boolean;
    function IsSortStored: Boolean;
  protected
    procedure Changed; override;
    function CreateProvider:TDataProvider;
    procedure GetItems(const AData: TDataItem); override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure Loaded; override;

    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
  public
    Style : TQueryStyle;

    Constructor Create(AOwner:TComponent); override;
    Constructor From(const AOwner:TComponent; const ASelect:TDataSelect); overload;
    Constructor From(const AOwner:TComponent; const ASummary:TSummary); overload;

    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function Calculate:TDataItem;
    class function CanBeMeasure(const AData:TDataItem):Boolean; static;
    procedure Clear;

    procedure Parse(const AData:TDataItem; const SQL:String; const AError:TBIErrorProc=nil);
    procedure Refresh;
    function ToString:String; override;

    property Main:TDataItem read IMain;
    property OnError:TErrorProc read FOnError write FOnError;
  published
    property Dimensions:TQueryDimensions read FDimensions write SetDimensions stored IsDimensionsStored;
    property Distinct:Boolean read FDistinct write SetDistinct default False;
    property Filter:TBIFilter read FFilter write SetFilter;
    property MaxRows:Int64 read FMax write SetMax default 0;
    property Measures:TQueryMeasures read FMeasures write SetMeasures stored IsMeasuresStored;
    property RemoveMissing:TRemoveMissing read FRemoveMissing write SetRemoveMissing;
    property SortBy:TQuerySort read FSort write SetSort stored IsSortStored;
    property StartRow:Int64 read FStart write SetStartRow default 0;
  end;

implementation

uses
 {$IF Declared(CompilerVersion)}
  {$IF CompilerVersion>=33}
  {System.}Generics.Collections, // <-- cnAdded
  {$ENDIF}
 {$ENDIF} 
  BI.Arrays, BI.SQL;

{ TBIQuery }

type
  TBIFilterAccess=class(TBIFilter);
  TRemoveMissingAccess=class(TRemoveMissing);

Constructor TBIQuery.Create(AOwner: TComponent);
begin
  inherited;

  FRemoveMissing:=TRemoveMissing.Create;
  TRemoveMissingAccess(FRemoveMissing).IChanged:=DoChanged;

  FDimensions:=TQueryDimensions.Create(Self);
  FMeasures:=TQueryMeasures.Create(Self);

  FFilter:=TBIFilter.Create;
  TBIFilterAccess(FFilter).IChanged:=DoChanged;

  FSort:=TQuerySort.Create(Self);
end;

Destructor TBIQuery.Destroy;
begin
  FSort.Free;
  FFilter.Free;
  FMeasures.Free;
  FDimensions.Free;
  FRemoveMissing.Free;

  inherited;
end;

Constructor TBIQuery.From(const AOwner:TComponent; const ASelect: TDataSelect);
begin
  Create(AOwner);
  Assign(ASelect);
end;

Constructor TBIQuery.From(const AOwner:TComponent; const ASummary: TSummary);
begin
  Create(AOwner);
  Assign(ASummary);
end;

type
  TGroupByAccess=class(TGroupBy);

function StyleOfBy(const AGroup:TGroupBy):TDimensionStyle;
begin
  if TGroupByAccess(AGroup).RealLayout=TGroupByLayout.Items then
     result:=TDimensionStyle.Column
  else
     result:=TDimensionStyle.Row
end;

procedure TBIQuery.GetItems(const AData: TDataItem);
begin
  if (not Changing) and (not ILoading) then
     AData.Load;
end;

function TBIQuery.IsDimensionsStored: Boolean;
begin
  result:=Dimensions.Count>0;
end;

function TBIQuery.IsMeasuresStored: Boolean;
begin
  result:=Measures.Count>0;
end;

function TBIQuery.IsSortStored: Boolean;
begin
  result:=SortBy.Count>0;
end;

type
  TDataCursorAccess=class(TDataCursor);
  TDataSelectAccess=class(TDataSelect);
  TSummaryAccess=class(TSummary);

function TBIQuery.CreateProvider:TDataProvider;

  procedure AddMeasures(const ASum:TSummary);
  var t : Integer;
      tmp : TQueryMeasure;
      tmpMeasure : TMeasure;
  begin
    for t:=0 to Measures.Count-1 do
    begin
      tmp:=Measures[t];

      if tmp.Enabled and (tmp.RealData<>nil) then
      begin
        if tmp.Expression=nil then
           tmpMeasure:=ASum.Measures.Add(tmp.RealData,tmp.Aggregate)
        else
           tmpMeasure:=ASum.Measures.Add(tmp.Expression,tmp.Aggregate);

        tmpMeasure.Calculation:=tmp.Measure.Calculation;
        tmpMeasure.Missing:=tmp.Measure.Missing;
      end;
    end;
  end;

  procedure AddDimensions(const ASum:TSummary);
  var t : Integer;
      tmp : TQueryDimension;
      tmpDim : TGroupBy;
  begin
    for t:=0 to Dimensions.Count-1 do
    begin
      tmp:=Dimensions[t];

      if tmp.Enabled and (tmp.RealData<>nil) then
      begin
        if tmp.Expression=nil then
           tmpDim:=ASum.By.Add(tmp.RealData)
        else
           tmpDim:=ASum.By.Add(tmp.Expression);

        tmpDim.Layout:=tmp.Layout;
        tmpDim.DatePart:=tmp.GroupBy.DatePart;
        tmpDim.Histogram:=tmp.GroupBy.Histogram;
      end;
    end;
  end;

  function CreateSummary:TSummary;
  begin
    result:=TSummary.Create(nil);
    result.RemoveMissing:=FRemoveMissing;

    AddDimensions(result);
    AddMeasures(result);

    result.SortBy:=FSort.Sort;

    TSummaryAccess(result).SetDirectFilter(FFilter.Filter);

    if result.Valid then
       Style:=TQueryStyle.Summary;
  end;

  function CreateSelect:TDataSelect;
  var t : Integer;
      tmp : TQueryDimension;
  begin
    result:=TDataSelect.Create(nil);
    result.Distinct:=FDistinct;
    result.Max:=FMax;;
    result.Start:=FStart;

    for t:=0 to Dimensions.Count-1 do
    begin
      tmp:=Dimensions[t];

      if tmp.Enabled then
      begin
        if tmp.RealData=nil then
        begin
          if tmp.Expression<>nil then
             result.Add(tmp.Expression);
        end
        else
           result.Add(tmp.RealData);
      end;
    end;

    TDataSelectAccess(result).SetDirectFilter(FFilter.Filter);

    result.SortBy:=FSort.Sort;

    if result.DataItems<>nil then
       Style:=TQueryStyle.Select;
  end;

begin
  Style:=TQueryStyle.Unknown;

  if Measures.EnabledCount>0 then
     result:=CreateSummary
  else
  if Dimensions.EnabledCount>0 then
     result:=CreateSelect
  else
     result:=nil;
end;

type
  TDataProviderAccess=class(TDataProvider);
  TDataAccess=class(TDataItem);

procedure TBIQuery.Load(const AData: TDataItem; const Children: Boolean);
var tmp : TDataProvider;
begin
  if (not (csLoading in ComponentState)) and (not Changing) and (not IClearing) then
  begin
    ILoading:=True;
    try
      IMain:=nil;

      tmp:=CreateProvider;

      if tmp=nil then
         DoClearData(AData)
      else
      try
        TDataProviderAccess(tmp).Load(AData,Children);

        if tmp is TSummary then
           IMain:=TSummary(tmp).MainData
        else
        if tmp is TDataSelect then
           IMain:=TDataSelect(tmp).MainData;

      finally
        tmp.Free;
      end;
    finally
      ILoading:=False;
    end;
  end;
end;

procedure TBIQuery.Loaded;
var t : Integer;
begin
  inherited;

  BeginUpdate;

  // Just in case Active was set before Data
  for t:=0 to Dimensions.Count-1 do
      Dimensions[t].Loaded;

  for t:=0 to Measures.Count-1 do
      Measures[t].Loaded;

  EndUpdate;
end;

procedure TBIQuery.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if not (csDestroying in ComponentState) then
     if Operation=TOperation.opRemove then
     begin
       Dimensions.Removed(AComponent);
       Measures.Removed(AComponent);
     end;
end;

procedure TBIQuery.AddDimensions(const ASummary:TSummary);
var t : Integer;
    tmp : TGroupBy;
begin
  for t:=0 to ASummary.By.Count-1 do
  begin
    tmp:=ASummary.By[t];
    Dimensions.Add(tmp).Style:=StyleOfBy(tmp);
  end;
end;

procedure TBIQuery.AddMeasures(const ASummary:TSummary);
var t : Integer;
begin
  for t:=0 to ASummary.Measures.Count-1 do
      Measures.Add(ASummary.Measures[t]);
end;

procedure TBIQuery.Parse(const AData:TDataItem; const SQL: String; const AError:TBIErrorProc=nil);
var tmp : TDataItem;
begin
  tmp:=TBISQL.From(AData,SQL,nil,AError);

  if tmp<>nil then
  try
    Clear;

    if tmp.Provider<>nil then
       Assign(tmp.Provider);

  finally
    //TDataAccess(tmp).KeepProvider:=True; <--- why? (mem leak)
    tmp.Free;
  end;
end;

procedure TBIQuery.Refresh;
begin
  Changed;
end;

procedure TBIQuery.Assign(Source: TPersistent);

  procedure AssignQuery(const AQuery:TBIQuery);
  begin
    BeginUpdate;
    try
      Clear;

      Dimensions:=AQuery.Dimensions;
      Measures:=AQuery.Measures;
      Filter:=AQuery.Filter;

      RemoveMissing:=AQuery.RemoveMissing;
      FDistinct:=AQuery.Distinct;
      FMax:=AQuery.MaxRows;
      FStart:=AQuery.StartRow;

      SortBy:=AQuery.SortBy;
    finally
      EndUpdate;
    end;
  end;

  procedure AssignSummary(const ASummary:TSummary);
  begin
    ASummary.Prepare;
    TSummaryAccess(ASummary).GuessByLayout;

    Clear;

    RemoveMissing:=ASummary.RemoveMissing;
    AddDimensions(ASummary);
    AddMeasures(ASummary);

    TBIFilterAccess(FFilter).ICustom:=ASummary.Filter;

    // !! ASummary.Having
  end;

  procedure AssignSelect(const ASelect:TDataSelect);
  var t : Integer;
  begin
    Clear;

    FDistinct:=ASelect.Distinct;
    FMax:=ASelect.Max;
    FStart:=ASelect.Start;

    for t:=0 to ASelect.Items.Count-1 do
        FDimensions.Add(ASelect.Items[t].Data,TDimensionStyle.Row,ASelect.Items[t].Active);

    SortBy.CopyFrom(ASelect.SortBy);

    TBIFilterAccess(FFilter).ICustom:=ASelect.Filter;

    Changed;
  end;

begin
  if Source=nil then
     Clear
  else
  if Source is TBIQuery then
     AssignQuery(TBIQuery(Source))
  else
  if Source is TSummary then
     AssignSummary(TSummary(Source))
  else
  if Source is TDataSelect then
     AssignSelect(TDataSelect(Source));

  inherited;
end;

function TBIQuery.Calculate: TDataItem;
begin
  result:=TDataItem.Create;
  Load(result,True);
end;

class function TBIQuery.CanBeMeasure(const AData: TDataItem): Boolean;
var tmp : TSummaryItemType;
begin
  tmp:=TSummaryItem.GuessType(AData);
  result:=(tmp=TSummaryItemType.Measure) or
          (tmp=TSummaryItemType.GroupOrMeasure) or
          AData.Primary; // <-- accept "ID" items, just in case
end;

procedure TBIQuery.Clear;
begin
  IMain:=nil;

  FMeasures.Clear;
  FDimensions.Clear;
  FSort.Clear;

  FFilter.Clear;
  RemoveMissing.Rows:=False;
  RemoveMissing.Columns:=False;

  FDistinct:=False;
  FMax:=0;
  FStart:=0;
end;

procedure TBIQuery.SetDimensions(const Value: TQueryDimensions);
begin
  FDimensions.Assign(Value);
end;

procedure TBIQuery.SetDistinct(const Value: Boolean);
begin
  if Distinct<>Value then
  begin
    FDistinct:=Value;
    Changed;
  end;
end;

(*
{$IFDEF FPC}
var
  IQuery : TBIQuery;

function CallOnError(const APos:Integer; const AMessage:String):Boolean;
begin
  result:=Assigned(IQuery.FOnError) and IQuery.FOnError(APos,AMessage);
end;
{$ENDIF}

procedure TBIQuery.DoSetFilter(const Value: String);

  function GetFilter(const AMain:TDataItem):TExpression;
  begin
    {$IFDEF FPC}
    IQuery:=Self;
    {$ENDIF}

    result:=TDataFilter.FromString(AMain,Value,
      {$IFDEF FPC}
      CallOnError
      {$ELSE}
      function(const APos:Integer; const AMessage:String):Boolean
      begin
        result:=Assigned(FOnError) and FOnError(APos,AMessage);
      end
      {$ENDIF});
  end;

begin
  TSummaryAccess(Summary).SetDirectFilter(nil);
  TDataCursorAccess(Select).SetDirectFilter(nil);

  if Style=TQueryStyle.Summary then
     Summary.Filter:=GetFilter(Summary.MainData)
  else
  if Style=TQueryStyle.Select then
     Select.Filter:=GetFilter(Select.MainData)
  else
     IFilter:=Value; // raise ?
end;
*)

procedure TBIQuery.DoChanged(Sender: TObject);
begin
  Changed;
end;

procedure TBIQuery.DoClearData(const AData:TDataItem);
begin
  if not IClearing then // <-- avoid re-entrancy
  begin
    IClearing:=True;

    if not ILoading then
       BeginUpdate;
    try
      AData.Clear;
    finally
      if not ILoading then
         EndUpdate;

      IClearing:=False;
    end;
  end;
end;

procedure TBIQuery.Changed;
begin
  if not (csReading in ComponentState) then

  if not ILoading then
  begin
    if FConsumers.Changing=0 then
    begin
      if FData<>nil then
      begin
        ILoading:=True;
        try
          DoClearData(FData);
        finally
          ILoading:=False;
        end;
      end;

      inherited;
    end;
  end;
end;

procedure TBIQuery.SetMeasures(const Value: TQueryMeasures);
begin
  FMeasures.Assign(Value);
end;

procedure TBIQuery.SetFilter(const Value: TBIFilter);
begin
  FFilter.Assign(Value);
end;

procedure TBIQuery.SetMax(const Value: Int64);
begin
  if MaxRows<>Value then
  begin
    FMax:=Value;
    Changed;
  end;
end;

procedure TBIQuery.SetSort(const Value: TQuerySort);
begin
  FSort.Assign(Value);
end;

procedure TBIQuery.SetStartRow(const Value: Int64);
begin
  if StartRow<>Value then
  begin
    FStart:=Value;
    Changed;
  end;
end;

procedure TBIQuery.SetRemoveMissing(const Value: TRemoveMissing);
begin
  RemoveMissing.Assign(Value);
  Changed;
end;

function TBIQuery.ToString: String;
var tmp : TDataProvider;
begin
  tmp:=CreateProvider;
  try
    if (tmp is TSummary) and TSummary(tmp).Valid then
       result:=tmp.ToString
    else
    if (tmp is TDataSelect) and (TDataSelect(tmp).DataItems<>nil) then
       result:=tmp.ToString
    else
       result:='';
  finally
    tmp.Free;
  end;
end;

{ TQueryItem }

Constructor TQueryItem.Create(Collection: TCollection);
begin
  inherited;
  FEnabled:=True;
end;

type
  TCollectionAccess=class(TCollection);

procedure TQueryItem.Changed;
begin
  if TCollectionAccess(Collection).UpdateCount=0 then
  begin
    inherited;
    Query.Changed;
  end;
end;

procedure TQueryItem.DoChanged(Sender: TObject);
begin
  Changed;
end;

procedure TQueryItem.Assign(Source: TPersistent);
begin
  inherited;

  if Source is TQueryItem then
  begin
    FEnabled:=TQueryItem(Source).FEnabled;
    Data:=TQueryItem(Source).Data;
  end;
end;

(*
function TQueryItem.CanChange:Boolean;
begin
  result:=(Collection<>nil) and
          (not (csLoading in Query.ComponentState)) and
          (not Query.Changing);
end;

procedure TQueryItem.SetActive(const Value: Boolean);
begin
  if Active<>Value then
  begin
    IActive:=Value;

    if FSelectIndex=-1 then
    begin
      if FSummaryItem<>nil then
         FSummaryItem.Active:=Value;
    end
    else
      Query.Select.Items[FSelectIndex].Active:=Value;

    if CanChange then
       Changed;
  end;
end;
*)

procedure TQueryItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled<>Value then
  begin
    FEnabled:=Value;
    Changed;
  end;
end;

(*
procedure TQueryItem.SetExpression(const Value: TExpression);
begin
  if FSummaryItem=nil then
     IExpression:=Value // <-- temp
  else
     FSummaryItem.Expression:=Value;

  if CanChange then
     Changed;
end;

procedure TQueryItem.Recreate;
begin
  IActive:=Active;

  DoRemove;
  Query.Items.SetupItem(Self,False,IActive);
end;

procedure TQueryItem.ValidateData(const AData:TDataItem);
begin
  if (AData<>nil) and (RealStyle=TQueryItemStyle.Measure) then
     if not TBIQuery.CanBeMeasure(AData) then
        raise EBIException.Create('Error: Data: '+AData.FullName+' cannot be a summary measure');

     //if FData=nil then
     //   FStyle:=TQueryItemStyle.Automatic // <-- allow in this case (FData=nil)
     //else
end;

procedure TQueryItem.DoRemove;
begin
  if FSummaryItem<>nil then
  begin
    if FSummaryItem is TMeasure then
       Query.Summary.Measures.Remove(TMeasure(FSummaryItem))
    else
       Query.Summary.By.Remove(TGroupBy(FSummaryItem));

    FSummaryItem:=nil;
  end;

  if FSelectIndex<>-1 then
  begin
    Query.DeleteSelect(FSelectIndex);
    FSelectIndex:=-1;
  end;
end;
*)

{ TQueryItem }

function TQueryItem.GetQuery: TBIQuery;
begin
  result:= TBIQuery(Collection.Owner);
end;

{ TQueryDimensions }

Constructor TQueryDimensions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner,TQueryDimension);
end;

function TQueryDimensions.EnabledCount:Integer;
var t : Integer;
begin
  result:=0;

  for t:=0 to Count-1 do
      if Items[t].Enabled and
         ( (Items[t].RealData<>nil) or (Items[t].Expression<>nil) ) then
            Inc(result);
end;

procedure TQueryDimensions.Exchange(const A, B: TQueryDimension);
begin
  A.Index:=B.Index;
end;

(*
var tmp,
    tmpA,
    tmpB : Integer;
begin
  if A<>B then
  begin
    A.Index:=B.Index;

    if A.FSelectIndex<>-1 then
    begin
      Query.Select.Items.Exchange(A.FSelectIndex,B.FSelectIndex);

      tmp:=A.FSelectIndex;
      A.FSelectIndex:=B.FSelectIndex;
      B.FSelectIndex:=tmp;
    end
    else
    if A.FSummaryItem<>nil then
       if A.FSummaryItem is TGroupBy then
       begin
         tmpA:=Query.Summary.By.IndexOf(A.FSummaryItem as TGroupBy);
         tmpB:=Query.Summary.By.IndexOf(B.FSummaryItem as TGroupBy);

         Query.Summary.By.Exchange(tmpA,tmpB);
       end
       else
       begin
         tmpA:=Query.Summary.Measures.IndexOf(A.FSummaryItem as TMeasure);
         tmpB:=Query.Summary.Measures.IndexOf(B.FSummaryItem as TMeasure);

         Query.Summary.Measures.Exchange(tmpA,tmpB);
       end;

    if not Query.Changing then
       Query.Changed;
  end;
end;
*)
procedure TQueryDimensions.Swap;
var t : Integer;
    tmp : TQueryDimension;
begin
  Query.BeginUpdate;

  for t:=0 to Count-1 do
  begin
    tmp:=Items[t];

    case tmp.RealStyle of
       TDimensionStyle.Row: tmp.Style:=TDimensionStyle.Column;
    TDimensionStyle.Column: tmp.Style:=TDimensionStyle.Row;
     end;
  end;

  Query.EndUpdate;
end;

function TQueryDimensions.Add(const AGroupBy: TGroupBy): TQueryDimension;
begin
  result:=(inherited Add) as TQueryDimension;
  result.GroupBy.Assign(AGroupBy);
  result.Data:=AGroupBy.RealData;
end;

function TQueryDimensions.AddItem(const AData: TDataItem;
                                  const AStyle: TDimensionStyle):TQueryDimension;
begin
  result:=TQueryDimension(inherited Add);

  result.FStyle:=AStyle;
  result.Data:=AData;

  TGroupByAccess(result.GroupBy).Source:=result.GroupBy.Expression; // TDataItemExpression.Create(AData);
end;

procedure ErrorDataNil(const AMessage:String);
begin
  raise EBIException.Create('Error: Data is nil at: '+AMessage);
end;

function TQueryDimensions.Add(const AData: TDataItem;
                         const AStyle: TDimensionStyle;
                         const IsActive:Boolean): TQueryDimension;
begin
  if AData=nil then
     ErrorDataNil('Query Dimensions');

  Query.BeginUpdate;
  try
    result:=AddItem(AData,AStyle);
    result.FEnabled:=IsActive;
  finally
    Query.EndUpdate;
  end;
end;

function TQueryDimensions.Add(const AData:TDataItem;
                              const AExpression:String;
                              const AStyle:TDimensionStyle): TQueryDimension;
begin
  result:=Add(nil,AStyle);
  result.Expression:=TDataExpression.FromString(AData,AExpression);
end;

function TQueryDimensions.Get(const Index: Integer): TQueryDimension;
begin
  result:=TQueryDimension(inherited GetItem(Index));
end;

function TQueryDimensions.IndexOf(const ADimension: TQueryDimension): Integer;
var t : Integer;
begin
  for t:=0 to Count-1 do
      if Items[t]=ADimension then
         Exit(t);

  result:=-1;
end;

procedure TQueryDimensions.Put(const Index: Integer; const Value: TQueryDimension);
begin
  inherited Items[Index]:=Value;
end;

procedure TQueryDimensions.Removed(const AComponent: TComponent);
var t : Integer;
begin
  for t:=0 to Count-1 do
      if Items[t].Provider=AComponent then
         Items[t].Provider:=nil;
end;

{ TQueryMeasures }

Constructor TQueryMeasures.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner,TQueryMeasure);
end;

function TQueryMeasures.Add(const AMeasure: TMeasure): TQueryMeasure;
begin
  result:=(inherited Add) as TQueryMeasure;
  result.Measure.Assign(AMeasure);
  result.Data:=AMeasure.RealData;
end;

function TQueryMeasures.EnabledCount:Integer;
var t : Integer;
begin
  result:=0;

  for t:=0 to Count-1 do
      if Items[t].Enabled and (Items[t].RealData<>nil) then
         Inc(result);
end;

procedure TQueryMeasures.Exchange(const A, B: TQueryMeasure);
begin
  A.Index:=B.Index;
end;

function TQueryMeasures.Get(const Index: Integer): TQueryMeasure;
begin
  result:=TQueryMeasure(inherited GetItem(Index));
end;

function TQueryMeasures.IndexOf(const AMeasure: TQueryMeasure): Integer;
var t : Integer;
begin
  for t:=0 to Count-1 do
      if Items[t]=AMeasure then
         Exit(t);

  result:=-1;
end;

procedure TQueryMeasures.Put(const Index: Integer; const Value: TQueryMeasure);
begin
  inherited Items[Index]:=Value;
end;

procedure TQueryMeasures.Removed(const AComponent: TComponent);
var t : Integer;
begin
  for t:=0 to Count-1 do
      if Items[t].Provider=AComponent then
         Items[t].Provider:=nil;
end;

type
  TMeasureAccess=class(TMeasure);

function TQueryMeasures.Add(const AData: TDataItem; const AMeasure: TAggregate;
                            const IsActive:Boolean): TQueryMeasure;
begin
  if AData=nil then
     ErrorDataNil('Query Measures');

  Query.BeginUpdate;
  try
    result:=Add as TQueryMeasure;
    result.Data:=AData;

    if result.Measure.Expression=nil then
       TMeasureAccess(result.Measure).Source:=TDataItemExpression.Create(AData);

    result.Aggregate:=AMeasure;
    result.FEnabled:=IsActive;
  finally
    Query.EndUpdate;
  end;
end;

{ TQueryDimension }

Constructor TQueryDimension.Create(Collection: TCollection);
begin
  inherited;

  FGroupBy:=TGroupBy.Create;
  TGroupByAccess(FGroupBy).IChanged:=DoChanged;
end;

Destructor TQueryDimension.Destroy;
begin
  FGroupBy.Free;
  inherited;
end;

procedure TQueryDimension.Assign(Source:TPersistent);
begin
  if Source is TQueryDimension then
  begin
    FGroupBy.Assign(TQueryDimension(Source).FGroupBy);
    FStyle:=TQueryDimension(Source).FStyle;
  end;

  inherited;
end;

function TQueryDimension.GetDatePart: TDateTimePart;
begin
  result:=FGroupBy.DatePart;
end;

function TQueryDimension.GetExpression: TExpression;
begin
  result:=FGroupBy.Expression;
end;

function TQueryDimension.GetHistogram: THistogram;
begin
  result:=FGroupBy.Histogram;
end;

function TQueryDimension.Layout: TGroupByLayout;
begin
  case FStyle of
          Row: result:=TGroupByLayout.Rows;
       Column: result:=TGroupByLayout.Items;
  else
    {Automatic:} result:=TGroupByLayout.Automatic;
  end;
end;

function TQueryDimension.RealData: TDataItem;
begin
  result:=Data;

  if result=nil then
     result:=FGroupBy.RealData;
end;

function TQueryDimension.RealStyle: TDimensionStyle;
begin
  if FStyle=TDimensionStyle.Automatic then
     result:=StyleOfBy(FGroupBy)
  else
     result:=FStyle;
end;

procedure TQueryDimension.SetData(const Value: TDataItem);
var tmp : Boolean;
begin
  tmp:=FGroupBy.RealData<>Value;

  inherited;

  if tmp then
     FGroupBy.Expression:=TDataItemExpression.Create(Value);
end;

procedure TQueryDimension.SetDatePart(const Value: TDateTimePart);
begin
  FGroupBy.DatePart:=Value;
end;

procedure TQueryDimension.SetExpression(const Value: TExpression);
begin
  // Data:=nil;

  FGroupBy.Expression:=Value;
  Changed;
end;

procedure TQueryDimension.SetHistogram(const Value: THistogram);
begin
  FGroupBy.Histogram:=Value;
  Changed;
end;

procedure TQueryDimension.SetStyle(const Value: TDimensionStyle);
begin
  if FStyle<>Value then
  begin
    FStyle:=Value;
    Changed;
  end;
end;

function TQueryDimension.ToString:String;
begin
  result:=FGroupBy.ToString;

  if FGroupBy.Histogram.Active then
     result:=result+' (histogram)';
end;

{ TQueryMeasure }

Constructor TQueryMeasure.Create(Collection: TCollection);
begin
  inherited;

  FMeasure:=TMeasure.Create;
  TMeasureAccess(FMeasure).IChanged:=DoChanged;
end;

Destructor TQueryMeasure.Destroy;
begin
  FMeasure.Free;
  inherited;
end;

procedure TQueryMeasure.Assign(Source: TPersistent);
begin
  if Source is TQueryMeasure then
     FMeasure.Assign(TQueryMeasure(Source).FMeasure);

  inherited;
end;

function TQueryMeasure.GetAggregate: TAggregate;
begin
  result:=FMeasure.Aggregate;
end;

function TQueryMeasure.GetCalculation: TMeasureCalculation;
begin
  result:=FMeasure.Calculation;
end;

function TQueryMeasure.GetExpression: TExpression;
begin
  result:=FMeasure.Expression;
end;

function TQueryMeasure.GetMissing: TMeasureMissing;
begin
  result:=FMeasure.Missing;
end;

function TQueryMeasure.RealData: TDataItem;
begin
  result:=Data; //FMeasure.RealData;
end;

procedure TQueryMeasure.SetAggregate(const Value: TAggregate);
begin
  if Aggregate<>Value then
  begin
    FMeasure.Aggregate:=Value;
    Changed;
  end;
end;

procedure TQueryMeasure.SetCalculation(const Value: TMeasureCalculation);
begin
  Calculation.Assign(Value);
  Changed;
end;

procedure TQueryMeasure.SetData(const Value: TDataItem);
var tmp : Boolean;
begin
  tmp:=FMeasure.RealData<>Value;

  inherited;

  if tmp then
     FMeasure.Expression:=TDataItemExpression.Create(Value);
end;

procedure TQueryMeasure.SetExpression(const Value: TExpression);
begin
  FMeasure.Expression:=Value;
  Changed;
end;

procedure TQueryMeasure.SetMissing(const Value: TMeasureMissing);
begin
  Missing.Assign(Value);
  Changed;
end;

function TQueryMeasure.ToString: String;
begin
  result:=FMeasure.ToString;
end;

{ TQueryCollection }

function TQueryCollection.GetQuery: TBIQuery;
begin
  result:= TBIQuery(Owner);
end;

procedure TQueryCollection.DoChanged;
begin
  if not IUpdating then
     Query.Changed;
end;

procedure TQueryCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;

  if Action<>cnExtracting then
     if not (csDestroying in Query.ComponentState) then
        if not (csLoading in Query.ComponentState) then
           DoChanged;
end;

procedure TQueryCollection.Update(Item: TCollectionItem);
begin
  inherited;
  DoChanged;
end;

{ TQuerySort }

constructor TQuerySort.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner,TQuerySortItem);
end;

function TQuerySort.AddSort(const AData: TDataItem; const Ascending,
  IgnoreTextCase: Boolean):TQuerySortItem;
begin
  IUpdating:=True;
  try
    result:=TQuerySortItem(inherited Add);

    result.Data:=AData;
    result.Ascending:=Ascending;
    result.IgnoreTextCase:=IgnoreTextCase;
  finally
    IUpdating:=False;
    Changed;
  end;
end;

procedure TQuerySort.CopyFrom(const ASort: TSortItems);
var tmp : TSortItem;
begin
  Clear;

  for tmp in ASort.Items do
      AddSort(tmp.Data,tmp.Descending,tmp.IgnoreTextCase);
end;

function TQuerySort.Get(const Index: Integer): TQuerySortItem;
begin
  result:=TQuerySortItem(inherited Items[Index]);
end;

function TQuerySort.ItemOf(const AData: TDataItem): TQuerySortItem;
var t : Integer;
begin
  for t:=0 to Count-1 do
      if Items[t].Data=AData then
         Exit(Items[t]);

  result:=nil;
end;

procedure TQuerySort.Put(const Index: Integer; const Value: TQuerySortItem);
begin
  inherited Items[Index]:=Value;
end;

function TQuerySort.Sort: TSortItems;
var t,
    L : Integer;
    tmp : TQuerySortItem;
begin
  result.Items:=nil;

  L:=0;

  for t:=0 to Count-1 do
  begin
    tmp:=Items[t];

    if tmp.Enabled and (tmp.Data<>nil) then
    begin
      SetLength(result.Items,L+1);

      result.Items[L].Active:=True;
      result.Items[L].Data:=tmp.Data;
      result.Items[L].Descending:=not tmp.Ascending;
      result.Items[L].IgnoreTextCase:=tmp.IgnoreTextCase;

      Inc(L);
    end;
  end;
end;

{ TQuerySortItem }

procedure TQuerySortItem.Assign(Source: TPersistent);
begin
  if Source is TQuerySortItem then
  begin
    FAscending:=TQuerySortItem(Source).FAscending;
    FIgnoreTextCase:=TQuerySortItem(Source).FIgnoreTextCase;
  end;

  inherited;
end;

Constructor TQuerySortItem.Create(ACollection: TCollection);
begin
  inherited;
  FAscending:=True;
  FIgnoreTextCase:=True;
end;

procedure TQuerySortItem.SetAscending(const Value: Boolean);
begin
  if FAscending<>Value then
  begin
    FAscending:=Value;
    Changed;
  end;
end;

procedure TQuerySortItem.SetIgnoreTextCase(const Value: Boolean);
begin
  if FIgnoreTextCase<>Value then
  begin
    FIgnoreTextCase:=Value;
    Changed;
  end;
end;

end.
