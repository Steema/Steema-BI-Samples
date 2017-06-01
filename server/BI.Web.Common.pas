{*********************************************}
{  TeeBI Software Library                     }
{  Web Server (VCL and Firemonkey)            }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.Common;

interface

uses
  System.Classes, System.Diagnostics, System.SyncObjs,
  BI.Web.AllData, BI.DataItem, BI.DataSource, BI.Expression, BI.Arrays,
  BI.Arrays.Strings,
  BI.Summary, BI.Persist, BI.Info,

  System.UITypes;

type
  // Abstract class (see BI.Web.IndyContext for implementation)
  TBIWebContext=class
  public
    Context : TObject;
    ContentText: String;
    ContentType: String;
    ResponseInfo: TObject;
    RequestInfo: TObject;
    ResponseStream: TStream;

    procedure AddCookie(const AName,AValue:String); virtual; abstract;
    function FormParams: String; virtual; abstract;
    function GetContentType:String; virtual; abstract;
    function GetCookie(const AName:String):String; virtual; abstract;
    function GetDocument:String; virtual; abstract;
    function GetStream:TStream; virtual; abstract;
    function Headers: TStrings; virtual; abstract;
    function Params:TStrings; virtual; abstract;
    procedure Redirect(const AURL: String); virtual; abstract;
    function ResponseSize: Int64; virtual; abstract;
    procedure ReturnFile(const AFile:String); virtual; abstract;
    procedure ReturnIcon(const AStream:TStream); virtual; abstract;
    procedure SetResponse(const AText: String); overload; virtual; abstract;
    procedure SetResponse(const AType: String; const AStream:TStream); overload; virtual; abstract;
  end;

  THistoryProc=procedure(const AContext:TBIWebContext;
                         const Command:String;
                         const Tag:String;
                         const Success:Boolean;
                         const Millisec:Integer;
                         const Size:Int64) of object;

  TWebLogs=class
  private
    Lock : TCriticalSection;

    procedure SetCurrentFile;
  public
    const
      LogExtension='.bilog';

    var
    AddHistory : THistoryProc;
    History : TDataItem;

    CurrentFile : String;
    Persist : Boolean;
    Store : String;

    Constructor Create;
    Destructor Destroy; override;

    procedure TrySave;
  end;

  TImportScheduler=class
  private
    FData : TDataItem;
    FStore : String;

    procedure AddAll;
    function Definition(const Index:Integer):TDataDefinition;
    function GetData: TDataItem;
    function NextRefresh(const Index:Integer):TDateTime;
    procedure TryReImport(const Index:Integer);
  public
    Enabled : Boolean;

    Destructor Destroy; override;

    procedure Process;
    procedure Refresh(const AStore:String);

    property Data:TDataItem read GetData;
  end;

  TBIWebCommon=class
  private
    T1 : TStopWatch;

    class function CalcTotals(const ACursor:TDataCursor;
                              const Aggregate:TAggregate):TDataItem; static;

    class function CheckZip(const AStream:TStream; const Params:TStrings):TStream; static;

    class function CursorToText(var ACursor:TDataCursor; const AFormat:String;
                          const GetText:TExportGetText=nil;
                          const ATotals:TDataItem=nil):String; static;

    procedure DataLink(const Sender:TBIExport; const AData:TDataItem;
                             const AIndex:TInteger; var Text:String);

    procedure DoAddHistory(const AContext: TBIWebContext;
                           const Command:String;
                           const Tag:String=''; const Success:Boolean=True);

    class function GetCSS(const AName:String):String; static;

    class function GetFilter(const AData:TDataItem; const AFilter:String):TExpression; static;

    procedure PrepareCursor(const ACursor:TDataCursor; const AData:TDataItem;
                            const Params:TStrings);

    {$IFNDEF NODASHBOARD}
    function ProcessDashboard(const AContext: TBIWebContext):Boolean;
    {$ENDIF}

    procedure ProcessData(const AContext: TBIWebContext;
                          const Format:String;
                          const Params:TStrings);

    procedure ReturnCursor(const AContext:TBIWebContext;
                           const Params:TStrings;
                           var ACursor:TDataCursor; const AFormat:String;
                           const AURL:String='';
                           const GetText:TExportGetText=nil);

    procedure ReturnNormal(const AContext:TBIWebContext;
                           const AParams:TStrings;
                           const AFormat:String;
                           const AData:TDataItem;
                           const ACommand,ATag:String;
                           const FreeData:Boolean=True);

    procedure SetupPublicFolder;

    function TotalsOf(const Params:TStrings; const ACursor:TDataCursor):TDataItem;

  protected
    function GetDocument(const AContext: TBIWebContext):String; virtual;

    procedure ProcessSingleData(const AData,AStore:String;
                                const AContext: TBIWebContext;
                                const Format:String;
                                const Params:TStrings);
  public
    type
      TWebPublic=record
      private
        FPath : String;
        RealPath : String;

        procedure SetPath(const Value: String);
      public
        Enabled : Boolean;

        function PathOf(const AFileName:String):String;

        property Path : String read FPath write SetPath;
      end;

    var
    Data : TAllData;
    Logs : TWebLogs;
    PublicFolder : TWebPublic;
    Scheduler : TImportScheduler;

    Constructor Create;
    Destructor Destroy; override;

    function QueryOf(const AQuery,AData,AFilter,ADistinct: String): TDataItem;
    function SummaryOf(const ASummary,AData,AHaving,AFilter:String):TDataItem;

    procedure ProcessFile(const ADocument:String; const AContext: TBIWebContext); virtual;

    procedure ProcessGet(const AContext: TBIWebContext);
    procedure ProcessPost(const AContext: TBIWebContext);
  end;

  TBIWebCommonClass=class of TBIWebCommon;

implementation

uses
  {System.}SysUtils, {System.}IOUtils, {System.}Types,
  BI.SQL, BI.Html, BI.Languages.English,

  BI.Web.Html,

  {$IFNDEF NODASHBOARD}
  BI.Dashboard,
  BI.Dashboard.HTML,
  BI.Dashboard.Loader,
  {$ENDIF}

  BI.Expressions,

  {$IFNDEF NOTEECHART}

  {$IFDEF LINUX}
  {$IFDEF FPC}
  {$DEFINE TEECHART}
  {$ENDIF}
  {$ELSE}
  {$DEFINE TEECHART}
  {$ENDIF}

  {$IFDEF TEECHART}
  BI.Web.Common.Chart,
  {$ENDIF}

  {$ENDIF}

  BI.UI;

Constructor TBIWebCommon.Create;
begin
  inherited Create;
  Logs:=TWebLogs.Create;
  Scheduler:=TImportScheduler.Create;
  SetupPublicFolder;
end;

Destructor TBIWebCommon.Destroy;
begin
  Scheduler.Free;
  Logs.Free;
  inherited;
end;

procedure TBIWebCommon.SetupPublicFolder;
begin
  PublicFolder.Enabled:=TBIRegistry.ReadBoolean('BIWeb','PublicEnabled',True);
  PublicFolder.Path:=TBIRegistry.ReadString('BIWeb','PublicFolder','public');
end;

class function TBIWebCommon.CursorToText(var ACursor:TDataCursor; const AFormat:String;
                          const GetText:TExportGetText=nil;
                          const ATotals:TDataItem=nil):String;
var tmpClass : TBIExportClass;
    tmp : TBIExport;
begin
  result:='';

  tmpClass:=TBIExporters.GuessExtension(AFormat);

  if tmpClass<>nil then
  begin
    tmp:=tmpClass.Create;
    try
      if tmp.BinaryOnly then
         result:='' // cannot return as string
      else
      begin
        tmp.OnGetText:=GetText;
        tmp.Cursor:=ACursor;

        tmp.Totals:=ATotals;  // grand totals

        // tmp.Layout:=ALayout; // record or table?
        result:=tmp.AsString;
      end;
    finally
      tmp.Free;
    end;
  end;
end;

class function TBIWebCommon.GetCSS(const AName:String):String;
begin
  if (AName='') or SameText(AName,'pure') then
     result:=THTMLHelper.PureCSS+TCommonUI.CRLF+
             '<meta name="viewport" content="width=device-width, initial-scale=1">'+TCommonUI.CRLF
  else
     result:='';
end;

function DeleteParam(const AURL,AParam:String):String;
var i, i2 : Integer;
begin
  result:=AURL;

  i:=Pos('&FORMAT=',UpperCase(result));

  if i>0 then
  begin
    Delete(result,i,8);
    i2:=Pos('&',Copy(result,i,Length(result)));

    if i2>0 then
       Delete(result,i,i2-i)
    else
       Delete(result,i,Length(result));
  end;
end;

type
  TDataItemAccess=class(TDataItem);

class function TBIWebCommon.CalcTotals(const ACursor:TDataCursor;
                                 const Aggregate:TAggregate):TDataItem;

  function CanAggregate(const AItem:TDataItem):Boolean;
  begin
    result:=AItem.Kind.IsNumeric and
           (not AItem.Primary) and
           (not TDataItemAccess(AItem).HasMaster);
  end;

var tmp : TSummary;
    tmpItems : TDataArray;
    tmpSum,
    tmpCalc,
    tmpOrigin,
    tmpItem : TDataItem;
    t,
    tmpPos : Integer;
begin
  result:=nil;

  tmp:=TSummary.Create(nil);
  try
    tmpItems:=ACursor.DataItems;

    if tmpItems=nil then
       tmpItems:=ACursor.Data.Items.AsArray;

    for t:=0 to High(tmpItems) do
    begin
      tmpItem:=tmpItems[t];

      if CanAggregate(tmpItem) then
         tmp.AddMeasure(tmpItem,Aggregate);
    end;

    if tmp.Valid then
    begin
      tmpSum:=tmp.Calculate;
      try
        result:=TDataItem.Create(True);

        tmpPos:=0;

        // Now insert empty items (the ones not measured)
        for t:=0 to High(tmpItems) do
        begin
          if CanAggregate(tmpItems[t]) then
          begin
            tmpOrigin:=tmpSum.Items[tmpPos];
            tmpCalc:=result.Items.Add(tmpItems[t].Name,tmpOrigin.Kind);
            tmpCalc.Resize(tmpOrigin.Count);
            tmpCalc.CopyFrom(0,tmpOrigin,0);
            Inc(tmpPos);
          end
          else
          begin
            tmpCalc:=result.Items.Add(tmpItems[t].Name,TDataKind.dkText);
            tmpCalc.Resize(1);

            if (tmpPos=0) and (t=0) then
               tmpCalc.TextData[0]:=Aggregate.ToString;
          end;
        end;

        result.Resize(1);

      finally
        tmpSum.Free;
      end;
    end;

  finally
    tmp.Free;
  end;
end;

function TBIWebCommon.TotalsOf(const Params:TStrings; const ACursor:TDataCursor):TDataItem;
var tmpAgg : String;
    tmpAggregate : TAggregate;
begin
  result:=nil;

  tmpAgg:=Params.Values['totals'];

  if tmpAgg<>'' then
     if TAggregate.FromString(tmpAgg,tmpAggregate) then
        result:=CalcTotals(ACursor,tmpAggregate)
     else
        raise EBIException.CreateFmt(BIMsg_Summary_WrongAggregate,[tmpAgg]);
end;

procedure TBIWebCommon.ReturnCursor(const AContext: TBIWebContext;
                    const Params:TStrings;
                    var ACursor:TDataCursor; const AFormat:String;
                    const AURL:String;
                    const GetText:TExportGetText);

  function URLFromParams(const Skip:String):String;
  var t : Integer;
      tmp : String;
  begin
    result:='';

    for t:=0 to Params.Count-1 do
    begin
      tmp:=Params.Names[t];

      if tmp='' then
         tmp:=Params[t];

      if not SameText(tmp,Skip) then
      begin
        if result<>'' then
           result:=result+'&';

        result:=result+tmp+'='+Params.Values[tmp];
      end;
    end;

    result:='?'+TBIHtmlHelper.Escape(result);
  end;

  function GetSearchCode(const ASearch:String):String;

    function MakeURL:String;
    begin
      if AURL='' then
         result:=URLFromParams('search')
      else
         result:=DeleteParam(AURL,'search');

      //result:=result+'&search="'+TBIHtmlHelper.Escape(ASearch)+'"';
    end;

  var tmp : String;
  begin
    tmp:='location.href='''+MakeURL+''+'&search=''+document.getElementById(''search'').value';

    result:='<input type="text" id="search" style="margin-left: 20px;" name="search" value="'+
            ASearch+'" placeholder="(type to search)" onkeydown="if (event.keyCode == 13) document.getElementById(''btnsearch'').click()"/>'+
            '<input type="submit" id="btnsearch" onclick="'+tmp+';" value="Search"/>';
  end;

  // From: http://schier.co/blog/2014/10/23/creating-a-pure-css-dropdown-using-the-hover-selector.html
  function GetExportCode:String;

    function MakeURL(const AFormat:String):String;
    begin
      if AURL='' then
         result:=URLFromParams('format')
      else
         result:=DeleteParam(AURL,'format');

      result:=result+'&format=.'+LowerCase(AFormat);

      result:=TBIHtmlHelper.Escape(result);
    end;

  var tmp : TBIExportClass;
      tmpS : String;
      tmpFilters : TFileFilters;
  begin
    result:='<style>.dropdown { position: relative; display: inline-block; z-index: 9999; }'+
      '.dropdown .dropdown-menu { position: absolute; top: 100%; display: none; margin: 0; list-style: none; width: 120%; padding: 0; }'+
      '.dropdown:hover .dropdown-menu { display: block; }'+
      '.dropdown button { background: #2362FF; color: #FFFFFF; border: none; margin: 0; padding: 0.4em 0.8em; font-size: 1em; }'+
      '.dropdown-wide button { min-width: 13em; }'+
      '.dropdown a { display: block; padding: 0.2em 0.8em; text-decoration: none; background: #CCCCCC; color: #333333; }'+
      '.dropdown a:hover { background: #BBBBBB; }</style>'+
      '<div class="dropdown"><button>'+BIMsg_Export+'</button><ul class="dropdown-menu">';

      for tmp in TBIExporters.Items do
      begin
        tmpFilters:=tmp.FileFilter;

        if tmpFilters<>nil then
        begin
          tmpS:=tmpFilters[0].FirstExtension;
          result:=result+'<li><a href="#" onclick="location.href='''+MakeURL(tmpS)+''';">'+UpperCase(tmpS)+'</a></li>';
        end;
      end;

      result:=result+'</ul></div>';
  end;

  function ExportHTMLHeader:String;
  var tmpButton,
      tmpButtonDisabled,

      tmpExport,
      tmpSearch,
      tmpNav : String;
  begin
    result:=GetCSS(Params.Values['css']);

    if result='' then
    begin
      TBIHtmlHelper.TableClass:='';

      tmpButton:='';
      tmpButtonDisabled:='';
    end
    else
    begin
      TBIHtmlHelper.TableClass:='pure-table pure-table-striped';

      tmpButton:='pure-button';
      tmpButtonDisabled:='pure-button pure-button-disabled';
    end;

    if AURL='' then
       tmpNav:=TBIHtmlHelper.Navigator(ACursor,URLFromParams('start'),tmpButton,tmpButtonDisabled)
    else
       tmpNav:=TBIHtmlHelper.Navigator(ACursor,AURL,tmpButton,tmpButtonDisabled);

    tmpExport:=GetExportCode;
    tmpSearch:=GetSearchCode(Params.Values['search']);

    if (tmpNav<>'') or (tmpExport<>'') or (tmpSearch<>'') then
       result:=result+tmpNav+tmpExport+tmpSearch+TBIHtmlHelper.Return;
  end;

var tmp : String;
    tmpTotals : TDataItem;
begin
  try
    {$IFDEF TEECHART}
    if not TBIWebCommonChart.ExportChart(AContext,ACursor,Params,AFormat) then
    {$ENDIF}
    begin
      if SameText(AFormat,'.htm') then
         tmp:=ExportHTMLHeader
      else
         tmp:='';

      tmpTotals:=TotalsOf(Params,ACursor);
      try
        AContext.ContentText:=tmp+CursorToText(ACursor,AFormat,GetText,tmpTotals);
      finally
        tmpTotals.Free;
      end;

      // Reset:
      TBIHtmlHelper.TableClass:='';
    end;
  finally
    ACursor.Filter:=nil;
  end;
end;

procedure TBIWebCommon.ProcessPost(const AContext: TBIWebContext);
var tmpS : String;
    Cursor : TDataCursor;
    tmpData : TDataItem;
begin
  tmpS:=AContext.Params.Values['data'];

  if tmpS<>'' then
  begin
    tmpData:=Data.Find(tmpS);

    if (not tmpData.AsTable) and (tmpData.Kind=TDataKind.dkUnknown) then
    begin
      AContext.ContentText:=
         'Data: '+tmpData.Name+TBIHtmlHelper.Return+TBIHtmlHelper.InitForm+
         'Items: '+TBIHtmlHelper.Combo('data',Data.GetData(tmpData.Items),
                  Data.GetDataOrigins(tmpData.Items))+TBIHtmlHelper.Return+
         TBIHtmlHelper.FinishForm('getdata');
    end
    else
    begin
      Cursor:=TDataCursor.Create(nil);
      try
        PrepareCursor(Cursor,tmpData,AContext.Params);

        ReturnCursor(AContext,AContext.Params,Cursor,'.htm','?data='+tmpS+'&format=.htm',DataLink);

        AContext.ContentText:=
            'Data: '+Cursor.Data.Name+TBIHtmlHelper.Return+TBIHtmlHelper.Return+
            AContext.ContentText;
      finally
        Cursor.Free;
      end;
    end;

    DoAddHistory(AContext,'post',tmpS,True);
  end
  {$IFNDEF NODASHBOARD}
  else
  if not ProcessDashboard(AContext) then
//     ProcessData(AContext,Format,Params);
  {$ENDIF}
end;

class function TBIWebCommon.GetFilter(const AData:TDataItem; const AFilter:String):TExpression;
begin
  if AFilter='' then
     result:=nil
  else
     result:=TDataFilter.FromString(AData,AFilter);
end;

procedure TBIWebCommon.DoAddHistory(const AContext: TBIWebContext;
                       const Command:String; const Tag:String='';
                       const Success:Boolean=True);
var tmpSize : Int64;
begin
  tmpSize:=AContext.ResponseSize;

  Logs.Lock.Enter;
  try
    Logs.AddHistory(AContext,Command,Tag,Success,T1.ElapsedMilliseconds,tmpSize);

    // Pending: Delayed "idle" mechanism to persist logs after some time interval
    if Logs.Persist then
       Logs.TrySave;
  finally
    Logs.Lock.Leave;
  end;
end;

type
  TRowAsTextExpression=class(TTextExpression)
  private
    Cursor : TDataCursor;
  public
    function Value:TData; override;
  end;

  TDataCursorAccess=class(TDataCursor);
  TDataHopsAccess=class(TDataHops);

// Pending: return all items as string, to perform search "contains"
function TRowAsTextExpression.Value:TData;
var tmp : TInteger;
    Item : TDataItem;
begin
  tmp:=TDataCursorAccess(Cursor).Current;

  result:='';

  for Item in Cursor.Data.Items.AsArray do
      result:=result+' '+Item.DataToString(tmp);
end;

procedure TBIWebCommon.PrepareCursor(const ACursor:TDataCursor; const AData:TDataItem;
                                     const Params:TStrings);

  procedure PrepareSearch;
  var Search : String;
      tmp : TRowAsTextExpression;
  begin
    Search:=Params.Values['search'];

    if (Search<>'') and (ACursor.Filter=nil) then
    begin
      tmp:=TRowAsTextExpression.Create('');
      tmp.Cursor:=ACursor;

      ACursor.Filter:=TTextLogicalExpression.Create(tmp,TTextLogicalOperand.Contains,TTextExpression.Create(Search));
    end;
  end;

  procedure TryParseSort;
  var tmpSort : String;
      tmp : TTextArray;
  begin
    tmpSort:=Trim(Params.Values['sort']);

    if tmpSort<>'' then
    begin
      SetLength(tmp,1);
      tmp[0]:=tmpSort;

      TSQLParser.ParseSort(AData,ACursor.SortBy,tmp);
    end;
  end;

var Start,
    Max : String;
begin
  ACursor.Data:=AData;

  if ACursor.Data<>nil then
  begin
    ACursor.GuessItems(Params.Values['items']);
    TDataCursorAccess(ACursor).SetDirectFilter(GetFilter(ACursor.Data,Params.Values['filter']));

    Start:=Params.Values['start'];

    if Start='' then
       ACursor.Start:=0
    else
       ACursor.Start:=StrToInt(Start);

    Max:=Params.Values['max'];

    if Max='' then
       ACursor.Max:=100
    else
       ACursor.Max:=StrToInt(Max);

    TryParseSort;

    PrepareSearch;
  end;
end;

function TBIWebCommon.GetDocument(const AContext: TBIWebContext):String;
begin
  result:='Welcome to TeeBI'+TBIHtmlHelper.Return+TBIHtmlHelper.Return+
                   TBIHtmlHelper.InitForm+
                   'Choose data:'+
                   TBIHtmlHelper.Combo('data',Data.GetDataArray,nil)+TBIHtmlHelper.Return+
                   TBIHtmlHelper.FinishForm('getdata')+
                   TBIHtmlHelper.Link('All Data','?data&format=.htm')+TBIHtmlHelper.Return+
                   TBIHtmlHelper.Link('History','?history=yes&format=.htm&max=10000')
end;

function CursorToBothStream(const ACursor:TDataCursor; const AParams:TStrings):TStream;
begin
  if ACursor.Data=nil then
     result:=nil
  else
  begin
    result:=TMemoryStream.Create;
    TDataItemPersistence.Save(ACursor.Data,result); // <-- info+data
    result:=TBIWebCommon.CheckZip(result,AParams);
  end;
end;

procedure TBIWebCommon.ReturnNormal(const AContext:TBIWebContext;
                                    const AParams:TStrings;
                                    const AFormat:String;
                                    const AData:TDataItem;
                                    const ACommand,ATag:String;
                                    const FreeData:Boolean=True);
var Cursor : TDataCursor;
begin
  Cursor:=TDataCursor.Create(nil);
  try
    PrepareCursor(Cursor,AData,AParams);

    if Cursor.Data<>nil then
    try
      if AFormat='' then
         AContext.ResponseStream:=CursorToBothStream(Cursor,AParams)
      else
         ReturnCursor(AContext,AParams,Cursor,AFormat);
    finally
      if FreeData then
         Cursor.Data.Free;
    end;

    DoAddHistory(AContext,ACommand,ATag,Cursor.Data<>nil);
  finally
    Cursor.Free;
  end;
end;

procedure SetContentType(const AContext:TBIWebContext; const AFormat:String);
begin
  if AFormat<>'' then
     if AContext.ContentType='' then
     begin
       if SameText(AFormat,'.HTM') or SameText(AFormat,'.HTML') or SameText(AFormat,'.HTML5')  then
          AContext.ContentType:='text/html'
       else
       if SameText(AFormat,'.CSV') then
          AContext.ContentType:='text/plain'
       else
          AContext.ContentType:='application/'+LowerCase(Copy(AFormat,2,Length(AFormat)));
     end;
end;

procedure TBIWebCommon.ProcessSingleData(const AData,AStore:String;
                                         const AContext: TBIWebContext;
                                         const Format:String;
                                         const Params:TStrings);

  procedure ReturnSummary(const AData,ASummary:String);
  begin
    ReturnNormal(AContext,Params,Format,
                 SummaryOf(ASummary,AData,
                 Params.Values['having'],
                 Params.Values['summaryfilter']),'summary',ASummary);
  end;

  procedure ReturnQuery(const AData,AQuery:String);
  begin
    ReturnNormal(AContext,Params,Format,
                 QueryOf(AQuery,AData,Params.Values['filter'],
                 Params.Values['distinct']),'query',AQuery);
  end;

  procedure ReturnSQL(const AData,ASQL:String);
  begin
    ReturnNormal(AContext,Params,Format,
                 TBISQL.From(Data.Find(AData),ASQL),'sql',ASQL);
  end;

  function DoReturnData(const AData:TDataItem):String;
  var tmpZip : Boolean;
      Cursor : TDataCursor;
  begin
    result:='data';
    tmpZip:=Params.Values['zip']<>'';

    Cursor:=TDataCursor.Create(nil);
    try
      if Params.Values['info']<>'' then
      begin
        PrepareCursor(Cursor,TDataInfo.Create(AData),Params);
        result:='info';
      end
      else
        PrepareCursor(Cursor,AData,Params);

      if Format='' then
      begin
        if Params.Values['both']<>'' then
           result:='both';

        if (result='info') or (result='both') then
           AContext.ResponseStream:=CursorToBothStream(Cursor,Params)
        else
        begin
          if Params.Values['meta']<>'' then
          begin
            result:='meta';
            AContext.ResponseStream:=Data.MetaStream(Cursor.Data,tmpZip);
          end
          else
            AContext.ResponseStream:=Data.DataStream(Cursor,tmpZip);
        end;
      end
      else
        ReturnCursor(AContext,Params,Cursor,Format,'',DataLink);
    finally
      Cursor.Free;
    end;
  end;

  procedure ReturnData(const AData:String);
  var tmp : TDataItem;
      tmpCommand : String;
  begin
    tmp:=Data.Find(AData,AStore);

    if tmp=nil then
       tmpCommand:='data'
    else
       tmpCommand:=DoReturnData(tmp);

    DoAddHistory(AContext,tmpCommand,AData,tmp<>nil);
  end;

var Summary,
    Query,
    SQL : String;
begin
  Summary:=Params.Values['summary'];

  if Summary<>'' then
     ReturnSummary(AData,Summary)
  else
  begin
    Query:=Params.Values['query'];

    if Query<>'' then
       ReturnQuery(AData,Query)
    else
    begin
      SQL:=Trim(Params.Values['sql']);

      if SQL<>'' then
         ReturnSQL(AData,SQL)
      else
         ReturnData(AData);
    end;
  end;

  SetContentType(AContext,Format);
end;

procedure TBIWebCommon.ProcessData(const AContext: TBIWebContext;
                                   const Format:String;
                                   const Params:TStrings);
  procedure ReturnHistory;
  begin
    ReturnNormal(AContext,Params,Format,
                 Logs.History,'history','',False);
  end;

  procedure ReturnAllData;
  begin
    if (Params.IndexOf('data')<>-1) or (Params.IndexOf('data=')<>-1) then
    begin
      AContext.ContentText:=Data.GetData;
      DoAddHistory(AContext,'data');
    end
    else
    begin
      AContext.ContentText:=GetDocument(AContext);
      DoAddHistory(AContext,'UI','/');
    end;
  end;

var tmpS : String;
begin
  if Params.Values['history']<>'' then
     ReturnHistory
  else
  begin
    tmpS:=Params.Values['data'];

    if tmpS='' then
       ReturnAllData
    else
    begin
      ProcessSingleData(tmpS,'',AContext,Format,Params);
      Exit;
    end;
  end;

  SetContentType(AContext,Format);
end;

class function TBIWebCommon.CheckZip(const AStream:TStream; const Params:TStrings):TStream;
begin
  AStream.Position:=0;

  if Params.Values['zip']<>'' then
     result:=TStore.ZipStream(AStream)
  else
     result:=AStream;
end;

type
  TDataAccess=class(TDataItem);

procedure TBIWebCommon.DataLink(const Sender:TBIExport; const AData:TDataItem; const AIndex:TInteger; var Text:String);
var tmp : String;
    tmpDest : TDataItem;
    tmpIndex : TNativeIntArray;
begin
  if Sender is TBIHtmlExport then
  begin
    // First column of a TDataInfo:
    if (AData.Parent is TDataInfo) and (AData=AData.Parent.Items[0]) then
       tmp:=TStore.OriginOf(TDataInfo(AData.Parent).Data,Data.Store)+'|'+Text
    else
    if (not AData.AsTable) and (AData.Kind=TDataKind.dkUnknown) then
       tmp:=TStore.OriginOf(AData,Data.Store)+'|'+Text
    else
    if TDataAccess(AData).HasMaster then
    begin
      if TDataAccess(AData).IMaster.Index=nil then
         AData.CreateMasterIndex;

      tmpDest:=AData.Master;
      if (tmpDest.Parent<>nil) and tmpDest.Parent.AsTable then
         tmpDest:=tmpDest.Parent;

      tmpIndex:=TDataAccess(AData).IMaster.Index;

      if AIndex<Length(tmpIndex) then // ?? internal error?
         tmp:=TStore.OriginOf(tmpDest,Data.Store)+'&start='+IntToStr(tmpIndex[AIndex])+'&max=1'
      else
         tmp:='';
    end
    else
       tmp:='';

    if tmp<>'' then
       Text:=TBIHtmlHelper.Link(Text,'?data='+TBIHtmlHelper.Escape(tmp)+'&format=.htm');
  end;
end;

{$IFNDEF NODASHBOARD}
function TBIWebCommon.ProcessDashboard(const AContext: TBIWebContext):Boolean;

  function TitleOrName(const APanel:TCustomBIPanel):String;
  begin
    result:=APanel.Title;

    if result='' then
       result:=APanel.Name;
  end;

  procedure ReturnTemplate(const AText,ATemplateName:String);
  var tmpTemplate : TBITemplate;
      tmpDash : String;
      tmpRender : THTMLRender;
      tmpText,
      tmpURL : String;
      t : Integer;

      tmpDashboard : TDashboard;

      tmpIndex : Integer;
  begin
    tmpTemplate:=TBITemplate.Create(nil);
    try
      TTemplateLoader.FromJSON(AText,tmpTemplate);

      tmpDash:=AContext.Params.Values['dashboard'];

      if tmpDash='' then
      begin
        tmpText:='';

        for t:=0 to tmpTemplate.Dashboards.Count-1 do
        begin
          tmpURL:='?template='+ATemplateName+'&dashboard='+tmpTemplate.DashBoards[t].Name;

          tmpText:=tmpText+TBIHtmlHelper.Link(TitleOrName(tmpTemplate.Dashboards[t]),
                   tmpURL)+
                   TBIHtmlHelper.Return+TCommonUI.CRLF;
        end;

        AContext.ContentText:=tmpText;
      end
      else
      begin
        tmpIndex:=tmpTemplate.Dashboards.IndexOf(tmpDash);

        if tmpIndex=-1 then
           raise Exception.Create('Error: Cannot find Dashboard: '+tmpDash)
        else
        begin
          tmpDashboard:=tmpTemplate.Dashboards[tmpIndex];

          tmpRender:=THTMLRender.Create;
          try
            if AContext.FormParams='' then
               tmpTemplate.Generate(tmpRender,tmpDashboard,AContext.Params.Values['layout'])
            else
               tmpTemplate.Generate(tmpRender,tmpDashboard,'',AContext.FormParams);

            AContext.ContentText:=tmpRender.HTML.Text;
          finally
            tmpRender.Free;
          end;
        end;
      end;

      DoAddHistory(AContext,'dashboard',ATemplateName+' '+tmpDash);
    finally
      tmpTemplate.Free;
    end;
  end;

var tmpTemplateName,
    tmpJSONFile : String;
    tmpText : String;
begin
  tmpTemplateName:=AContext.Params.Values['template'];

  result:=tmpTemplateName<>'';

  if result then
  begin
    if PublicFolder.Enabled then
    begin
      tmpJSONFile:=PublicFolder.PathOf(tmpTemplateName);
      try
        tmpText:=TBITextSource.LoadFromFile(tmpJSONFile);

        if AContext.Params.Values['info']<>'' then
        begin
          AContext.ContentText:=tmpText;
          AContext.ContentType:='application/json';

          DoAddHistory(AContext,'template',tmpTemplateName);
        end
        else
          ReturnTemplate(tmpText,tmpTemplateName);
      except
        on EFOpenError do
           raise Exception.Create('Error: Cannot access Dashboard Template file')
        else
           raise;
      end;
    end
    else
       raise Exception.Create('Error: Cannot access Dashboard Template file')
  end;
end;
{$ENDIF}

procedure TBIWebCommon.ProcessGet(const AContext: TBIWebContext);
var Def : String;
    Params : TStrings;
begin
  T1:=TStopwatch.StartNew;

  Params:=AContext.Params;

  Def:=Params.Values['def'];

  if Def<>'' then
  begin
    AContext.ContentText:=Data.GetDefinition(Def);
    DoAddHistory(AContext,'definition',Def);
  end
  else
  {$IFNDEF NODASHBOARD}
  if not ProcessDashboard(AContext) then
  {$ENDIF}
     ProcessData(AContext,Params.Values['format'],Params);
end;

procedure TBIWebCommon.ProcessFile(const ADocument:String; const AContext: TBIWebContext);

  procedure ReturnFile(const AFile:String);
  begin
    AContext.ReturnFile(AFile);
  end;

  procedure Return404;
  begin
    //AResponseInfo.ResponseNo:=404;
    //AResponseInfo.ResponseText:='Document not found';
    //AResponseInfo.WriteHeader;

    raise EBIException.Create('Resource not found: '+ADocument);
  end;

var tmp : String;
begin
  if PublicFolder.Enabled then
  begin
    tmp:=PublicFolder.PathOf(ADocument);

    if TFile.Exists(tmp) then
       ReturnFile(tmp)
    else
       Return404;
  end
  else
    raise EBIException.Create('Cannot serve file: '+ADocument);
end;

function TBIWebCommon.QueryOf(const AQuery,AData,AFilter,ADistinct: String): TDataItem;
var tmp : TDataSelect;
    Items : TArray<String>;
    tmpS : String;
begin
  result:=nil;

  Items:=AQuery.Split([',']);

  if Length(Items)>0 then
  begin
    tmp:=TDataSelect.Create(nil);
    try
      tmp.Data:=Data.Find(AData);

      if tmp.Data<>nil then
      begin
        TDataCursorAccess(tmp).SetDirectFilter(GetFilter(tmp.Data,AFilter));

        for tmpS in Items do
            tmp.Add(tmp.Data.Items.Find(tmpS));

        tmp.Distinct:=ADistinct<>'';

        result:=tmp.Calculate;
      end;
    finally
      tmp.Free;
    end;
  end;
end;

type
  TSummaryAccess=class(TSummary);
  
function TBIWebCommon.SummaryOf(const ASummary,AData,AHaving,AFilter: String): TDataItem;

  procedure PrepareSummary(const Summary:TSummary; const Data:TDataItem;
                           const Items : TArray<String>);

    function CreateGroup(const S:String):TGroupBy;
    var tmp : TDataItem;
    begin
      tmp:=Data.Items.Find(S);

      if tmp=nil then
         result:=Summary.AddGroupBy(TDataExpression.FromString(Data,S))
      else
         result:=Summary.AddGroupBy(tmp);
    end;

  var S : TArray<String>;
      t : Integer;
      tmpS : String;
      Aggregate : TAggregate;
      tmpPart : TDateTimePart;
      tmp : TDataItem;
  begin
    // Measures
    S:=Items[0].Split([',']);

    if Length(S)=0 then
       Summary.AddMeasure(Data,TAggregate.Count)
    else
    for t:=0 to High(S) do
    begin
      tmpS:=S[t];

      if TSQLParser.FindAggregate(tmpS,Aggregate) then
      begin
        tmp:=Data.Items.Find(tmpS);

        if tmp=nil then
           Summary.AddMeasure(TDataExpression.FromString(Data,tmpS),Aggregate)
        else
           Summary.AddMeasure(tmp,Aggregate);
      end
      else
        raise EBIException.CreateFmt(BIMsg_Summary_WrongAggregate,[tmpS]);
    end;

    // GroupBy
    if Length(Items)>1 then
    begin
      S:=Items[1].Split([',']);

      for t:=0 to High(S) do
      begin
        tmpS:=S[t];

        if TSQLParser.FindGroupByPart(tmpS,tmpPart) then
           CreateGroup(tmpS).DatePart:=tmpPart
        else
           CreateGroup(tmpS);
      end;
    end;
  end;

var tmp: TSummary;
    tmpData : TDataItem;
    Items : TArray<String>;
begin
  result:=nil;

  Items:=ASummary.Split([';']);

  if Length(Items)>0 then
  begin
    tmp:=TSummary.Create(nil);
    try
      tmpData:=Data.Find(AData);

      if tmpData<>nil then
      begin
        TSummaryAccess(tmp).SetDirectFilter(GetFilter(tmpData,AFilter));

        if AHaving<>'' then
           tmp.Having.Add(AHaving);

        PrepareSummary(tmp,tmpData,Items);

        result:=tmp.Calculate;
      end;
    finally
      tmp.Free;
    end;
  end;
end;

{ TWebLogs }

function DateTimeFile(const ADate:TDateTime):String;
begin
  result:=FormatDateTime('yyyymmdd_hhnnss',ADate);
end;

constructor TWebLogs.Create;
begin
  inherited Create;
  Lock:=TCriticalSection.Create;
end;

destructor TWebLogs.Destroy;
begin
  Lock.Free;
  inherited;
end;

procedure TWebLogs.SetCurrentFile;
var tmp : String;
begin
  if (Store='') or (TStores.IndexOf(Store)=-1) then
     tmp:=TPath.Combine(TPath.GetDocumentsPath,'BIWeb')
  else
     tmp:=TStore.PathOf(Store);

  if not TDirectory.Exists(tmp) then
     ForceDirectories(tmp);

  CurrentFile:=TPath.Combine(tmp,DateTimeFile(Now)+LogExtension);
end;

procedure TWebLogs.TrySave;
begin
  Lock.Enter;
  try
    if CurrentFile='' then
       SetCurrentFile;

    TDataItemPersistence.Save(History,CurrentFile);
  finally
    Lock.Leave;
  end;
end;

{ TBIWebCommon.TWebPublic }

function TBIWebCommon.TWebPublic.PathOf(const AFileName: String): String;
var tmp : String;
begin
  if AFileName='' then
     result:=''
  else
  begin
    tmp:=StringReplace(AFileName,'/','\',[rfReplaceAll]);

    if Copy(tmp,1,1)='\' then
       Delete(tmp,1,1);

    tmp:=TPath.Combine(RealPath,tmp);

    result:=TPath.GetFullPath(tmp);
  end;
end;

procedure TBIWebCommon.TWebPublic.SetPath(const Value: String);
var tmpReal,
    tmp : String;
begin
  tmp:=Trim(Value);

  if FPath<>tmp then
  begin
    if TDirectory.IsRelativePath(tmp) then
       tmpReal:=TPath.Combine(ExtractFilePath(ParamStr(0)),tmp)
    else
       tmpReal:=tmp;

    tmpReal:=TPath.GetFullPath(tmpReal);

    FPath:=tmp;
    RealPath:=tmpReal;

    if not TDirectory.Exists(RealPath) then
       ForceDirectories(RealPath);
  end;
end;

{ TImportScheduler }

Destructor TImportScheduler.Destroy;
begin
  FData.Free;
  inherited;
end;

function UnitsToString(const AUnits:TRefreshUnit):String;
begin
  case AUnits of
    Seconds: result:='Seconds';
    Minutes: result:='Minutes';
      Hours: result:='Hours';
       Days: result:='Days';
      Weeks: result:='Weeks';
     Months: result:='Months';
      Years: result:='Years';
  end;
end;

type
  TDataDefinitionAccess=class(TDataDefinition);

procedure TImportScheduler.AddAll;
var t,L : Integer;
    tmp : TStringArray;
    tmpDef : TDataDefinition;
begin
  tmp:=TStore.AllData(FStore);

  L:=Length(tmp);
  FData.Resize(L);

  for t:=0 to L-1 do
  begin
    FData[1].TextData[t]:=tmp[t];

    tmpDef:=Definition(t);

    if tmpDef<>nil then
    try
      FData[0].BooleanData[t]:=TDataDefinitionAccess(tmpDef).Refresh.Enabled;
      FData[2].Int32Data[t]:=TDataDefinitionAccess(tmpDef).Refresh.Period;
      FData[3].TextData[t]:=UnitsToString(TDataDefinitionAccess(tmpDef).Refresh.Units);
    finally
      tmpDef.Free;
    end
    else
    begin
      FData[0].BooleanData[t]:=False;
      FData[2].Int32Data[t]:=10;
      FData[3].TextData[t]:=UnitsToString(TRefreshUnit.Hours);
    end;
  end;
end;

function TImportScheduler.GetData: TDataItem;
begin
  if FData=nil then
  begin
    FData:=TDataItem.Create(True);
    FData.Items.Add('Enabled',TDataKind.dkBoolean);
    FData.Items.Add('Data',TDataKind.dkText);
    FData.Items.Add('Every',TDataKind.dkInt32);
    FData.Items.Add('TimeUnit',TDataKind.dkText);

    AddAll;
  end;

  result:=FData;
end;

function TImportScheduler.Definition(const Index:Integer):TDataDefinition;
begin
  result:=TStore.GetDefinition(FStore,FData[1].TextData[Index]);
end;

function TImportScheduler.NextRefresh(const Index:Integer):TDateTime;
var tmp : TDataDefinition;
begin
  tmp:=Definition(Index);
  try
    result:=tmp.NextRefresh;
  finally
    tmp.Free;
  end;
end;

procedure TImportScheduler.TryReImport(const Index:Integer);
begin
  // Import to temp *.bi file, delete old,
  // try to rename current to old, then rename new to current
end;

procedure TImportScheduler.Process;
var t : Integer;
begin
  if Enabled and (FData<>nil) then
     for t:=0 to FData.Count-1 do
         if FData[0].BooleanData[t] then
            if NextRefresh(t)>Now then
               TryReImport(t);
end;

procedure TImportScheduler.Refresh(const AStore:String);
begin
  FStore:=AStore;

  if FData<>nil then
  begin
    FData.ClearData(True);
    AddAll;
  end;
end;

end.
