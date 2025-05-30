{*********************************************}
{  TeeBI Software Library                     }
{  A default web "Module"                     }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.Modules.Default;

interface

uses
  System.Classes,

  BI.Arrays,
  BI.DataItem, BI.DataSource, BI.Expression, BI.Summary,

  BI.Web.AllData,
  BI.Web.Context,
  BI.Web.Logs,
  BI.Web.Modules;

type
  TDefaultModule=class(TModule)
  private
    class function CalcTotals(const ACursor:TDataCursor;
                              const Aggregate:TAggregate):TDataItem; static;

    class function CursorToText(var ACursor:TDataCursor; const AFormat:String;
                          const GetText:TExportGetText=nil;
                          const ATotals:TDataItem=nil):String; static;

    procedure DataLink(const Sender:TBIExport; const AData:TDataItem;
                             const AIndex:TInteger; var Text:String);

    procedure DoAddHistory(const AContext: TWebContext;
                           const Msec:Int64;
                           const Command:String;
                           const Tag:String='';
                           const Success:Boolean=True);

    class function GetCSS(const AName:String):String; static;

    class function GetFilter(const AData:TDataItem; const AFilter:String):TExpression; static;

    procedure PrepareCursor(const ACursor:TDataCursor; const AData:TDataItem;
                            const Params:TStrings);

    procedure ProcessData(const AContext: TWebContext;
                          const Format:String;
                          const Params:TStrings);

    function QueryOf(const AData:TDataItem; const AQuery,AFilter,ADistinct: String): TDataItem;

    procedure ReturnCursor(const AContext:TWebContext;
                           const Params:TStrings;
                           var ACursor:TDataCursor; const AFormat:String;
                           const AURL:String='';
                           const GetText:TExportGetText=nil);

    function ReturnNormal(const AContext:TWebContext;
                           const AParams:TStrings;
                           const AFormat:String;
                           const AData:TDataItem;
                           const ACommand,ATag:String;
                           const FreeData:Boolean=True):Boolean;


    function Root(const AContext: TWebContext):String;

    function SummaryOf(const AData:TDataItem; const ASummary,AHaving,AFilter:String):TDataItem;

    function TotalsOf(const Params:TStrings; const ACursor:TDataCursor):TDataItem;

  public
    Data : TAllData;
    Logs : TWebLogs;

    procedure AllData(const AContext:TWebContext);

    function Get(const AContext: TWebContext):Boolean; override;
    function Post(const AContext: TWebContext):Boolean; override;

    procedure ProcessSingleData(const AData:TDataItem;
                                const AContext: TWebContext;
                                const Format:String;
                                const Params:TStrings); overload;

    procedure ProcessSingleData(const AData,AStore:String;
                                const AContext: TWebContext;
                                const Format:String;
                                const Params:TStrings); overload;
  end;

implementation

uses
  System.Diagnostics, System.SysUtils,

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

  BI.Languages.English, BI.Info, BI.UI,
  BI.Persist, BI.Expressions, BI.SQL, BI.Html, BI.Web.Html;

{ TDefaultModule }

function TDefaultModule.Get(const AContext: TWebContext):Boolean;
var Def : String;
    Params : TStrings;
    T1 : TStopWatch;
begin
  T1:=TStopWatch.StartNew;

  Params:=AContext.Params;

  Def:=Params.Values['def'];

  if Def<>'' then
  begin
    AContext.ContentText:=Data.GetDefinition(Def);
    DoAddHistory(AContext,T1.ElapsedMilliseconds,'definition',Def);
  end
  else
    ProcessData(AContext,Params.Values['format'],Params);

  result:=True;
end;

procedure TDefaultModule.DoAddHistory(const AContext: TWebContext;
                                           const Msec:Int64;
                                           const Command:String;
                                           const Tag:String='';
                                           const Success:Boolean=True);
var tmpSize : Int64;
begin
  tmpSize:=AContext.ResponseSize;

  Logs.Enter;
  try
    Logs.AddHistory(AContext,Command,Tag,Success,Msec,tmpSize);

    // Pending: Delayed "idle" mechanism to persist logs after some time interval
    if Logs.Persist then
       Logs.TrySave;
  finally
    Logs.Leave;
  end;
end;

function CheckZip(const AStream:TStream; const Params:TStrings):TStream;
begin
  AStream.Position:=0;

  if Params.Values['zip']<>'' then
     result:=TStore.ZipStream(AStream)
  else
     result:=AStream;
end;

function CursorToBothStream(const ACursor:TDataCursor; const AParams:TStrings):TStream;
begin
  if ACursor.Data=nil then
     result:=nil
  else
  begin
    result:=TMemoryStream.Create;
    TDataItemPersistence.Save(ACursor.Data,result); // <-- info+data
    result:=CheckZip(result,AParams);
  end;
end;

function TDefaultModule.ReturnNormal(const AContext:TWebContext;
                                     const AParams:TStrings;
                                     const AFormat:String;
                                     const AData:TDataItem;
                                     const ACommand,ATag:String;
                                     const FreeData:Boolean=True):Boolean;
var Cursor : TDataCursor;
begin
  Cursor:=TDataCursor.Create(nil);
  try
    PrepareCursor(Cursor,AData,AParams);

    result:=Cursor.Data<>nil;

    if result then
    try
      if AFormat='' then
         AContext.ResponseStream:=CursorToBothStream(Cursor,AParams)
      else
         ReturnCursor(AContext,AParams,Cursor,AFormat);
    finally
      if FreeData then
         Cursor.Data.Free;
    end;
  finally
    Cursor.Free;
  end;
end;

procedure SetContentType(const AContext:TWebContext; const AFormat:String);
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

type
  TSummaryAccess=class(TSummary);

function TDefaultModule.SummaryOf(const AData:TDataItem; const ASummary,AHaving,AFilter: String): TDataItem;

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
    Items : TArray<String>;
begin
  result:=nil;

  Items:=ASummary.Split([';']);

  if Length(Items)>0 then
  begin
    tmp:=TSummary.Create(nil);
    try
      if AData<>nil then
      begin
        TSummaryAccess(tmp).SetDirectFilter(GetFilter(AData,AFilter));

        if AHaving<>'' then
           tmp.Having.Add(AHaving);

        PrepareSummary(tmp,AData,Items);

        result:=tmp.Calculate;
      end;
    finally
      tmp.Free;
    end;
  end;
end;

procedure TDefaultModule.ProcessSingleData(const AData:TDataItem;
                                         const AContext: TWebContext;
                                         const Format:String;
                                         const Params:TStrings);

  procedure ReturnSummary(const ASummary:String);
  var T1 : TStopwatch;
      tmp : Boolean;
      tmpSum : TDataItem;
  begin
    T1:=TStopwatch.StartNew;

    tmpSum:=SummaryOf(AData,ASummary,Params.Values['having'],Params.Values['summaryfilter']);

    tmp:=ReturnNormal(AContext,Params,Format,tmpSum,'summary',ASummary);

    DoAddHistory(AContext,T1.ElapsedMilliseconds,'summary',ASummary,tmp);
  end;

  procedure ReturnQuery(const AQuery:String);
  var T1 : TStopwatch;
      tmp : Boolean;
      tmpQuery : TDataItem;
  begin
    T1:=TStopwatch.StartNew;

    tmpQuery:=QueryOf(AData,AQuery,Params.Values['filter'],Params.Values['distinct']);

    tmp:=ReturnNormal(AContext,Params,Format,tmpQuery,'query',AQuery);

    DoAddHistory(AContext,T1.ElapsedMilliseconds,'query',AQuery,tmp);
  end;

  procedure ReturnSQL(const ASQL:String);
  var T1 : TStopwatch;
      tmp : Boolean;
  begin
    T1:=TStopwatch.StartNew;

    tmp:=ReturnNormal(AContext,Params,Format,TBISQL.From(AData,ASQL),'sql',ASQL);

    DoAddHistory(AContext,T1.ElapsedMilliseconds,'sql',ASQL,tmp);
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

  procedure ReturnData;
  var tmpData,
      tmpCommand : String;
      T1 : TStopwatch;
  begin
    T1:=TStopwatch.StartNew;

    if AData=nil then
    begin
      tmpCommand:='data';
      tmpData:='';
    end
    else
    begin
      tmpCommand:=DoReturnData(AData);
      tmpData:=AData.Name;
    end;

    DoAddHistory(AContext,T1.ElapsedMilliseconds,tmpCommand,tmpData,AData<>nil);
  end;

var Summary,
    Query,
    SQL : String;
begin
  Summary:=Params.Values['summary'];

  if Summary<>'' then
     ReturnSummary(Summary)
  else
  begin
    Query:=Params.Values['query'];

    if Query<>'' then
       ReturnQuery(Query)
    else
    begin
      SQL:=Trim(Params.Values['sql']);

      if SQL<>'' then
         ReturnSQL(SQL)
      else
         ReturnData;
    end;
  end;

  SetContentType(AContext,Format);
end;

procedure TDefaultModule.ProcessSingleData(const AData,AStore:String;
                                const AContext: TWebContext;
                                const Format:String;
                                const Params:TStrings);
var tmpStore : String;
    tmp : TDataItem;
begin
  if AStore='' then
     tmpStore:=TStore.DefaultName
  else
     tmpStore:=AStore;

  if AData='' then
     tmp:=Data.AllData(tmpStore)
  else
     tmp:=Data.Find(AData,tmpStore);

  ProcessSingleData(tmp,AContext,Format,Params);
end;

function TDefaultModule.Root(const AContext: TWebContext):String;
begin
  result:='Welcome to TeeBI'+TBIHtmlHelper.Return+TBIHtmlHelper.Return+
                   TBIHtmlHelper.InitForm+
                   'Choose data:'+
                   TBIHtmlHelper.Combo('data',Data.GetDataArray,nil)+TBIHtmlHelper.Return+
                   TBIHtmlHelper.FinishForm('getdata')+
                   TBIHtmlHelper.Link('All Data','?data&format=.htm')+TBIHtmlHelper.Return+
                   TBIHtmlHelper.Link('History','?history=yes&format=.htm&max=10000')
end;

procedure TDefaultModule.ProcessData(const AContext: TWebContext;
                                     const Format:String;
                                     const Params:TStrings);
  procedure ReturnHistory;
  begin
    ReturnNormal(AContext,Params,Format,
                 Logs.History,'history','',False);
  end;

  procedure ReturnAllData;
  var T1 : TStopwatch;
  begin
    T1:=TStopwatch.StartNew;

    if (Params.IndexOf('data')<>-1) or (Params.IndexOf('data=')<>-1) then
    begin
      AContext.ContentText:=Data.GetData;
      DoAddHistory(AContext,T1.ElapsedMilliseconds,'data');
    end
    else
    begin
      AContext.ContentText:=Root(AContext);
      DoAddHistory(AContext,T1.ElapsedMilliseconds,'UI','/');
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

function TDefaultModule.Post(const AContext: TWebContext):Boolean;
var tmpS : String;
    Cursor : TDataCursor;
    tmpData : TDataItem;
    T1 : TStopWatch;
begin
  tmpS:=AContext.Params.Values['data'];

  result:=tmpS<>'';

  if result then
  begin
    T1:=TStopWatch.StartNew;

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

    DoAddHistory(AContext,T1.ElapsedMilliseconds,'post',tmpS,True);
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
  result:='';

  if Cursor<>nil then
  begin
    tmp:=TDataCursorAccess(Cursor).Current;

    for Item in Cursor.Data.Items.AsArray do
        result:=result+' '+Item.DataToString(tmp);
  end;
end;

procedure TDefaultModule.PrepareCursor(const ACursor:TDataCursor;
                                       const AData:TDataItem;
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

class function TDefaultModule.GetCSS(const AName:String):String;
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

procedure TDefaultModule.ReturnCursor(const AContext: TWebContext;
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

function TDefaultModule.QueryOf(const AData:TDataItem; const AQuery,AFilter,ADistinct: String): TDataItem;
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
      if AData<>nil then
      begin
        tmp.Data:=AData;

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

class function TDefaultModule.GetFilter(const AData:TDataItem; const AFilter:String):TExpression;
begin
  if AFilter='' then
     result:=nil
  else
     result:=TDataFilter.FromString(AData,AFilter);
end;

type
  TDataAccess=class(TDataItem);

procedure TDefaultModule.DataLink(const Sender:TBIExport; const AData:TDataItem; const AIndex:TInteger; var Text:String);
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
    // First column of a TAllData:
    if (AData.Parent is TAllDataInfo) and (AData=AData.Parent.Items[0]) then
       tmp:=Text
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

function TDefaultModule.TotalsOf(const Params:TStrings; const ACursor:TDataCursor):TDataItem;
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

class function TDefaultModule.CursorToText(var ACursor:TDataCursor;
                                           const AFormat:String;
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

type
  TDataItemAccess=class(TDataItem);

procedure TDefaultModule.AllData(const AContext: TWebContext);
begin
  ProcessSingleData(AContext.Params.Values['data'],'',AContext,'.htm',AContext.Params);
end;

class function TDefaultModule.CalcTotals(const ACursor:TDataCursor;
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

end.
