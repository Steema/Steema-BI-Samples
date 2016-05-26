{*********************************************}
{  TeeBI Software Library                     }
{  Web Server (VCL and Firemonkey)            }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.Common;

interface

uses
  System.Classes, IdContext, IdCustomHTTPServer, System.Diagnostics,
  System.SyncObjs,
  BI.Web.AllData, BI.Data, BI.DataSource, BI.Expression, BI.Arrays,
  BI.Summary, BI.Persist,

  {$IFDEF FMX}
  FMXTee.Constants, FMXTee.Canvas, FMXTee.Procs, FMXTee.Chart,

  FMX.Types,

  {$IF Declared(FireMonkeyVersion) and (FireMonkeyVersion<+21)}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  System.UITypes, FMX.Surfaces,

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO}
  {$ENDIF}

  {$IFDEF TEEPRO}
  FMXTee.Themes, BI.FMX.PDF, FMXTee.Canvas.Javascript,
  {$ENDIF}
  FMXTee.Series, BI.FMX.Chart
  {$ELSE}

  VCLTee.TeeConst, VCLTee.Chart, VCLTee.TeeExport, VCLTee.TeeBmpOptions,

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO}
  {$ENDIF}

  {$IFDEF TEEPRO}
  VCLTee.TeeJPEG, VCLTee.TeePNG, VCLTee.TeeThemes, BI.VCL.PDF,
  VCLTee.TeeJavascript,
  {$ENDIF}

  VCLTee.Series, VCLTee.TeEngine, VCL.Controls, VCL.Graphics, BI.VCL.Chart
  {$ENDIF}
  ;

type
  THistoryProc=procedure(const AContext:TIdContext;
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

    class function ChartFromCursor(var ACursor:TDataCursor;
                           const Params: TStrings;
                           const AWidth,AHeight:Integer;
                           const AUseTheme:Boolean=True):TBIChart; static;

    class function ChartToStream(var ACursor:TDataCursor; const Params:TStrings;
                           const AFormat:String;
                           const AWidth,AHeight:Integer):TStream; static;

    class function CheckZip(const AStream:TStream; const Params:TStrings):TStream; static;

    class function CursorToText(var ACursor:TDataCursor; const AFormat:String;
                          const GetText:TExportGetText=nil;
                          const ATotals:TDataItem=nil):String; static;

    procedure DataLink(const Sender:TBIExport; const AData:TDataItem;
                             const AIndex:TInteger; var Text:String);

    procedure DoAddHistory(const AContext: TIdContext;
                           const AResponseInfo: TIdHTTPResponseInfo;
                           const Command:String;
                           const Tag:String=''; const Success:Boolean=True);

    class function GetCSS(const AName:String):String; static;

    class function GetFilter(const AData:TDataItem; const AFilter:String):TBaseLogicalExpression; static;

    class procedure GetWidthHeight(const Params:TStrings; out AWidth,AHeight:Integer); static;
    procedure PrepareCursor(const ACursor:TDataCursor; const AData:TDataItem;
                            const Params:TStrings);

    function ProcessDashboard(const AContext: TIdContext;
             const AResponseInfo: TIdHTTPResponseInfo;
             const ARequestInfo:TIdHTTPRequestInfo):Boolean;

    procedure ProcessData(const AContext: TIdContext;
                          const AResponseInfo: TIdHTTPResponseInfo;
                          const Format:String;
                          const Params:TStrings);

    procedure ReturnCursor(const AResponseInfo: TIdHTTPResponseInfo;
                           const Params:TStrings;
                           var ACursor:TDataCursor; const AFormat:String;
                           const AURL:String='';
                           const GetText:TExportGetText=nil);

    function TotalsOf(const Params:TStrings; const ACursor:TDataCursor):TDataItem;

  public
    type
      TWebPublic=record
      private
        FPath : String;
        RealPath : String;

        function PathOf(const AFileName:String):String;
        procedure SetPath(const Value: String);
      public
        Enabled : Boolean;
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

    procedure ProcessFile(const ADocument:String; const AContext: TIdContext;
                          const AResponseInfo: TIdHTTPResponseInfo);

    procedure ProcessGet(const AContext: TIdContext; const AResponseInfo: TIdHTTPResponseInfo;
                         const ARequestInfo:TIdHTTPRequestInfo);
    procedure ProcessPost(const AContext: TIdContext; const AResponseInfo: TIdHTTPResponseInfo;
                          const ARequestInfo:TIdHTTPRequestInfo);
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Types,
  BI.Data.SQL, BI.Data.Html, BI.Languages.English, IdSSL,
  BI.Dashboard, BI.Dashboard.HTML;

Constructor TBIWebCommon.Create;
begin
  inherited Create;
  Logs:=TWebLogs.Create;
  Scheduler:=TImportScheduler.Create;
end;

Destructor TBIWebCommon.Destroy;
begin
  Scheduler.Free;
  Logs.Free;
  inherited;
end;

class function TBIWebCommon.ChartFromCursor(var ACursor: TDataCursor;
                    const Params : TStrings;
                    const AWidth, AHeight: Integer;
                    const AUseTheme:Boolean=True): TBIChart;

  function GetBoolean(const ATag:String; const ADefault:Boolean):Boolean;
  var tmp : String;
  begin
    tmp:=Params.Values[ATag];

    if tmp='' then
       result:=ADefault
    else
       result:=StrToBoolDef(tmp,ADefault);
  end;

  function Get3D:Boolean;
  begin
    result:=GetBoolean('view3d',False);
  end;

  {$IFDEF TEEPRO}
  function FindThemeClass(const S:String):TChartThemeClass;
  var t : Integer;
      tmp : TChartTheme;
  begin
    result:=TLookoutTheme;

    if S<>'' then
    begin
      for t:=0 to ChartThemes.Count-1 do
      begin
        tmp:=ChartThemes[t].Create(nil);
        try
          if SameText(tmp.Description,S) then
             Exit(ChartThemes[t]);
        finally
          tmp.Free;
        end;
      end;
    end;
  end;
  {$ENDIF}

begin
  result:=TBIChart.Create(nil);

  {$IFDEF TEEPRO}
  if AUseTheme then
  begin
    ApplyChartTheme(FindThemeClass(Params.Values['theme']),result.Chart);

    // Force disable bevels after applying theme
    result.Chart.BevelOuter:=bvNone;
  end;
  {$ENDIF}

  result.Width:=AWidth;
  result.Height:=AHeight;

  result.Fill(ACursor,nil);
  result.Chart.View3D:=Get3D;
end;

class function TBIWebCommon.ChartToStream(var ACursor:TDataCursor;
                                    const Params:TStrings;
                                    const AFormat:String;
                                    const AWidth,AHeight:Integer):TStream;

var C : TBIChart;
    {$IFDEF FMX}
    tmpB : TBitmap;
    tmpSurface : TBitmapSurface;
    {$ELSE}
    Format : TTeeExportFormat;
    {$ENDIF}
begin
  C:=ChartFromCursor(ACursor,Params,AWidth,AHeight);
  try
    {$IFDEF FMX}
    tmpB:=C.Chart.TeeCreateBitmap(TAlphaColors.Null,C.Chart.GetRectangle);
    try
      tmpSurface:=TBitmapSurface.Create;
      try
        tmpSurface.Assign(tmpB);

        result:=TMemoryStream.Create;

        //XE6: AV if png is used for first time (jpg is ok, then png is ok too)
        //possible workaround: if not TBitmapCodecManager.CodecExists()...

        TBitmapCodecManager.SaveToStream(result, tmpSurface, AFormat, nil {SaveParams});
        result.Position:=0;
      finally
        tmpSurface.Free;
      end;
    finally
      tmpB.Free;
    end;
    {$ELSE}
    {$IFDEF TEEPRO}
    if SameText(AFormat,'.JPG') or SameText(AFormat,'.JPEG') then
       Format:=TJPEGExportFormat.Create
    else
       Format:=TPNGExportFormat.Create;
    {$ELSE}
       Format:=TBMPExportFormat.Create;
    {$ENDIF}

    try
      Format.Panel:=C.Chart;
      // Format.CheckSize(AWidth, AHeight);

      result:=TMemoryStream.Create;
      Format.SaveToStream(result); // ,Options
      result.Position:=0;
    finally
      Format.Free;
    end;
    {$ENDIF}
  finally
    C.Free;
  end;
end;

class function TBIWebCommon.CursorToText(var ACursor:TDataCursor; const AFormat:String;
                          const GetText:TExportGetText=nil;
                          const ATotals:TDataItem=nil):String;
var Format : TBIFileSourceClass;
    tmp : TBIExport;
begin
  result:='';

  Format:=TBIFileImporters.GuessExtension(AFormat);

  if Format<>nil then
  begin
    tmp:=Format.ExportFormat;

    if tmp<>nil then
    try
      tmp.OnGetText:=GetText;
      tmp.Cursor:=ACursor;

      tmp.Totals:=ATotals;  // grand totals

      // tmp.Layout:=ALayout; // record or table?
      result:=tmp.AsString;
    finally
      tmp.Free;
    end;
  end;
end;

class procedure TBIWebCommon.GetWidthHeight(const Params:TStrings; out AWidth,AHeight:Integer);

  function TryTag(const ATag:String; const ADefault:Integer):Integer;
  var tmp : String;
  begin
    tmp:=Params.Values[ATag];

    if tmp='' then
       result:=ADefault
    else
       result:=StrToIntDef(tmp,ADefault);
  end;

begin
  AWidth:=TryTag('width',800);
  AHeight:=TryTag('height',600);
end;

class function TBIWebCommon.GetCSS(const AName:String):String;
begin
  if (AName='') or SameText(AName,'pure') then
     result:=THTMLRender.PureCSS+TBIHTMLExport.CRLF+
             '<meta name="viewport" content="width=device-width, initial-scale=1">'+TBIHTMLExport.CRLF
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

procedure TBIWebCommon.ReturnCursor(const AResponseInfo: TIdHTTPResponseInfo;
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

  var tmp : TBIFileSourceClass;
      tmpS : String;
  begin
    result:='<style>.dropdown { position: relative; display: inline-block; z-index: 9999; }'+
      '.dropdown .dropdown-menu { position: absolute; top: 100%; display: none; margin: 0; list-style: none; width: 120%; padding: 0; }'+
      '.dropdown:hover .dropdown-menu { display: block; }'+
      '.dropdown button { background: #2362FF; color: #FFFFFF; border: none; margin: 0; padding: 0.4em 0.8em; font-size: 1em; }'+
      '.dropdown-wide button { min-width: 13em; }'+
      '.dropdown a { display: block; padding: 0.2em 0.8em; text-decoration: none; background: #CCCCCC; color: #333333; }'+
      '.dropdown a:hover { background: #BBBBBB; }</style>'+
      '<div class="dropdown"><button>'+BIMsg_Export+'</button><ul class="dropdown-menu">';

      for tmp in TBIFileImporters.Importers do
      begin
        tmpS:=tmp.ClassName;

        if Copy(tmpS,1,3)='TBI' then
           Delete(tmpS,1,3);

        if SameText(tmpS,'JSON') or
           SameText(tmpS,'CSV') or
           SameText(tmpS,'HTML') or
           {$IFDEF TEEPRO}
           SameText(tmpS,'PDF') or
           {$ENDIF}
           SameText(tmpS,'XML') then
             result:=result+'<li><a href="#" onclick="location.href='''+MakeURL(tmpS)+''';">'+tmpS+'</a></li>';
      end;

      result:=result+'</ul></div>';
  end;

  procedure ExportChart(const AExtension,AContent:String);
  var w,h : Integer;
  begin
    GetWidthHeight(Params,w,h);
    AResponseInfo.ContentStream:=ChartToStream(ACursor,Params,AExtension,w,h);

    if AResponseInfo.ContentStream<>nil then
       AResponseInfo.ContentType:=AContent;
  end;

  procedure ExportChartHTML5;
  var w,h : Integer;
      tmp : TBIChart;
      tmpJS : TJavascriptExportFormat;
  begin
    GetWidthHeight(Params,w,h);

    tmp:=ChartFromCursor(ACursor,Params,w,h,False);
    try
      tmp.Chart.Title.Caption:=Params.Values['title'];

      // Avoid "TeeChart" title in html5 javascript chart
      if tmp.Chart.Title.Caption='' then
         tmp.Chart.Title.Caption:=' ';

      tmp.Chart.Color:=clNone; //{$IFDEF FMX}TAlphaColors.White{$ELSE}clWhite{$ENDIF};

      tmpJS:=TJavascriptExportFormat.Create;
      try
        // DEBUG using local js:
        //tmpJS.SourceScriptPath:='file:///C:\Root\Teechartjscript\src';

        tmp.Chart.View3DOptions.FontZoom:=StrToIntDef(Params.Values['fontzoom'],100); // %

        tmpJS.Minify:=True;
        tmpJS.Panel:=tmp.Chart;

        AResponseInfo.ContentText:=tmpJS.JScript.Text;
      finally
        tmpJS.Free;
      end;
    finally
      tmp.Free;
    end;
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
    if SameText(AFormat,'.PNG') then
       ExportChart('.png','image/png')
    else
    if SameText(AFormat,'.JPG') or SameText(AFormat,'.JPEG') then
       ExportChart('.jpg','image/jpeg')
    else
    if SameText(AFormat,'.HTML5') then
       ExportChartHTML5
    else
    begin
      if SameText(AFormat,'.htm') then
         tmp:=ExportHTMLHeader
      else
         tmp:='';

      tmpTotals:=TotalsOf(Params,ACursor);
      try
        AResponseInfo.ContentText:=tmp+CursorToText(ACursor,AFormat,GetText,tmpTotals);
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

procedure TBIWebCommon.ProcessPost(const AContext: TIdContext; const AResponseInfo: TIdHTTPResponseInfo;
                                   const ARequestInfo:TIdHTTPRequestInfo);
var tmpS : String;
    Cursor : TDataCursor;
    tmpData : TDataItem;
begin
  tmpS:=ARequestInfo.Params.Values['data'];

  if tmpS<>'' then
  begin
    tmpData:=Data.Find(tmpS);

    if (not tmpData.AsTable) and (tmpData.Kind=TDataKind.dkUnknown) then
    begin
      AResponseInfo.ContentText:=
         'Data: '+tmpData.Name+TBIHtmlHelper.Return+TBIHtmlHelper.InitForm+
         'Items: '+TBIHtmlHelper.Combo('data',Data.GetData(tmpData.Items),
                  Data.GetDataOrigins(tmpData.Items))+TBIHtmlHelper.Return+
         TBIHtmlHelper.FinishForm('getdata');
    end
    else
    begin
      Cursor:=TDataCursor.Create(nil);
      try
        PrepareCursor(Cursor,tmpData,ARequestInfo.Params);

        ReturnCursor(AResponseInfo,ARequestInfo.Params,Cursor,'.htm','?data='+tmpS+'&format=.htm',DataLink);

        AResponseInfo.ContentText:=
            'Data: '+Cursor.Data.Name+TBIHtmlHelper.Return+TBIHtmlHelper.Return+
            AResponseInfo.ContentText;
      finally
        Cursor.Free;
      end;
    end;

    DoAddHistory(AContext,AResponseInfo,'post',tmpS,True);
  end
  else
  if not ProcessDashboard(AContext,AResponseInfo,ARequestInfo) then
//     ProcessData(AContext,AResponseInfo,Format,Params);
end;

class function TBIWebCommon.GetFilter(const AData:TDataItem; const AFilter:String):TBaseLogicalExpression;
var tmp : TExpression;
begin
  if AFilter='' then
     result:=nil
  else
  begin
    tmp:=TDataFilter.FromString(AData,AFilter);

    if tmp=nil then
       raise EBIException.Create('Bad Filter: '+AFilter)
    else
    if tmp is TBaseLogicalExpression then
       result:=TBaseLogicalExpression(tmp)
    else
    begin
      tmp.Free;
      raise EBIException.Create('Filter must be a logical expression: '+AFilter)
    end;
  end;
end;

procedure TBIWebCommon.DoAddHistory(const AContext: TIdContext;
                       const AResponseInfo: TIdHTTPResponseInfo;
                       const Command:String; const Tag:String='';
                       const Success:Boolean=True);
var tmpSize : Int64;
begin
  tmpSize:=AResponseInfo.ContentLength;

  if tmpSize=-1 then
     if AResponseInfo.ContentStream<>nil then
        tmpSize:=AResponseInfo.ContentStream.Size
     else
        tmpSize:=Length(AResponseInfo.ContentText);

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

procedure TBIWebCommon.ProcessData(const AContext: TIdContext;
                                   const AResponseInfo: TIdHTTPResponseInfo;
                                   const Format:String;
                                   const Params:TStrings);

  function CursorToBothStream(const ACursor:TDataCursor):TStream;
  begin
    if ACursor.Data=nil then
       result:=nil
    else
    begin
      result:=TMemoryStream.Create;
      TDataItemPersistence.Save(ACursor.Data,result); // <-- info+data
      result:=CheckZip(result,Params);
    end;
  end;

  function GetDocument:String;
  begin
    result:='Welcome to TeeBI'+TBIHtmlHelper.Return+TBIHtmlHelper.Return+
                     TBIHtmlHelper.InitForm+
                     'Choose data:'+
                     TBIHtmlHelper.Combo('data',Data.GetDataArray,nil)+TBIHtmlHelper.Return+
                     TBIHtmlHelper.FinishForm('getdata')+
                     TBIHtmlHelper.Link('All Data','?data&format=.htm')+TBIHtmlHelper.Return+
                     TBIHtmlHelper.Link('History','?history=yes&format=.htm&max=10000')
  end;

  procedure SetContentType;
  begin
    if SameText(Format,'.JSON') then
       AResponseInfo.ContentType:='application/json'
    else
    if SameText(Format,'.CSV') then
       AResponseInfo.ContentType:='text/plain'
    else
    if SameText(Format,'.XML') then
       AResponseInfo.ContentType:='application/xml'
    {$IFDEF TEEPRO}
    else
    if SameText(Format,'.PDF') then
       AResponseInfo.ContentType:='application/pdf';
    {$ENDIF}
  end;

var tmpS,
    Summary,
    Query,
    SQL,
    tmpCommand : String;

    tmpZip : Boolean;
    Cursor : TDataCursor;
    tmpData : TDataItem;
begin
  Cursor:=TDataCursor.Create(nil);
  try
    if Params.Values['history']<>'' then
    begin
      PrepareCursor(Cursor,Logs.History,Params);

      if Format='' then
         AResponseInfo.ContentStream:=CursorToBothStream(Cursor)
      else
         ReturnCursor(AResponseInfo,Params,Cursor,Format);

      DoAddHistory(AContext,AResponseInfo,'history','',True);
    end
    else
    begin
      tmpS:=Params.Values['data'];

      if tmpS='' then
      begin
        AResponseInfo.ContentText:=GetDocument;
        DoAddHistory(AContext,AResponseInfo,'UI','/');
      end
      else
      begin
        Summary:=Params.Values['summary'];

        if Summary<>'' then
        begin
          PrepareCursor(Cursor,
                SummaryOf(Summary,tmpS,
                            Params.Values['having'],
                            Params.Values['summaryfilter']),
                            Params);

          if Cursor.Data<>nil then
          try
            if Format='' then
               AResponseInfo.ContentStream:=CursorToBothStream(Cursor)
            else
               ReturnCursor(AResponseInfo,Params,Cursor,Format);
          finally
            Cursor.Data.Free;
          end;

          DoAddHistory(AContext,AResponseInfo,'summary',Summary,Cursor.Data<>nil);
        end
        else
        begin
          Query:=Params.Values['query'];

          if Query<>'' then
          begin
            PrepareCursor(Cursor,
                QueryOf(Query,tmpS,Params.Values['filter'],
                                   Params.Values['distinct']),
                                   Params);

            if Cursor.Data<>nil then
            try
              if Format='' then
                 AResponseInfo.ContentStream:=CursorToBothStream(Cursor)
              else
                 ReturnCursor(AResponseInfo,Params,Cursor,Format);
            finally
              Cursor.Data.Free;
            end;

            DoAddHistory(AContext,AResponseInfo,'query',Query,Cursor.Data<>nil);
          end
          else
          begin
            SQL:=Trim(Params.Values['sql']);

            if SQL<>'' then
            begin
              PrepareCursor(Cursor,TBISQL.From(Data.Find(tmpS),SQL),Params);

              if Cursor.Data<>nil then
              try
                if Format='' then
                   AResponseInfo.ContentStream:=CursorToBothStream(Cursor)
                else
                   ReturnCursor(AResponseInfo,Params,Cursor,Format);
              finally
                Cursor.Data.Free;
              end;

              DoAddHistory(AContext,AResponseInfo,'sql',SQL,Cursor.Data<>nil);
            end
            else
            begin
              tmpZip:=Params.Values['zip']<>'';

              tmpData:=Data.Find(tmpS);

              tmpCommand:='data';

              if tmpData<>nil then
              begin
                if Params.Values['info']<>'' then
                begin
                  PrepareCursor(Cursor,TDataInfo.Create(tmpData),Params);
                  tmpCommand:='info';
                end
                else
                  PrepareCursor(Cursor,tmpData,Params);

                if Format='' then
                begin
                  if Params.Values['both']<>'' then
                     tmpCommand:='both';

                  if (tmpCommand='info') or (tmpCommand='both') then
                     AResponseInfo.ContentStream:=CursorToBothStream(Cursor)
                  else
                  begin
                    if Params.Values['meta']<>'' then
                    begin
                      tmpCommand:='meta';
                      AResponseInfo.ContentStream:=Data.MetaStream(Cursor.Data.Name,tmpZip);
                    end
                    else
                      AResponseInfo.ContentStream:=Data.DataStream(Cursor,tmpZip);
                  end;
                end
                else
                  ReturnCursor(AResponseInfo,Params,Cursor,Format,'',DataLink);
              end;

              DoAddHistory(AContext,AResponseInfo,tmpCommand,tmpS,tmpData<>nil);
            end;
          end;
        end;
      end;
    end;

    SetContentType;
  finally
    Cursor.Free;
  end;
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

function TBIWebCommon.ProcessDashboard(const AContext: TIdContext;
           const AResponseInfo: TIdHTTPResponseInfo;
           const ARequestInfo:TIdHTTPRequestInfo):Boolean;

  function TitleOrName(const APanel:TBIPanel):String;
  begin
    result:=APanel['title'];

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
    tmpTemplate:=TBITemplate.FromJSON(AText);
    try
      tmpDash:=ARequestInfo.Params.Values['dashboard'];

      if tmpDash='' then
      begin
        tmpText:='';

        for t:=0 to tmpTemplate.Dashboards.Count-1 do
        begin
          tmpURL:='?template='+ATemplateName+'&dashboard='+tmpTemplate.DashBoards[t].Name;

          tmpText:=tmpText+TBIHtmlHelper.Link(TitleOrName(tmpTemplate.Dashboards[t]),
                   tmpURL)+
                   TBIHtmlHelper.Return+TBIHTMLExport.CRLF;
        end;

        AResponseInfo.ContentText:=tmpText;
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
            if ARequestInfo.FormParams='' then
               tmpTemplate.Generate(tmpRender,tmpDashboard,ARequestInfo.Params.Values['layout'])
            else
               tmpTemplate.Generate(tmpRender,tmpDashboard,'',ARequestInfo.FormParams);

            AResponseInfo.ContentText:=tmpRender.HTML.Text;
          finally
            tmpRender.Free;
          end;
        end;
      end;

      DoAddHistory(AContext,AResponseInfo,'dashboard',ATemplateName+' '+tmpDash);
    finally
      tmpTemplate.Free;
    end;
  end;

var tmpTemplateName,
    tmpJSONFile : String;
    tmpText : String;
begin
  tmpTemplateName:=ARequestInfo.Params.Values['template'];

  result:=tmpTemplateName<>'';

  if result then
  begin
    if PublicFolder.Enabled then
    begin
      tmpJSONFile:=PublicFolder.PathOf(tmpTemplateName);
      try
        tmpText:=TBITextSource.LoadFromFile(tmpJSONFile);

        if ARequestInfo.Params.Values['info']<>'' then
        begin
          AResponseInfo.ContentText:=tmpText;
          AResponseInfo.ContentType:='application/json';

          DoAddHistory(AContext,AResponseInfo,'template',tmpTemplateName);
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

procedure TBIWebCommon.ProcessGet(const AContext: TIdContext; const AResponseInfo: TIdHTTPResponseInfo;
                                  const ARequestInfo:TIdHTTPRequestInfo);
var Def,
    Format : String;
    Cursor : TDataCursor;
    Params : TStrings;
begin
  T1:=TStopwatch.StartNew;

  Params:=ARequestInfo.Params;

  Format:=Params.Values['format'];

  if Params.IndexOf('data')<>-1 then
  begin
    if Format='' then
    begin
      AResponseInfo.ContentText:=Data.GetData;
      DoAddHistory(AContext,AResponseInfo,'data');
    end
    else
    begin
      Cursor:=TDataCursor.Create(nil);
      try
        PrepareCursor(Cursor,Data.AllData,Params);

        if Cursor.Data<>nil then
        try
          ReturnCursor(AResponseInfo,Params,Cursor,Format,'',DataLink);
        finally
          Cursor.Data.Free;
        end;
      finally
        Cursor.Free;
      end;

      DoAddHistory(AContext,AResponseInfo,'datainfo');
    end;
  end
  else
  begin
    Def:=Params.Values['def'];

    if Def<>'' then
    begin
      AResponseInfo.ContentText:=Data.GetDefinition(Def);
      DoAddHistory(AContext,AResponseInfo,'definition',Def);
    end
    else
    if not ProcessDashboard(AContext,AResponseInfo,ARequestInfo) then
       ProcessData(AContext,AResponseInfo,Format,Params);
  end;
end;

procedure TBIWebCommon.ProcessFile(const ADocument:String; const AContext: TIdContext; const AResponseInfo: TIdHTTPResponseInfo);

  procedure ReturnFile(const AFile:String);
  var Enable : Boolean;
  begin
    if AResponseInfo.ContentType='' then
       AResponseInfo.ContentType:=AResponseInfo.HTTPServer.MIMETable.GetFileMIMEType(AFile);

    AResponseInfo.ContentLength:=TBIFileSource.GetFileSize(AFile);

    AResponseInfo.WriteHeader;
    Enable:=not (AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase);
    AContext.Connection.IOHandler.WriteFile(AFile, Enable);
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
           CreateGroup(tmpS).DateOptions.Part:=tmpPart
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
    tmp : TStringDynArray;
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

initialization
  RegisterTeeStandardSeries;
end.
