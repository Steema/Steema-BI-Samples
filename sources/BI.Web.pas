{*********************************************}
{  TeeBI Software Library                     }
{  HTTP Web data access                       }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web;

interface

// This units contains an ABSTRACT class to control all web client and server
// TeeBI data management.

// See the different implementations of the abstract class:

{
   Unit             Description
   ---------------  -----------------------------------
   BI.Web.Net       TNetHttpClient support from RAD RTL System.Net
   BI.Web.Indy      TIdHttp Indy support
   BI.Web.FPC       FreePascal Lazarus RTL TfpHttpClient

}

uses
  System.Classes, System.Types, System.SysUtils,
  BI.Arrays, BI.DataItem, BI.Persist, BI.DataSource, BI.Compression;

type
  EHttpAbort=class(Exception);

  TBIHttpProgress=procedure(Sender:TObject; const ACurrent,ATotal:Int64; var Abort:Boolean) of object;

  TBIHttpClass=class of TBIHttp;

  TWebProxy=record
  public
    Host : String;
    Port : Integer;
    User,
    Password : String;
  end;

  TBIFtp=class
  public
    const
      DefaultPort=21;

    Constructor Create(const ADef:TDataDefinition); virtual; abstract;

    procedure Connect; virtual; abstract;
    procedure DisConnect; virtual; abstract;
    function Get(const AFileName:String):TStream; virtual; abstract;
    function IncludedFiles(const AFolder,AIncludeMask:String):TStringDynArray; virtual; abstract;
    function ListFiles(const AFolder,AIncludeMask:String): TStrings; virtual; abstract;
  end;

  TWebURL=record
  private
    procedure RemoveParam(const ATag:String);
  public
    Host : String;
    Port : Integer;
    Params : String;
  end;

  TBIHttp=class abstract
  private
    class var
      FOnProgress : TBIHttpProgress;
  protected
    procedure DoProgress(const ACurrent,ATotal:Int64);
  public
    class var
      Engine : TBIHttpClass;

    Constructor Create(const AOwner:TComponent); virtual; abstract;

    class function FTP(const ADef:TDataDefinition):TBIFtp; virtual; abstract;

    procedure Get(const AURL:String; const AStream:TStream); overload; virtual; abstract;
    function Get(const AURL:String):String; overload; virtual; abstract;
    class function Parse(const AURL:String):TWebURL; virtual; abstract;
    procedure SetProxy(const AProxy:TWebProxy); virtual; abstract;
    procedure SetTimeout(const ATimeout:Integer); virtual; abstract;

    class property OnProgress:TBIHttpProgress read FOnProgress write FOnProgress;
  end;

  TWebCompression=(No,Yes);

  TBIWebClient=class
  private
  class var
    FOnFinish,
    FOnStart : TNotifyEvent;

  var
    FHttp : TBIHttp;

    function GetHttp: TBIHttp;
    procedure TryErrorAsString(const AStream:TStream);
  protected
    function DoFromURL(const AURL:String): TDataItem;
    function GetDataStream(const Data:String; const Children:Boolean; const Compress:TWebCompression): TStream;

  public
  const
    DefaultPort=15015;
    DefaultTimeout=2000;

    class property OnFinish:TNotifyEvent read FOnFinish write FOnFinish;
    class property OnStart:TNotifyEvent read FOnStart write FOnStart;

  var
    Server : String;
    Port : Integer;
    Timeout : Integer;
    Compress : TWebCompression;
    Store : String;

    Proxy : TWebProxy;

    Constructor Create(const AServer:String; const APort:Integer=DefaultPort); overload;
    Constructor Create(const AData:TDataDefinition); overload;

    Destructor Destroy; override;

    class function FromPath(const APath:String):TBIWebClient; static;

    class function FTP(const ADef:TDataDefinition):TBIFtp; static;

    procedure GetData(const Data:String; var Items:TDataArray; const ACompress:TWebCompression); overload;
    procedure GetData(const AData:TDataItem; const Children:Boolean; const ACompress:TWebCompression); overload;
    procedure GetData(const AOrigin:String; const AData:TDataItem; const Children:Boolean; const ACompress:TWebCompression); overload;
    function GetData(const AData:String; const Children:Boolean; const ACompress:TWebCompression):TDataItem; overload;

    function GetData:String; overload;
    function GetMetaData(const S:String; const ACompress:TWebCompression): TDataItem;
    function GetStream(const S:String):TStream;
    function GetString(const S:String):String;

    function Load(const AData: String; const ACompress: TWebCompression=TWebCompression.Yes): TDataItem;

    class function FromURL(const AServer:String;
                           const APort:Integer;
                           const AParams:String;
                           const ACompress: TWebCompression=TWebCompression.Yes): TDataItem; overload; static;

    class function FromURL(const AURL:String;
                           const ACompress: TWebCompression=TWebCompression.Yes): TDataItem; overload; static;

    class function Query(const AStore,AData,ASQL:String):TDataItem; static;

    class function UnZip(const AStream:TStream):TStream; static; inline;
    function URL:String;

    property Http:TBIHttp read GetHttp;
  end;

  TBIWebServer=class
  public
  type
    TVersion=record
    public
      Major,
      Minor : Integer;
      URL : String;

      function ToString:String;
    end;

  const
    Version:TVersion=(Major:1; Minor:0; URL:'');
  end;

  TBIWebHistory=class(TDataItem)
  public
    Constructor Create;

    function Add(const Time:TDateTime; const RemoteIP,Command,Tag:String;
                  const Success:Boolean; const Millisec:Integer; const Size:Int64):TInteger;
  end;

  TSteema=class(TBIWebClient)
  public
    class function CheckNewVersion(out AError:String):Boolean; static;
    class function Download(const Source,Dest:String):Boolean; static;
    class function GetLatestVersion(out V: TBIWebServer.TVersion; out Error:String):Boolean; static;
  end;

  TDelayHandlerWeb=class(TDataDelayProvider)
  protected
    function GetStream(const AData,ANext:TDataItem):TStream; override;
    function GetStream(const AItems:TDataArray):TStream; override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    BIWeb : TBIWebClient;

    Constructor CreateWeb(const ABIWeb:TBIWebClient);
    Destructor Destroy; override;
  end;

  TBIURLSource=class(TBIFileSource)
  protected
    function DoImportFile(const FileName:String):TDataArray; override;
    function ImportURLFile(const FileName:String):TDataArray;
  public
    class function FileFilter:TFileFilters; override;
    class function From(const AURL:String):TDataItem; static;
    function FromStrings(const URL:String; const S:TStrings):TDataArray;
    class function StreamFrom(const URL: String): TStream; static;
    function Import(const URL:String):TDataArray;
    class function Supports(const Extension:String):Boolean; override;
  end;

implementation

{$IFNDEF FPC}
{$IF COMPILERVERSION>=30} // RX 10 Seattle
{$DEFINE HASHTTPCLIENT}
{$ENDIF}
{$ENDIF}

uses
  {$IFDEF FPC}
  BI.Web.FPC, BI.FPC,
  {$ELSE}

  {$IFDEF USESynLZ}
  BI.Compression.SynZip,
  {$ENDIF}

  // Default web engine:

  {$IFDEF HASHTTPCLIENT}
  BI.Web.Net,
  {$ELSE}
  BI.Web.Indy,
  {$ENDIF}

  System.IOUtils,
  {$ENDIF}

  BI.Streams, BI.Languages.English, BI.UI, BI.Arrays.Strings;

{ TBIWebClient }

Constructor TBIWebClient.Create(const AServer:String; const APort:Integer=DefaultPort);
begin
  inherited Create;
  Server:=AServer;
  Port:=APort;
end;

Constructor TBIWebClient.Create(const AData: TDataDefinition);
begin
  Create(AData['WebServer']);

  Port:=StrToIntDef(AData['WebPort'],TBIWebClient.DefaultPort);
  Timeout:=StrToIntDef(AData['Timeout'],TBIWebClient.DefaultTimeout);

  if AData['Compress']<>'' then
     Compress:=TWebCompression.Yes;

  // Proxy:

  Proxy.Host:=AData['WebProxyServer'];
  TryStrToInt(AData['WebProxyPort'],Proxy.Port);
  Proxy.User:=AData['WebProxyUser'];
  Proxy.Password:=TCrypto.Decrypt(AData['WebProxyPass']);
end;

Destructor TBIWebClient.Destroy;
begin
  FHttp.Free;
  inherited;
end;

class function TBIWebClient.FromPath(const APath: String): TBIWebClient;
var tmp : TStringArray;
    tmpCount : Integer;
begin
  tmp:=TStringArray.Split(APath,':');

  tmpCount:=tmp.Count;

  if tmpCount>1 then
  begin
    result:=TBIWebClient.Create(tmp[1]);

    if tmpCount>2 then
       result.Port:=StrToIntDef(tmp[2],TBIWebClient.DefaultPort);

    if tmpCount>3 then
       if SameText(tmp[3],'zip') then
          result.Compress:=TWebCompression.Yes
       else
          result.Compress:=TWebCompression.No;

    if tmpCount>4 then
       result.Proxy.Host:=tmp[4];

    if tmpCount>5 then
       result.Proxy.Port:=StrToIntDef(tmp[5],TBIWebClient.DefaultPort);

    if tmpCount>6 then
       result.Proxy.User:=tmp[6];

    if tmpCount>7 then
       result.Proxy.Password:=TCrypto.Decrypt(tmp[7]);
  end
  else
    result:=nil;
end;

class function TBIWebClient.FromURL(const AServer:String;
                                    const APort:Integer;
                                    const AParams:String;
                                    const ACompress: TWebCompression=TWebCompression.Yes): TDataItem;
begin
  with TBIWebClient.Create(AServer,APort) do
  try
    result:=Load(AParams,ACompress);
  finally
    Free;
  end;
end;

function TBIWebClient.DoFromURL(const AURL:String): TDataItem;
var tmp : TWebURL;
begin
  tmp:=TBIHttp.Engine.Parse(AURL);

  if tmp.Host<>'' then
     result:=TBIWebClient.FromURL(AURL,Compress)
  else
  begin
    tmp.Params:=AURL;
    tmp.RemoveParam('format');
    result:=GetData(tmp.Params,True,Compress);
  end;
end;

class function TBIWebClient.FromURL(const AURL:String;
                                    const ACompress: TWebCompression=TWebCompression.Yes): TDataItem;
var tmp : TWebURL;
begin
  tmp:=TBIHttp.Engine.Parse(AURL);

  if tmp.Port<=0 then
     tmp.Port:=TBIWebClient.DefaultPort;

  tmp.RemoveParam('format');

  with TBIWebClient.Create(tmp.Host,tmp.Port) do
  try
    Compress:=ACompress;
    result:=GetData(tmp.Params,True,Compress);
  finally
    Free;
  end;
end;

class function TBIWebClient.FTP(const ADef:TDataDefinition): TBIFtp;
begin
  result:=TBIHttp.Engine.FTP(ADef);
end;

function TBIWebClient.GetDataStream(const Data:String; const Children:Boolean; const Compress:TWebCompression): TStream;
var tmp : String;
begin
  if Compress=TWebCompression.Yes then
     tmp:='&zip=1'
  else
     tmp:='';

  if Children then
     tmp:=tmp+'&children=1';

  result:=GetStream('data='+Data+tmp);
end;

// Uncompress AStream and destroy it
class function TBIWebClient.UnZip(const AStream:TStream):TStream;
begin
  result:=TCompression.DeCompress(AStream,'data');

  AStream.{$IFDEF NEXTGEN}DisposeOf{$ELSE}Free{$ENDIF};
end;

procedure TBIWebClient.GetData(const AData:TDataItem; const Children:Boolean;
                       const ACompress:TWebCompression);
begin
  GetData(TStore.OriginOf(AData,Store),AData,Children,ACompress);
end;

procedure TBIWebClient.GetData(const AOrigin:String; const AData:TDataItem;
                       const Children:Boolean; const ACompress:TWebCompression);
var tmp : TStream;
    r   : TBIReader;
begin
  tmp:=GetDataStream(AOrigin,Children,ACompress);
  try
    if tmp.Size>0 then
    begin
      tmp.Position:=0;

      if ACompress=TWebCompression.Yes then
         tmp:=UnZip(tmp);

      r:=TBIReader.Create(tmp,TPersistence.DefaultBufferSize);
      try
        TDataPersistence.DoLoad(r,AData,Children,True);
      finally
        r.Free;
      end;
    end
    else
      raise EBIException.Create(BIMsg_Web_ErrorEmptyDataStream);
  finally
    tmp.Free;
  end;
end;

procedure TBIWebClient.GetData(const Data: String; var Items: TDataArray; const ACompress:TWebCompression);

  function ItemsToString:String;
  var t, H : Integer;
  begin
    result:='';

    H:=High(Items);

    for t:=0 to H do
        if t<H then
           result:=result+Items[t].Name+','
        else
           result:=result+Items[t].Name;
  end;

var tmp : TStream;
begin
  tmp:=GetDataStream(Data+'&items='+ItemsToString,False {?},ACompress);
  try
    if tmp.Size>0 then
    begin
      tmp.Position:=0;

      if ACompress=TWebCompression.Yes then
         tmp:=UnZip(tmp);

      TDataPersistence.Load(tmp,Items);
    end
    else
      raise EBIException.Create(BIMsg_Web_ErrorEmptyDataStream);
  finally
    tmp.Free;
  end;
end;

{$IFDEF FPC}
procedure FPCLoadFromStream(const ASource:TStream; const ADest:TStringStream);
var Count: Longint;
    tmp : String;
begin
  ASource.Position:=0;
  Count := ASource.Size;
  SetLength(tmp,Count);
  ASource.Read(PChar(tmp)^,Count);
  ADest.WriteString(tmp);
end;
{$ENDIF}

procedure TBIWebClient.TryErrorAsString(const AStream:TStream);

  function StreamToString(const AStream:TStream):String;
  var tmp : TStringStream;
  begin
    tmp:=TStringStream.Create('');
    try
      AStream.Position:=0;

      {$IFDEF FPC}
      FPCLoadFromStream(AStream,tmp);
      {$ELSE}
      tmp.LoadFromStream(AStream);
      {$ENDIF}

      result:=tmp.DataString;
    finally
      tmp.Free;
    end;
  end;

begin
  raise EBIException.Create('Error loading data from web: '+StreamToString(AStream));
end;


function TBIWebClient.GetData(const AData: String; const Children:Boolean; const ACompress: TWebCompression): TDataItem;
var tmp : TStream;
    tmpS : String;
begin
  result:=nil;

  tmpS:='&both=1';

  if ACompress=TWebCompression.Yes then
     tmpS:=tmpS+'&zip=1';

  tmp:=GetStream(AData+tmpS);
  try
    if tmp.Size>0 then
    begin
      tmp.Position:=0;

      if ACompress=TWebCompression.Yes then
         tmp:=UnZip(tmp);

      if tmp.Size>=SizeOf(Integer) then // <-- at a minimum, 4 bytes
      begin
        try
          result:=TDataItemPersistence.Load(tmp);
        except
          on E:EBILoadVersionException do
          begin
            if E.WrongVersion<TPersistence.SupportedFrom then  // assume its a normal Version error
               raise
            else
               TryErrorAsString(tmp);
          end;
        end;
      end
      else
         TryErrorAsString(tmp);
    end
    else
      raise EBIException.Create(BIMsg_Web_ErrorEmptyDataStream);
  finally
    tmp.Free;
  end;
end;

function TBIWebClient.GetData: String;
begin
  result:=GetString('data');
end;

function TBIWebClient.GetHttp: TBIHttp;
begin
  if FHttp=nil then
  begin
    FHttp:=TBIHttp.Engine.Create(nil);
    FHttp.SetProxy(Proxy);

    if Timeout=0 then
       FHttp.SetTimeout(TBIRegistry.ReadInteger('Settings','WebTimeout',TBIWebClient.DefaultTimeout))
    else
       FHttp.SetTimeout(Timeout);
  end;

  result:=FHttp;
end;

function TBIWebClient.GetMetaData(const S:String; const ACompress:TWebCompression): TDataItem;
var tmp : TStream;
begin
  result:=nil;

  tmp:=GetDataStream(S+'&meta=1',False,ACompress);
  try
    if tmp.Size>0 then
    begin
      tmp.Position:=0;

      if ACompress=TWebCompression.Yes then
         tmp:=UnZip(tmp);

      try
        result:=TPersistence.Load(tmp);
      except
        on E:EBILoadVersionException do
        begin
          if E.WrongVersion<TPersistence.SupportedFrom then  // assume its a normal Version error
             raise
          else
             TryErrorAsString(tmp);
        end;
      end;
    end;
  finally
    tmp.Free;
  end;
end;

function TBIWebClient.GetStream(const S: String): TStream;
begin
  if Assigned(FOnStart) then
     FOnStart(Self);

  result:=TMemoryStream.Create;
  try
    try
      HTTP.Get(URL+'?'+S,result);
    except
      on Exception do
      begin
        result.Free; // prevent memory leak
        raise;
      end;
    end;
  finally
    if Assigned(FOnFinish) then
       FOnFinish(Self);
  end;
end;

function TBIWebClient.GetString(const S: String): String;
begin
  if Assigned(FOnStart) then
     FOnStart(Self);

  try
    result:=HTTP.Get(URL+'?'+S); //{$IFNDEF INDY}.ContentAsString{$ENDIF}
  finally
    if Assigned(FOnFinish) then
       FOnFinish(Self);
  end;
end;

function TBIWebClient.URL: String;
begin
  result:='http://'+Server+':'+IntToStr(Port);
end;

function TBIWebClient.Load(const AData: String; const ACompress: TWebCompression): TDataItem;
begin
  result:=GetMetaData(AData,ACompress);
  GetData(result,False,ACompress);
end;

class function TBIWebClient.Query(const AStore, AData, ASQL: String): TDataItem;
var tmp : TBIWebClient;
begin
  tmp:=TBIWebClient.FromPath(TStore.PathOf(AStore));
  try
    tmp.Store:=AStore;
    result:=tmp.GetData('data='+AData+'&sql='+ASQL,True,tmp.Compress);
  finally
    tmp.Free;
  end;
end;

{ TBIWebServer.TVersion }

function TBIWebServer.TVersion.ToString: String;
begin
  result:=IntToStr(Major)+'.'+IntToStr(Minor);
end;

{ TSteema }

const
  SteemaBIWeb='http://www.steema.cat';

class function TSteema.CheckNewVersion(out AError: String): Boolean;
var V : TBIWebServer.TVersion;
begin
  result:=True;
  AError:='';

  if TSteema.GetLatestVersion(V,AError) then
  begin
    if (V.Major>TBIWebServer.Version.Major) or
       (
         (V.Major=TBIWebServer.Version.Major)
         and
         (V.Minor>TBIWebServer.Version.Minor)
       ) then
    begin
      if TSteema.Download(V.URL,TPath.Combine(TPath.GetTempPath,'biweb.exe')) then
         TCommonUI.ShowMessage(BIMsg_Web_LatestVersionDownloaded)
      else
      begin
        AError:='Error downloading BIWeb update from: '+V.URL;
        result:=False;
      end;
    end;
  end
  else
    result:=False;
end;

class function TSteema.Download(const Source, Dest: String): Boolean;
var Steema : TSteema;
    M : TMemoryStream;
begin
  Steema:=TSteema.Create(SteemaBIWeb);
  try
    M:=TMemoryStream.Create;
    try
      Steema.Http.Get(Source,M);
      M.SaveToFile(Dest);
      result:=True;
    finally
      M.Free;
    end;
  finally
    Steema.Free;
  end;
end;

class function TSteema.GetLatestVersion(out V: TBIWebServer.TVersion; out Error:String):Boolean;
var Steema : TSteema;
    S : String;
    Items : TStringArray;
begin
  result:=False;
  Error:='';

  Steema:=TSteema.Create(SteemaBIWeb);
  try
    try
      S:=Steema.Http.Get(SteemaBIWeb+'/bi/latestversion.txt');

      if S<>'' then
      begin
        Items:=TStringArray.Split(S,TCommonUI.CRLF);

        // Silently exit in case of missing Steema.cat configuration
        if Items.Count<3 then
           Exit;

        V.Major:=StrToInt(Items[0]);
        V.Minor:=StrToInt(Items[1]);
        V.URL:=Items[2];

        result:=True;
      end;
    except
      on E:Exception do
      begin
        Error:=E.Message;
      end;
    end;
  finally
    Steema.Free;
  end;
end;

type
  TBIWebImporter=class(TDataImporter)
  private
    BIWeb : TBIWebClient;

    function ImportFTP(const AClient:TBIWebClient): TDataArray;
    function GetStore: String;
    procedure SetStore(const Value: String);
    procedure TryCreateBIWeb(const AStore:String; const ADefinition:TDataDefinition);
  public
    Constructor CreateDefinition(const AStore:String; const ADefinition:TDataDefinition); override;

    Destructor Destroy; override;

    function AllData:TStringArray; override;
    function GetDefinition(const AName:String):TDataDefinition; override;
    function Import:TDataArray; override;
    class function IsRemote:Boolean; override;
    function Load(const AName:String):TDataItem; override;

    class function Supports(const Kind:TDataDefinitionKind; const ADefinition:TDataDefinition=nil):Boolean; override;
  published
    property Store:String read GetStore write SetStore;
  end;

{ TBIWebImporter }

Constructor TBIWebImporter.CreateDefinition(const AStore:String; const ADefinition:TDataDefinition);
begin
  inherited CreateDefinition(AStore,ADefinition);
  TryCreateBIWeb(AStore,ADefinition);
end;

Destructor TBIWebImporter.Destroy;
begin
  BIWeb.Free;
  inherited;
end;

procedure TBIWebImporter.TryCreateBIWeb(const AStore:String; const ADefinition:TDataDefinition);
var FPath : String;
begin
  if ADefinition=nil then
  begin
    FPath:=TStore.PathOf(AStore);
    BIWeb:=TBIWebClient.FromPath(FPath);

    if BIWeb=nil then
       BIWeb:=TBIWebClient.Create('');
  end
  else
    BIWeb:=TBIWebClient.Create(ADefinition);

  BIWeb.Store:=AStore;
end;

function TBIWebImporter.GetDefinition(const AName: String): TDataDefinition;
begin
  result:=TDataDefinition.Create(nil);  // <-- important: always "nil" as owner !
  result.LoadFromText(BIWeb.GetString('def='+AName));
end;

function TBIWebImporter.GetStore: String;
begin
  if BIWeb=nil then
     result:=''
  else
     result:=BIWeb.Store;
end;

function TBIWebImporter.AllData:TStringArray;
begin
  result:=TStringArray.Split(BIWeb.GetData,#13#10);
end;

type
  TDataItemAccess=class(TDataItem);

function TBIWebImporter.Load(const AName:String):TDataItem;

  procedure SetDelay(const AData:TDataItem);
  var Item : TDataItem;
  begin
    TDataItemAccess(AData).DelayPos:=-1;

    if TDataItemAccess(AData).HasItems then
       for Item in AData.Items.AsArray do
           SetDelay(Item);
  end;

begin
  result:=BIWeb.GetMetaData(AName,BIWeb.Compress);

  if result<>nil then
  begin
    result.Name:=AName;
    SetDelay(result);
    result.Provider:=TDelayHandlerWeb.CreateWeb(BIWeb);
  end;
end;

function TBIWebImporter.ImportFTP(const AClient:TBIWebClient): TDataArray;

  function DoImport(const Ftp:TBIFtp; const AList:TStrings):TDataArray;
  var tmpFile : TBIFileSource;
      tmpStream : TStream;
      tmpData : TDataArray;
      tmpCount : Integer;

      t,
      tt : Integer;
  begin
    result:=nil;

    tmpFile:=TBIFileSource.Create;
    try
      tmpCount:=0;

      for t:=0 to AList.Count-1 do
      begin
        tmpStream:=Ftp.Get(AList[t]);
        try
          tmpStream.Position:=0;

          tmpData:=tmpFile.ImportStream(AList[t],tmpStream);

          if tmpData<>nil then
          begin
            SetLength(result,tmpCount+tmpData.Count);

            for tt:=0 to tmpData.Count-1 do
                result[tt+tmpCount]:=tmpData[tt];

            Inc(tmpCount,tmpData.Count);
          end;
        finally
          tmpStream.Free;
        end;
      end;
    finally
      tmpFile.Free;
    end;
  end;

var tmp : TBIFtp;
    tmpS : TStrings;
begin
  tmp:=AClient.FTP(Definition);
  try
    tmp.Connect;
    try
      tmpS:=tmp.ListFiles(Definition['Folder'],Definition['IncludeMask']);
      try
        result:=DoImport(tmp,tmpS);
      finally
        tmpS.Free;
      end;
    finally
      tmp.Disconnect;
    end;
  finally
    tmp.Free;
  end;
end;

function TBIWebImporter.Import: TDataArray;

  function ArrayOf(const AData:TDataItem):TDataArray;
  begin
    if AData=nil then
       result:=nil
    else
    begin
      SetLength(result,1);
      result[0]:=AData;
    end;
  end;

var B : TBIWebClient;
    S : String;
    tmpCancel : Boolean;
begin
  result:=nil;

  tmpCancel:=False;
  DoProgress(0,tmpCancel);

  if not tmpCancel then
  begin
    B:=TBIWebClient.Create(Definition);
    try
      B.Store:=TStore.DefaultName;

      S:=Trim(Definition['WebURL']);

      if S<>'' then
         result:=ArrayOf(B.DoFromURL(S))
      else
      begin
        S:=Trim(Definition['WebData']);

        if S='' then
        begin
          if Definition['FTPServer']<>'' then
             result:=ImportFTP(B)
          else
             result:=nil
        end
        else
          result:=ArrayOf(B.GetData('data='+S,True,B.Compress));
      end;
    finally
      B.Free;
    end;
  end;

  DoProgress(100,tmpCancel);
end;

class function TBIWebImporter.IsRemote: Boolean;
begin
  result:=True;
end;

procedure TBIWebImporter.SetStore(const Value: String);
begin
  if Store<>Value then
  begin
    if BIWeb=nil then
       TryCreateBIWeb(Value,nil)
    else
       BIWeb.Store:=Value;
  end;
end;

class function TBIWebImporter.Supports(const Kind: TDataDefinitionKind;
                     const ADefinition:TDataDefinition=nil): Boolean;
begin
  result:=(Kind=TDataDefinitionKind.Web) or
          (
             (Kind=TDataDefinitionKind.Files) and
             (ADefinition<>nil) and
             (ADefinition['FTPServer']<>'')
          );
end;

{ TDelayHandlerWeb }

Constructor TDelayHandlerWeb.CreateWeb(const ABIWeb: TBIWebClient);
begin
  Create(nil);

  BIWeb:=TBIWebClient.Create(ABIWeb.Server,ABIWeb.Port);

  BIWeb.Proxy:=ABIWeb.Proxy;
  BIWeb.Store:=ABIWeb.Store;
  BIWeb.Compress:=ABIWeb.Compress;
end;

Destructor TDelayHandlerWeb.Destroy;
begin
  BIWeb.Free;
  inherited;
end;

function TDelayHandlerWeb.GetStream(const AItems: TDataArray): TStream;
begin
  result:=TMemoryStream.Create;
  TDataPersistence.Save(AItems,result);
end;

function TDelayHandlerWeb.GetStream(const AData,ANext: TDataItem): TStream;
begin
  result:=TMemoryStream.Create;
  TDataPersistence.Save(AData,result);
end;

procedure TDelayHandlerWeb.Load(const AData: TDataItem; const Children:Boolean);
begin
  BIWeb.GetData(AData,Children,BIWeb.Compress);
end;

{ TBIWebHistory }

Constructor TBIWebHistory.Create;
begin
  inherited Create(True);

  Items.Add('Time',dkDateTime);
  Items.Add('IP',dkText);
  Items.Add('Command',dkText);
  Items.Add('Tag',dkText);
  Items.Add('Success',dkBoolean);
  Items.Add('Millisec',dkInt32);
  Items.Add('Size',dkInt64);
end;

function TBIWebHistory.Add(const Time:TDateTime; const RemoteIP, Command, Tag: String;
                            const Success: Boolean;
                            const Millisec:Integer; const Size:Int64):TInteger;
begin
  result:=FCount;
  Resize(result+1);

  Items[0].DateTimeData[result]:=Time;
  Items[1].TextData[result]:=RemoteIP;
  Items[2].TextData[result]:=Command;
  Items[3].TextData[result]:=Tag;
  Items[4].BooleanData[result]:=Success;
  Items[5].Int32Data[result]:=Millisec;
  Items[6].Int64Data[result]:=Size;
end;

{ TWebURL }

procedure TWebURL.RemoveParam(const ATag: String);
var tmp : TStrings;
    t : Integer;
    tmpDeleted : Boolean;
begin
  tmp:=TStringList.Create;
  try
    tmp.Delimiter:='&';
    tmp.DelimitedText:=Params;

    tmpDeleted:=False;

    t:=0;

    while t<tmp.Count do
        if SameText(tmp.Names[t],ATag) then
        begin
          tmp.Delete(t);
          tmpDeleted:=True;
        end
        else
           Inc(t);

    if tmpDeleted then
       Params:=tmp.DelimitedText;
  finally
    tmp.Free;
  end;
end;

{ TBIHttp }

procedure TBIHttp.DoProgress(const ACurrent, ATotal: Int64);
var tmpAbort : Boolean;
begin
  tmpAbort:=False;

  OnProgress(Self,ACurrent,ATotal,tmpAbort);

  if tmpAbort then
     raise EHttpAbort.Create(BIMsg_Http_UserCancelled);
end;

{ TBIURLSource }

class function TBIURLSource.FileFilter:TFileFilters;
begin
  result:=nil;
  result.Add('URL files','*.url');
end;

class function TBIURLSource.Supports(const Extension: String): Boolean;
begin
  result:=SameText(Extension,'.url');
end;

function TBIURLSource.ImportURLFile(const FileName:String):TDataArray;
var S : TStrings;
    tmp : String;
begin
  S:=TStringList.Create;
  try
    S.LoadFromFile(FileName);

    tmp:=S.Values['URL'];

    if tmp='' then
       result:=nil
    else
       result:=Import(tmp);
  finally
    S.Free;
  end;
end;

function TBIURLSource.DoImportFile(const FileName: String): TDataArray;
var tmp : String;
begin
  tmp:=TPath.GetExtension(FileName);

  if SameText(tmp,'.url') then
     result:=ImportURLFile(FileName)
  else
     result:=nil;
end;

class function TBIURLSource.From(const AURL:String):TDataItem;
var tmp : TBIURLSource;
begin
  tmp:=TBIURLSource.Create;
  try
    result:=TBISource.FromData(tmp.Import(AURL));
  finally
    tmp.Free;
  end;
end;

class function TBIURLSource.StreamFrom(const URL: String): TStream;
var h : TBIHTTP;
begin
  {$IFDEF HASHTTPCLIENT}
  h:=TBIHttpClient.Create(nil); // force usage of System.Net
  {$ELSE}
  h:=TBIHttp.Engine.Create(nil); // current default Engine
  {$ENDIF}
  try
    result:=TMemoryStream.Create;

    h.Get(URL,result);

    if result.Size>0 then
       result.Position:=0;
  finally
    h.Free;
  end;
end;

function TBIURLSource.FromStrings(const URL:String; const S:TStrings):TDataArray;
var tmp : TBIFileSourceClass;
    tmpImport : TBIFileSource;
begin
  tmp:=TBIFileImporters.GuessExtension(TPath.GetExtension(URL));

  if tmp=nil then
     tmp:=GuessFromContent(S);

  if tmp=nil then
     raise EBIException.CreateFmt(BIMsg_ImporterMissing,[URL])
  else
  begin
    tmpImport:=tmp.Create(IDefinition,Parallel);
    try
      result:=tmpImport.Import(S);
    finally
      tmpImport.Free;
    end;
  end;
end;

function TBIURLSource.Import(const URL: String): TDataArray;
var tmpS : TStream;
    S : TStrings;
begin
  result:=nil;

  tmpS:=StreamFrom(URL);

  if tmpS<>nil then
  try
    if tmpS.Size>0 then
    begin
      S:=TStringList.Create;
      try
        S.LoadFromStream(tmpS);
        result:=FromStrings(URL,S);
      finally
        S.Free;
      end;
    end;
  finally
    tmpS.Free;
  end;
end;

initialization
  TBIHttp.Engine:={$IFDEF FPC}TBIFPCWeb
                  {$ELSE}
                  {$IFDEF HASHTTPCLIENT}TBIHttpClient{$ELSE}TBIIndy{$ENDIF}
                  {$ENDIF};

  TDataImporter.RegisterClass(TBIWebImporter);
  TBIFileImporters.RegisterClass(TBIURLSource);

  {$IFDEF USESynLZ}
  TCompression.Plugin:=TSynLZCompression;
  {$ENDIF}

finalization
  {$IFDEF USESynLZ}
  TCompression.Plugin:=TSystemCompression;
  {$ENDIF}

  TBIFileImporters.UnRegisterClass(TBIURLSource);
  TDataImporter.UnRegisterClass(TBIWebImporter);
end.


