program BI_Linux_Web;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.SyncObjs,

  IdBaseComponent, IdComponent, IdCustomTCPServer, IdCustomHTTPServer,
  IdHTTPServer, IdContext, IdStack,

  BI.Web,
  BI.Web.AllData in '..\BI.Web.AllData.pas',
  BI.Web.Common in '..\BI.Web.Common.pas',
  BI.Web.IndyContext in '..\BI.Web.IndyContext.pas',

  BI.Arrays, BI.Data, BI.Persist, System.IOUtils, BI.UI,
  BI.Data.CSV, BI.Data.Html, BI.Data.JSON, BI.Data.ClientDataset,
  BI.Data.XML, BI.Data.DB,

  {$IFNDEF FPC}
  {$IF CompilerVersion>26}
  BI.Data.DB.FireDAC,
  {$ELSE}
  BI.Data.DB.SqlExpr,
  {$ENDIF}
  {$ENDIF}

  BI.Languages.English, BI.Languages.Spanish;

type
  TBIWebMain=class(TComponent)
  private
    Data : TAllData;
    History : TBIWebHistory;

    BIWeb : TBIWebCommon;

    Server: TIdHTTPServer;

    FLog : Boolean;

    procedure AddHistory(const AContext:TBIWebContext;
                         const Command:String;
                         const Tag:String;
                         const Success:Boolean;
                         const Millisec:Integer;
                         const Size:Int64);

    procedure CheckForUpdates;

    procedure CreateAllData(const AStore:String);

    procedure Init;
    procedure Log(const S:String);

    procedure MainLoop;

    procedure SetupLogs;

    procedure ServerCommandGet(AContext: TIdContext;
                 ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure ServerConnect(AContext: TIdContext);
    procedure ServerDisconnect(AContext: TIdContext);
    procedure ServerStatus(ASender: TObject; const AStatus: TIdStatus;
                           const AStatusText: string);

    procedure Show;
    procedure ShowAddress;
    procedure ShowClients;
    procedure ShowHelp;
    procedure ToggleLog;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;
  end;

{ TBIWebMain }

Constructor TBIWebMain.Create;
begin
  inherited;

  Server:=TIdHTTPServer.Create(Self);

  Server.OnCommandGet:=ServerCommandGet;
  Server.OnConnect:=ServerConnect;
  Server.OnDisconnect:=ServerDisconnect;
  Server.OnStatus:=ServerStatus;

  Init;
  Show;
end;

procedure TBIWebMain.CreateAllData(const AStore:String);
var S : String;
begin
  S:=TStore.PathOf(AStore);

  // Protection against self-recursivity:
  if SameText(S,'WEB:LOCALHOST') or SameText(S,'WEB:LOCALHOST:'+IntToStr(Server.DefaultPort)) then
     raise EBIException.Create('Error: Store cannot be this same "localhost" server');

  Data.Free;
  Data:=TAllData.Create(AStore);
end;

procedure TBIWebMain.CheckForUpdates;
var tmp : String;
begin
  if not TSteema.CheckNewVersion(tmp) then
     Log(DateTimeToStr(Now)+' '+Format(BIMsg_Web_ErrorCheckingUpdates,[tmp]));
end;

function DefaultStore:String;
begin
  result:=TStore.DefaultName;

  if result='' then
     raise EBIException.Create(BIMsg_Web_NoDefaultStore);
end;

Destructor TBIWebMain.Destroy;
begin
  Data.Free;
  History.Free;
  BIWeb.Free;

  inherited;
end;

procedure TBIWebMain.Init;
begin
  Server.DefaultPort:=TBIRegistry.ReadInteger('BIWeb','Port',TBIWebClient.DefaultPort);

  {$IFNDEF MSWINDOWS}
  // http://codeverge.com/embarcadero.delphi.firemonkey/indy-http-server-android-xe5/1056236
//  Server.Bindings.Add.IPVersion := id_IPv4;
  {$ENDIF}

  History:=TBIWebHistory.Create;
  History.Name:='History';

  WriteLn('BIWeb Server');
  WriteLn('Version: '+TBIWebServer.Version.ToString);
  WriteLn('Port: '+Server.DefaultPort.ToString);
end;

procedure TBIWebMain.Show;
var S : String;
begin
  if ParamCount>0 then
     S:=ParamStr(1)
  else
     S:='';

  if Trim(S)='' then
     S:=DefaultStore;

  CreateAllData(S);

  BIWeb:=TBIWebCommon.Create;
  BIWeb.Data:=Data;

  BIWeb.Logs.History:=History;
  BIWeb.Logs.AddHistory:=AddHistory;

  BIWeb.Scheduler.Refresh(S);

  Server.Active:=True;

  SetupLogs;

  Log('Started: '+DateTimeToStr(Now));
end;

procedure TBIWebMain.SetupLogs;
begin
  BIWeb.Logs.Persist:=TBIRegistry.ReadBoolean('BIWeb','LogPersist',True);
  BIWeb.Logs.Store:=TBIRegistry.ReadString('BIWeb','LogStore','');
end;

procedure TBIWebMain.Log(const S:String);
begin
  if FLog then
     WriteLn(S);
end;

procedure TBIWebMain.AddHistory(const AContext:TBIWebContext;
                                const Command:String;
                                const Tag:String;
                                const Success:Boolean;
                                const Millisec:Integer;
                                const Size:Int64);
const
  MaxItems=1000;

var tmpNow : TDateTime;
    IP: String;
begin
  tmpNow:=Now;

  IP:=TBIIndyContext(AContext).PeerIP;

  History.Add(tmpNow,IP,Command,Tag,Success,Millisec,Size);

  if not Success then
     Log(DateTimeToStr(Now)+' '+IP+' '+Command+' '+Tag);
end;

procedure TBIWebMain.ServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

  function Parameters:String;
  begin
    result:=ARequestInfo.URI;

    if ARequestInfo.QueryParams<>'' then
       result:=result+'?'+ARequestInfo.QueryParams
    else
    if ARequestInfo.Params.Count>0 then
       result:=result+' '+ARequestInfo.Params.Text;
  end;

begin
  if SameText(ARequestInfo.Document,'/favicon.ico') then
  begin
    //AResponseInfo.Pragma:='Cache-Control: public';
    AResponseInfo.ContentType:='image/x-icon';
    // AResponseInfo.ContentStream:=FaviconStream; // "data:;base64,iVBORw0KGgo="
  end
  else
    try
      if FLog then
         Log(AContext.Binding.IP+' '+Parameters);

      TBIIndyContext.Process(BIWeb,AContext,ARequestInfo,AResponseInfo);
    except
      on E:Exception do
      begin
        WriteLn('EXCEPTION');
        WriteLn(E.Message);

        AResponseInfo.ContentText:=E.Message;
      end;
    end;
end;

procedure TBIWebMain.ServerConnect(AContext: TIdContext);
begin
//  Log('Connected Client: '+AContext.Binding.IP);
end;

procedure TBIWebMain.ServerDisconnect(AContext: TIdContext);
begin
//  Log('Disconnected Client: '+AContext.Binding.IP);
end;

procedure TBIWebMain.ServerStatus(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);
begin
  Log(AStatusText);
end;

type
  TCommand = (Clients, Address, Quit, Help, Log, Unknown);

function ReadCommand(const ACommand:String):TCommand;
var tmp : String;
begin
  tmp:=UpperCase(ACommand);

  if tmp='Q' then result:=TCommand.Quit else
  if tmp='C' then result:=TCommand.Clients else
  if tmp='I' then result:=TCommand.Address else
  if tmp='L' then result:=TCommand.Log else
  if (tmp='?') or (tmp='H') then result:=TCommand.Help else

  result:=TCommand.Unknown;
end;

procedure TBIWebMain.ShowAddress;
var t : Integer;
    tmpIP : TIdStackLocalAddressList;
begin
  if Server.ServerSoftware<>'' then
     WriteLn(Server.ServerSoftware);

  WriteLn('IP v6: '+BoolToStr(GStack.SupportsIPv6,True));

  tmpIP:=TIdStackLocalAddressList.Create;
  try
    GStack.GetLocalAddressList(tmpIP);

    for t:=0 to tmpIP.Count-1 do
        WriteLn(tmpIP[t].IPAddress);
  finally
    tmpIP.Free;
  end;

  //for t:=0 to Server.Bindings.Count-1 do
  //    WriteLn(Server.Bindings.Items[t].IP);
end;

procedure TBIWebMain.ShowClients;
begin
end;

procedure TBIWebMain.ShowHelp;
begin
  Writeln('q = Quit');
  Writeln('i = Address');
  Writeln('c = Connected Clients');
  Writeln('l = Toggle logging');
  Writeln('? or h = This Help');
end;

procedure TBIWebMain.ToggleLog;
begin
  FLog:=not FLog;

  WriteLn('Logging: '+BoolToStr(FLog,True));
end;

procedure TBIWebMain.MainLoop;
var tmp : String;
begin
  repeat
    Write('>');

    ReadLn(tmp);

    case ReadCommand(tmp) of
      TCommand.Clients : ShowClients;
      TCommand.Help : ShowHelp;
      TCommand.Address : ShowAddress;
      TCommand.Log : ToggleLog;
      TCommand.Quit : break;
    else
    begin
      WriteLn('Unknown command: '+tmp);
      WriteLn;
      ShowHelp;
    end;
    end;

  until False;
end;

procedure Main;
var BIWeb : TBIWebMain;
begin
  BIWeb:=TBIWebMain.Create(nil);
  try
    BIWeb.MainLoop;
  finally
    BIWeb.Free;
  end;
end;

begin
  try
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
