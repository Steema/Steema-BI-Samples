{*********************************************}
{  TeeBI Software Library                     }
{  Web Server for Linux                       }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
program BI_Linux_Web;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.SyncObjs,

  BI.Web,
  BI.Web.AllData in '..\BI.Web.AllData.pas',
  BI.Web.Common in '..\BI.Web.Common.pas',
  BI.Web.IndyContext in '..\BI.Web.IndyContext.pas',

  BI.Arrays, BI.DataItem, BI.Persist, System.IOUtils, BI.UI,
  BI.CSV, BI.Html, BI.JSON, BI.ClientDataset,
  BI.XMLData, BI.DB,

  {$IFNDEF FPC}
  {$IF CompilerVersion>26}
  BI.DB.Fire,
  {$ELSE}
  BI.DB.SqlExpr,
  {$ENDIF}
  {$ENDIF}

  BI.Web.Server.Indy in '..\BI.Web.Server.Indy.pas',

  BI.Languages.English, BI.Languages.Spanish;

type
  TBIWebMain=class(TComponent)
  private
    Data : TAllData;
    History : TBIWebHistory;

    BIWeb : TBIWebCommon;

    Server : THttpServer;

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

    procedure ServerConnect(const AContext: TBIWebContext);
    procedure ServerDisconnect(const AContext: TBIWebContext);
    procedure ServerException(const AContext: TBIWebContext; const AException: Exception);
    procedure ServerStatus(const ASender: TObject; const AStatusText: string);
    procedure ServerCommandGet(const AContext: TBIWebContext);

    procedure Show;
    procedure ShowAddresses;
    procedure ShowClients;
    procedure ShowHelp;
    procedure ToggleLog;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;
  end;

{ TBIWebMain }

Constructor TBIWebMain.Create;

  procedure CreateServer;
  begin
    Server:=THttpServer.Engine.Create(Self);

    Server.Port:=TBIRegistry.ReadInteger('BIWeb','Port',TBIWebClient.DefaultPort);

    Server.OnCommandGet:=ServerCommandGet;
    Server.OnConnect:=ServerConnect;
    Server.OnDisconnect:=ServerDisconnect;
    Server.OnException:=ServerException;
    Server.OnStatus:=ServerStatus;
  end;

begin
  inherited;

  CreateServer;

  Init;
  Show;
end;

procedure TBIWebMain.CreateAllData(const AStore:String);
var S : String;
begin
  S:=TStore.PathOf(AStore);

  // Protection against self-recursivity:
  if SameText(S,'WEB:LOCALHOST') or SameText(S,'WEB:LOCALHOST:'+IntToStr(Server.Port)) then
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
  {$IFNDEF MSWINDOWS}
  // http://codeverge.com/embarcadero.delphi.firemonkey/indy-http-server-android-xe5/1056236
//  Server.Bindings.Add.IPVersion := id_IPv4;
  {$ENDIF}

  History:=TBIWebHistory.Create;
  History.Name:='History';

  WriteLn('BIWeb Server');
  WriteLn('Version: '+TBIWebServer.Version.ToString);
  WriteLn('Port: '+Server.Port.ToString);
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

procedure TBIWebMain.ServerCommandGet(const AContext: TBIWebContext);

  {
  function Parameters:String;
  begin
    result:=ARequestInfo.URI;

    if ARequestInfo.QueryParams<>'' then
       result:=result+'?'+ARequestInfo.QueryParams
    else
    if ARequestInfo.Params.Count>0 then
       result:=result+' '+ARequestInfo.Params.Text;
  end;
  }

begin
  if SameText(AContext.GetDocument,'/favicon.ico') then
     // AContext.ReturnIcon(FaviconStream)
  else
  try
    TBIIndyContext.Process(BIWeb,AContext);
  except
    on E:Exception do
    begin
      Log(E.Message);
      AContext.SetResponse(E.Message);
    end;
  end;
end;

procedure TBIWebMain.ServerConnect(const AContext: TBIWebContext);
begin
//  Log('Connected Client: '+AContext.Binding.IP);
end;

procedure TBIWebMain.ServerDisconnect(const AContext: TBIWebContext);
begin
//  Log('Disconnected Client: '+AContext.Binding.IP);
end;

procedure TBIWebMain.ServerException(const AContext: TBIWebContext;
  const AException: Exception);
begin
  Log(AException.Message);
end;

procedure TBIWebMain.ServerStatus(const ASender: TObject; const AStatusText: string);
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

procedure TBIWebMain.ShowAddresses;
var t : Integer;
    tmp : TStrings;
begin
  tmp:=Server.GetAddresses;
  try
    for t:=0 to tmp.Count-1 do
        WriteLn(tmp[t]);
  finally
    tmp.Free;
  end;
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
      TCommand.Address : ShowAddresses;
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
