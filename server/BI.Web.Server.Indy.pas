{*********************************************}
{  TeeBI Software Library                     }
{  HTTP Web Server using Indy                 }
{                                             }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.Server.Indy;

interface

uses
  {System.}Classes, {System.}SysUtils,
  BI.Web.Context;

type
  THttpServerCommandEvent=procedure(const AContext: TWebContext) of object;
  THttpServerConnectEvent=procedure(const AContext: TWebContext) of object;
  THttpServerExceptionEvent=procedure(const AContext: TWebContext; const AException: Exception) of object;
  THttpServerStatusEvent=procedure(const ASender: TObject; const AStatusText: string) of object;

  THttpServerClass=class of THttpServer;

  THttpServer=class(TComponent)
  private
    FConnect,
    FDisconnect : THttpServerConnectEvent;
    FCommandGet : THttpServerCommandEvent;
    FException : THttpServerExceptionEvent;
    FStatus : THttpServerStatusEvent;
  protected
    function GetActive: Boolean; virtual; abstract;
    procedure SetActive(const Value: Boolean); virtual; abstract;

    function GetPort: Integer; virtual; abstract;
    procedure SetPort(const Value: Integer); virtual; abstract;
  public
    class var Engine : THttpServerClass;

    function ContextsCount: Integer; virtual; abstract;
    function GetAddresses:TStrings; virtual;

    property Active:Boolean read GetActive write SetActive default False;
    property Port:Integer read GetPort write SetPort default 15015;

    property OnCommandGet:THttpServerCommandEvent read FCommandGet write FCommandGet;
    property OnConnect:THttpServerConnectEvent read FConnect write FConnect;
    property OnDisconnect:THttpServerConnectEvent read FDisconnect write FDisconnect;
    property OnException:THttpServerExceptionEvent read FException write FException;
    property OnStatus:THttpServerStatusEvent read FStatus write FStatus;
  end;

implementation

uses
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdCustomHTTPServer,
  IdHTTPServer, IdContext, BI.Web.IndyContext, IdStack,
  IdSSLOpenSSLHeaders, IdServerIOHandlerSSLOpenSSL; // Added SSL units

type
  TIndyHttpServer=class(THttpServer)
  private
    Server: TIdHTTPServer;
    FSSLEnabled: Boolean;
    FSSLCertFile: string;
    FSSLKeyFile: string;
    FSSLPassword: string;
    FSSLPort: Integer;
    FIOHandler: TIdServerIOHandlerSSLOpenSSL;

    function GetActive: Boolean; override;
    procedure SetActive(const Value: Boolean); override;

    function GetPort: Integer; override;
    procedure SetPort(const Value: Integer); override;
    function GetSSLPort: Integer;
    procedure SetSSLPort(const Value: Integer);

    procedure ServerConnect(AContext: TIdContext);
    procedure ServerCommandGet(AContext: TIdContext;
                               ARequestInfo: TIdHTTPRequestInfo;
                               AResponseInfo: TIdHTTPResponseInfo);
    procedure ServerDisconnect(AContext: TIdContext);
    procedure ServerException(AContext: TIdContext; AException: Exception);
    procedure ServerStatus(ASender: TObject; const AStatus: TIdStatus;
                           const AStatusText: string);
  public
    Constructor Create(AOwner:TComponent); override;

    function ContextsCount: Integer; override;
    function GetAddresses:TStrings; override;

  public // Added public keyword for properties
    property SSLEnabled: Boolean read FSSLEnabled write FSSLEnabled;
    property SSLCertFile: string read FSSLCertFile write FSSLCertFile;
    property SSLKeyFile: string read FSSLKeyFile write FSSLKeyFile;
    property SSLPassword: string read FSSLPassword write FSSLPassword;
    property SSLPort: Integer read GetSSLPort write SetSSLPort;
  end;

{ TIndyHttpServer }

Constructor TIndyHttpServer.Create(AOwner: TComponent);
begin
  inherited;

  FIOHandler := TIdServerIOHandlerSSLOpenSSL.Create(Self); // Initialize FIOHandler
  FSSLPort := 15443; // Default SSL port

  Server:=TIdHTTPServer.Create(Self);
  Server.DefaultPort:=15015;

  Server.OnConnect:=ServerConnect;
  Server.OnDisconnect:=ServerDisconnect;
  Server.OnException:=ServerException;
  Server.OnStatus:=ServerStatus;
  Server.OnCommandGet:=ServerCommandGet;

  {$IFNDEF LINUX}
  {$IFNDEF MSWINDOWS}
  // http://codeverge.com/embarcadero.delphi.firemonkey/indy-http-server-android-xe5/1056236
  Server.Bindings.Add.IPVersion := id_IPv4;
  {$ENDIF}
  {$ENDIF}
end;

procedure TIndyHttpServer.ServerConnect(AContext: TIdContext);
var tmp : TWebContext;
begin
  if Assigned(FConnect) then
  begin
    tmp:=TBIIndyContext.Create(AContext);
    try
      FConnect(tmp);
    finally
      tmp.Free;
    end;
  end;
end;

procedure TIndyHttpServer.ServerDisconnect(AContext: TIdContext);
var tmp : TWebContext;
begin
  if Assigned(FDisconnect) then
  begin
    tmp:=TBIIndyContext.Create(AContext);
    try
      FDisconnect(tmp);
    finally
      tmp.Free;
    end;
  end;
end;

procedure TIndyHttpServer.ServerException(AContext: TIdContext;
  AException: Exception);
var tmp : TWebContext;
begin
  if Assigned(FException) then
  begin
    tmp:=TBIIndyContext.Create(AContext);
    try
      FException(tmp,AException);
    finally
      tmp.Free;
    end;
  end;
end;

procedure TIndyHttpServer.ServerStatus(ASender: TObject; const AStatus: TIdStatus;
   const AStatusText: string);
begin
  if Assigned(FStatus) then
     FStatus(ASender,AStatusText);
end;

procedure TIndyHttpServer.ServerCommandGet(AContext: TIdContext;
    ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var tmp : TWebContext;
begin
  if Assigned(FCommandGet) then
  begin
    tmp:=TBIIndyContext.Create(AContext);
    try
      tmp.RequestInfo:=ARequestInfo;
      tmp.ResponseInfo:=AResponseInfo;
      FCommandGet(tmp);
    finally
      tmp.Free;
    end;
  end;
end;

function TIndyHttpServer.GetActive: Boolean;
begin
  result:=Server.Active;
end;

function TIndyHttpServer.GetAddresses: TStrings;
var tmpIP : TIdStackLocalAddressList;
    t : Integer;
begin
  result:=inherited;

  tmpIP:=TIdStackLocalAddressList.Create;
  try
    GStack.GetLocalAddressList(tmpIP);

    for t:=0 to tmpIP.Count-1 do
        result.Add(tmpIP[t].IPAddress);
  finally
    tmpIP.Free;
  end;

  //for t:=0 to Server.Bindings.Count-1 do
  //    WriteLn(Server.Bindings.Items[t].IP);
end;

function TIndyHttpServer.ContextsCount: Integer;
begin
  result:=Server.Contexts.Count;
end;

function TIndyHttpServer.GetPort: Integer;
begin
  result:=Server.DefaultPort;
end;

procedure TIndyHttpServer.SetActive(const Value: Boolean);
begin
  if Value then
  begin
    if FSSLEnabled then
    begin
      Server.IOHandler := FIOHandler;
      FIOHandler.SSLOptions.CertFile := FSSLCertFile;
      FIOHandler.SSLOptions.KeyFile := FSSLKeyFile;
      if FSSLPassword <> '' then
        FIOHandler.SSLOptions.Password := FSSLPassword;
      FIOHandler.SSLOptions.Method := sslvTLSv1_2; // Or a more current version if available
      FIOHandler.SSLOptions.Mode := sslmServer;

      if (FSSLPort > 0) and (FSSLPort <> Port) then
        Server.DefaultPort := FSSLPort
      else
        Server.DefaultPort := Port; // Fallback to non-SSL port if SSLPort is not set

      Server.Bindings.Clear;
      Server.Bindings.Add.Port := Server.DefaultPort;
      // Note: Consider adding IP binding configuration here if necessary
    end
    else
    begin
      Server.IOHandler := nil;
      Server.DefaultPort := Port; // Ensure non-SSL port is used
      Server.Bindings.Clear;
      Server.Bindings.Add.Port := Server.DefaultPort;
    end;
  end;
  Server.Active := Value;
end;

procedure TIndyHttpServer.SetPort(const Value: Integer);
begin
  Server.DefaultPort:=Value;
end;

function TIndyHttpServer.GetSSLPort: Integer;
begin
  result := FSSLPort;
end;

procedure TIndyHttpServer.SetSSLPort(const Value: Integer);
begin
  FSSLPort := Value;
end;

{ THttpServer }

function THttpServer.GetAddresses: TStrings;
begin
  result:=TStringList.Create;
end;

initialization
  THttpServer.Engine:=TIndyHttpServer;
end.
