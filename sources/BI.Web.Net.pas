{*********************************************}
{  TeeBI Software Library                     }
{  HTTP Web data access using System.Net      }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.Net;

interface

{
 In TeeBI by default, Indy TIdHttp client component is used.

 To use the standard RTL THttp class instead of Indy:

 uses BI.Web.Net;
 TBIHttp.Engine:=TBIHttpClient;

}

{$IF CompilerVersion>27}
{$DEFINE THREADING}
{$ENDIF}

uses
  System.Classes, BI.Web,

  {$IFDEF THREADING}
  System.Threading,
  {$ENDIF}

  System.Net.HttpClient, System.Net.HttpClientComponent, BI.Persist;

type
  TBIHttpClient=class(TBIHttp)
  private
    FHttp : TNetHttpClient;  // System.Net

    {$IFDEF THREADING}
    FTask : ITask;

    procedure ReceivedData(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
    procedure RequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
    {$ENDIF}
  public
    Constructor Create(const AOwner:TComponent); override;
    Destructor Destroy; override;

    class function FTP(const ADef:TDataDefinition):TBIFtp; override;
    procedure Get(const AURL:String; const AStream:TStream); overload; override;
    function Get(const AURL:String):String; overload; override;
    class function Parse(const AURL: String): TWebURL; override;
    procedure SetProxy(const AProxy:TWebProxy); override;
    procedure SetTimeout(const ATimeout:Integer); override;

    property Http:TNetHTTPClient read FHttp;
  end;

implementation

uses
  System.SysUtils, System.Net.URLClient, BI.Arrays;

Constructor TBIHttpClient.Create(const AOwner:TComponent);
begin
  inherited;
  FHttp:=TNetHTTPClient.Create(AOwner);
end;

procedure TBIHttpClient.Get(const AURL: String; const AStream: TStream);
begin
  {$IFDEF THREADING}
  if Assigned(OnProgress) then
  begin
    FHttp.OnReceiveData:=ReceivedData;
    FHttp.OnRequestCompleted:=RequestCompleted;

    FTask:=TTask.Run(procedure
    begin
      FHttp.Get(AURL,AStream);
    end);

    FTask.Wait;
    FTask:=nil;
  end
  else
  {$ENDIF}
  begin
    FHttp.OnReceiveData:=nil;
    FHttp.OnRequestCompleted:=nil;

    //FHttp.CustomHeaders['Connection']:='keep-alive';
    //FHttp.CustomHeaders['Keep-Alive']:='timeout=30, max=3 header';

    FHttp.Get(AURL,AStream);
  end;
end;

destructor TBIHttpClient.Destroy;
begin
  FHttp.Free;
  inherited;
end;

function TBIHttpClient.Get(const AURL: String): String;
begin
  result:=FHttp.Get(AURL).ContentAsString;
end;

class function TBIHttpClient.Parse(const AURL: String): TWebURL;
var tmp : TURI;
begin
  tmp:=TURI.Create(AURL);

  result.Host:=tmp.Host;
  result.Port:=tmp.Port;
  result.Params:=tmp.Query;
end;

{$IFDEF THREADING}
procedure TBIHttpClient.ReceivedData(const Sender: TObject; AContentLength,
  AReadCount: Int64; var Abort: Boolean);
begin
  if FTask<>nil then
     if Assigned(OnProgress) then
        OnProgress(Self,AReadCount,AContentLength,Abort);
end;

procedure TBIHttpClient.RequestCompleted(const Sender: TObject;
  const AResponse: IHTTPResponse);
var Abort : Boolean;
begin
  if FTask<>nil then
     if Assigned(OnProgress) then
     begin
       Abort:=False;
       OnProgress(Self,0,0,Abort);
     end;
end;
{$ENDIF}

procedure TBIHttpClient.SetProxy(const AProxy: TWebProxy);
var tmp : TProxySettings;
    s : String;
begin
  s:=Trim(AProxy.Host);

  if s<>'' then
  begin
    tmp.Host:=AProxy.Host;
    tmp.Port:=AProxy.Port;
    tmp.UserName:=AProxy.User;
    tmp.Password:=AProxy.Password;

    FHttp.ProxySettings:=tmp;
  end;
end;

procedure TBIHttpClient.SetTimeout(const ATimeout: Integer);
begin
  {$IF CompilerVersion>30}
  FHttp.ConnectionTimeout:=ATimeout;
  {$ENDIF}
end;

class function TBIHttpClient.FTP(const ADef:TDataDefinition): TBIFtp;
begin
  raise EBIException.Create('FTP protocol not supported');
end;

end.
