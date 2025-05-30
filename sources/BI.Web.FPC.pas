{*********************************************}
{  TeeBI Software Library                     }
{  HTTP Web data access using Indy TIdHttp    }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.FPC;

interface

{
  FreePascal Lazarus implementation of the TBIHttp abstract class.
}

uses
  System.Classes, System.SysUtils,
  fpHttpClient, BI.Web, BI.Persist;

type
  EHttpAbort=class(Exception);

  TBIFPCWeb=class(TBIHttp)
  private
    FHttp : TFPHttpClient; // FPC

    //IMaxWork : Int64;
  public
    Constructor Create(const AOwner:TComponent); override;
    Destructor Destroy; override;

    class function FTP(const ADef:TDataDefinition):TBIFtp; override;

    procedure Get(const AURL:String; const AStream:TStream); overload; override;
    function Get(const AURL:String):String; overload; override;
    class function Parse(const AURL:String):TWebURL; override;
    procedure SetProxy(const AProxy:TWebProxy); override;
    procedure SetTimeout(const ATimeout:Integer); override;
  end;

implementation

uses
  BI.Languages.English, URIParser;

Constructor TBIFPCWeb.Create(const AOwner:TComponent);
begin
  //inherited;

  FHttp:=TfpHttpClient.Create(AOwner);

//  FHttp.ConnectTimeout:=TBIRegistry.ReadInteger('Settings','WebTimeout',2000);
//  FHttp.HandleRedirects:=True;
//  FHttp.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FHttp);

  {
  FHttp.OnWorkBegin:=WorkBegin;
  FHttp.OnWorkEnd:=WorkEnd;
  FHttp.OnWork:=Work;
  }
end;

Destructor TBIFPCWeb.Destroy;
begin
  FHttp.Free;
  inherited;
end;

class function TBIFPCWeb.FTP(const ADef: TDataDefinition): TBIFtp;
begin
  result:=nil; // Pending
end;

procedure TBIFPCWeb.Get(const AURL: String; const AStream: TStream);
begin
  FHttp.Get(AURL,AStream);
end;

function TBIFPCWeb.Get(const AURL: String): String;
begin
  result:=FHttp.Get(AURL);
end;

class function TBIFPCWeb.Parse(const AURL: String): TWebURL;
var tmp : TURI;
begin
  tmp:=ParseURI(AURL);

  result.Host:=tmp.Host;
  result.Port:=tmp.Port;
  result.Params:=tmp.Params;
end;

procedure TBIFPCWeb.SetProxy(const AProxy: TWebProxy);
begin
  {
  FHttp.ProxyParams.ProxyServer:=AProxy.Host;
  FHttp.ProxyParams.ProxyPort:=AProxy.Port;
  FHttp.ProxyParams.ProxyUsername:=AProxy.User;
  FHttp.ProxyParams.ProxyPassword:=AProxy.Password;
  }
end;

procedure TBIFPCWeb.SetTimeout(const ATimeout: Integer);
begin
//  FHttp.ConnectTimeout:=ATimeout;
end;

(*
procedure TBIFPCWeb.WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  IMaxWork:=0;

  if AWorkMode=TWorkMode.wmRead then
     if Assigned(OnProgress) then
        DoProgress(0,0);
end;

procedure TBIFPCWeb.WorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
  IMaxWork:=0;

  if AWorkMode=TWorkMode.wmRead then
     if Assigned(OnProgress) then
     begin
       IMaxWork:=AWorkCountMax;

       DoProgress(0,IMaxWork);
     end;
end;

procedure TBIFPCWeb.Work(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  if AWorkMode=TWorkMode.wmRead then
     if (IMaxWork>0) and Assigned(OnProgress) then
        DoProgress(AWorkCount,IMaxWork);
end;
*)

end.
