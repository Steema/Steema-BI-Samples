{*********************************************}
{  TeeBI Software Library                     }
{  Indy implementation of abstract Web Server }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.IndyContext;

interface

uses
  System.Classes,
  IdContext, IdCustomHTTPServer,
  BI.Web.Context, BI.Web.Common;

type
  TBIIndyContext=class(TWebContext)
  private
    function IContext:TIdContext;
  public
    Constructor Create(const AContext:TIdContext);

    procedure AddCookie(const AHost,AName,AValue:String); override;

    procedure Finish;
    function FormParams: String; override;
    function GetContentType:String; override;
    function GetCookie(const AName:String):String; override;
    function GetDocument:String; override;
    function GetStream:TStream; override;
    function Headers: TStrings; override;

    class procedure Process(const BIWeb:TBIWebCommon;
                            const AContext:TWebContext); static;

    function Params:TStrings; override;
    function PeerIP:String;
    function Post(const AURL:String; const AParams:TStrings):String; override;
    procedure Redirect(const AURL: String); override;
    function ResponseSize:Int64; override;
    procedure ReturnFile(const AFile:String); override;
    procedure ReturnIcon(const AStream:TStream); override;

    procedure SetResponse(const AText: String); override;
    procedure SetResponse(const AType: String; const AStream:TStream); override;
  end;

implementation

uses
  IdSSL, IdCookie, IdURI, IdHttp,
  BI.DataSource;

{ TBIIndyContext }

constructor TBIIndyContext.Create(const AContext: TIdContext);
begin
  inherited Create;
  Context:=AContext;
end;

procedure TBIIndyContext.Finish;
var tmp : TIdHTTPResponseInfo;
begin
  tmp:=TIdHTTPResponseInfo(ResponseInfo);
  tmp.ContentText:=ContentText;
  tmp.ContentType:=ContentType;
  tmp.ContentStream:=ResponseStream;
end;

function TBIIndyContext.FormParams: String;
begin
  result:=TIdHTTPRequestInfo(RequestInfo).FormParams;
end;

function TBIIndyContext.Headers: TStrings;
begin
  result:=TIdHTTPRequestInfo(RequestInfo).RawHeaders;
end;

function TBIIndyContext.IContext: TIdContext;
begin
  result:=TIdContext(Context);
end;

function TBIIndyContext.Params: TStrings;
begin
  result:=TIdHTTPRequestInfo(RequestInfo).Params;
end;

function TBIIndyContext.PeerIP: String;
begin
  result:=IContext.Connection.Socket.Binding.PeerIP;
end;

function TBIIndyContext.Post(const AURL: String; const AParams: TStrings): String;
var HTTP: TIdHTTP;
begin
  HTTP:=TIdHTTP.Create(nil);
  try
    result:=HTTP.Post(AURL,AParams);
  finally
    HTTP.Free;
  end;
end;

function TBIIndyContext.ResponseSize: Int64;
begin
  result:=TIdHTTPResponseInfo(ResponseInfo).ContentLength;

  if result=-1 then
     if ResponseStream<>nil then
        result:=ResponseStream.Size
     else
        result:=Length(ContentText);
end;

procedure TBIIndyContext.Redirect(const AURL: String);
begin
  TIdHTTPResponseInfo(ResponseInfo).Redirect(AURL);
end;

procedure TBIIndyContext.ReturnFile(const AFile: String);
var Enable : Boolean;
begin
  if ContentType='' then
     ContentType:=TIdHTTPResponseInfo(ResponseInfo).HTTPServer.MIMETable.GetFileMIMEType(AFile);

  TIdHTTPResponseInfo(ResponseInfo).ContentLength:=TBIFileSource.GetFileSize(AFile);

  TIdHTTPResponseInfo(ResponseInfo).WriteHeader;
  Enable:=not (IContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase);
  IContext.Connection.IOHandler.WriteFile(AFile, Enable);
end;

procedure TBIIndyContext.ReturnIcon(const AStream: TStream);
begin
  SetResponse('image/x-icon',AStream);// "data:;base64,iVBORw0KGgo="
end;

procedure TBIIndyContext.SetResponse(const AText: String);
begin
  //TIdHTTPResponseInfo(ResponseInfo).
  ContentText:=AText;
end;

procedure TBIIndyContext.SetResponse(const AType: String;
  const AStream: TStream);
begin
  ContentType:=AType;
  ResponseStream:=AStream;
end;

class procedure TBIIndyContext.Process(const BIWeb:TBIWebCommon;
                                       const AContext:TWebContext);
begin
  if AContext.GetDocument<>'/' then
     BIWeb.ProcessFile(AContext.GetDocument,AContext)
  else
  if TIdHttpRequestInfo(AContext.RequestInfo).CommandType=hcGET then
     BIWeb.ProcessGet(AContext)
  else
  if TIdHttpRequestInfo(AContext.RequestInfo).CommandType=hcPOST then
     BIWeb.ProcessPost(AContext);

  TBIIndyContext(AContext).Finish;
end;

function TBIIndyContext.GetContentType: String;
begin
  result:=TIdHTTPRequestInfo(RequestInfo).ContentType;
end;

function TBIIndyContext.GetCookie(const AName:String):String;
var tmp : TIdCookie;
begin
  tmp:=TIdHTTPRequestInfo(RequestInfo).Cookies.Cookie[AName,''];

  if tmp=nil then
     result:=''
  else
     result:=tmp.Value;
end;

function TBIIndyContext.GetDocument: String;
begin
  result:=TIdHTTPRequestInfo(RequestInfo).Document;
end;

function TBIIndyContext.GetStream: TStream;
begin
  result:=TIdHTTPRequestInfo(RequestInfo).PostStream;
end;

procedure TBIIndyContext.AddCookie(const AHost,AName,AValue:String);
var URI : TIdURI;
begin
  URI:=TIdURI.Create(AHost);
  try
    TIdHTTPResponseInfo(ResponseInfo).Cookies.AddServerCookie(AName+'='+AValue,URI);
  finally
    URI.Free;
  end;
end;

end.
