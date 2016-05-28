unit BI.Web.IndyContext;

interface

uses
  System.Classes,
  IdContext, IdCustomHTTPServer,
  BI.Web.Common;

type
  TBIIndyContext=class(TBIWebContext)
  private
    IContext : TIdContext;
  public
    procedure Finish;
    function FormParams: String; override;

    class procedure Process(const BIWeb:TBIWebCommon;
                      const AContext:TIdContext;
                      const ARequestInfo: TIdHTTPRequestInfo;
                      const AResponseInfo: TIdHTTPResponseInfo); static;

    function Params:TStrings; override;
    function PeerIP:String;
    function ResponseSize:Int64; override;
    procedure ReturnFile(const AFile:String); override;
  end;

implementation

uses
  IdSSL,
  BI.DataSource;

{ TBIIndyContext }

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

function TBIIndyContext.Params: TStrings;
begin
  result:=TIdHTTPRequestInfo(RequestInfo).Params;
end;

function TBIIndyContext.PeerIP: String;
begin
  result:=IContext.Connection.Socket.Binding.PeerIP;
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

class procedure TBIIndyContext.Process(const BIWeb:TBIWebCommon;
                        const AContext:TIdContext;
                        const ARequestInfo: TIdHTTPRequestInfo;
                        const AResponseInfo: TIdHTTPResponseInfo);
var tmp : TBIIndyContext;
begin
  tmp:=TBIIndyContext.Create;
  try
    tmp.IContext:=AContext;
    tmp.RequestInfo:=ARequestInfo;
    tmp.ResponseInfo:=AResponseInfo;

    if ARequestInfo.Document<>'/' then
       BIWeb.ProcessFile(ARequestInfo.Document,tmp)
    else
    if ARequestInfo.CommandType=hcGET then
       BIWeb.ProcessGet(tmp)
    else
    if ARequestInfo.CommandType=hcPOST then
       BIWeb.ProcessPost(tmp);

    tmp.Finish;
  finally
    tmp.Free;
  end;
end;

end.
