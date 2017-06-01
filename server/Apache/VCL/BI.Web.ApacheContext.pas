unit BI.Web.ApacheContext;

interface

uses
  System.Classes,
  Web.ApacheHTTP,
  BI.Web.Common;

type
  TBIApacheContext=class(TBIWebContext)
  public
    procedure Finish;

    class function Process(const BIWeb:TBIWebCommon;
                           const ARequest: TApacheRequest;
                           const AResponse: TApacheResponse):Boolean; static;

    procedure AddCookie(const AName,AValue:String); override;
    function FormParams: String; override;
    function GetContentType:String; override;
    function GetCookie(const AName:String):String; override;
    function GetStream:TStream; override;
    function Headers: TStrings; override;
    function Params:TStrings; override;
    procedure Redirect(const AURL: String); override;
    function PeerIP:String;
    function ResponseSize:Int64; override;
    procedure ReturnFile(const AFile:String); override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils, BI.DataSource, BI.Persist, Web.HttpApp;

{ TBIApacheContext }

procedure TBIApacheContext.Finish;
var tmp : TApacheResponse;
begin
  tmp:=TApacheResponse(ResponseInfo);
  tmp.Content:=ContentText;

  if ContentType='' then
     if (ContentText<>'') and (ResponseStream=nil) then
        ContentType:='text/html; charset=ISO-8859-1';

  tmp.ContentType:=AnsiString(ContentType);
  tmp.ContentStream:=ResponseStream;
end;

function TBIApacheContext.FormParams: String;
begin
  result:=TApacheRequest(RequestInfo).ContentFields.Text; //FormParams;
end;

function TBIApacheContext.Params: TStrings;
var tmp : TApacheRequest;
begin
  tmp:=TApacheRequest(RequestInfo);

  if tmp.MethodType=TMethodType.mtGet then
     result:=tmp.QueryFields
  else
     result:=tmp.ContentFields;
end;

function TBIApacheContext.PeerIP: String;
begin
  result:=String(TApacheRequest(RequestInfo).RemoteAddr);
end;

class function TBIApacheContext.Process(const BIWeb: TBIWebCommon;
  const ARequest: TApacheRequest; const AResponse: TApacheResponse):Boolean;
var tmp : TBIApacheContext;
begin
  tmp:=TBIApacheContext.Create;
  try
    tmp.ResponseInfo:=AResponse;
    tmp.RequestInfo:=ARequest;

    //tmp.ContentText:='Ini: '+TBIRegistry.IniFile;

    {$IFDEF DEBUGREQUEST}
    tmp.ContentText:=ARequest.URL;

    if ARequest.MethodType=TMethodType.mtGet then
       tmp.ContentText:=tmp.ContentText+' get'
    else
    if ARequest.MethodType=TMethodType.mtPost then
       tmp.ContentText:=tmp.ContentText+' post';

    tmp.ContentText:=tmp.ContentText+' '+ARequest.Query+' '+ARequest.PathInfo;
    {$ENDIF}

    {$IFDEF IIS_STATIC}
    // Pending: We cannot return static files yet
    if ARequest.URL<>'/' then
       BIWeb.ProcessFile(String(ARequest.URL),tmp)
    else
    {$ENDIF}

    if ARequest.MethodType=TMethodType.mtGet then
       BIWeb.ProcessGet(tmp)
    else
    if ARequest.MethodType=TMethodType.mtPost then
       BIWeb.ProcessPost(tmp);

    tmp.Finish;

    result:=True;
  finally
    tmp.Free;
  end;
end;

function TBIApacheContext.ResponseSize: Int64;
begin
  result:=TApacheResponse(ResponseInfo).ContentLength;

  if result=-1 then
     if ResponseStream<>nil then
        result:=ResponseStream.Size
     else
        result:=Length(ContentText);
end;

procedure TBIApacheContext.ReturnFile(const AFile: String);
var //Enable : Boolean;
    tmp : TApacheResponse;
    tmpStream : TStream;
begin
  tmp:=TApacheResponse(ResponseInfo);

//  if ContentType='' then
//     ContentType:=tmp.HTTPServer.MIMETable.GetFileMIMEType(AFile);

  tmp.ContentLength:=TBIFileSource.GetFileSize(AFile);

  tmpStream:=TFileStream.Create(AFile,fmOpenRead and fmShareDenyWrite);
  tmp.ContentStream:=tmpStream;
//  tmp.ContentType:='application/x-zip-compressed';
  tmp.SetCustomHeader('Content-Disposition','filename='+ExtractFilename(AFile));

//  tmp.WriteHeader;
//  Enable:=not (IContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase);
//  IContext.Connection.IOHandler.WriteFile(AFile, Enable);
end;

end.
