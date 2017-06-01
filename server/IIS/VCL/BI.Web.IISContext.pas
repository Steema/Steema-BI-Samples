unit BI.Web.IISContext;

interface

uses
  System.Classes,
  Web.HTTPApp,
  BI.Web.Common;

type
  TBIIISContext=class(TBIWebContext)
  public
    procedure Finish;

    class function Process(const BIWeb:TBIWebCommon;
                           const ARequest: TWebRequest;
                           const AResponse: TWebResponse):Boolean; static;

    procedure AddCookie(const AName,AValue:String); override;
    function FormParams: String; override;
    function GetContentType:String; override;
    function GetCookie(const AName:String):String; override;
    function GetStream:TStream; override;
    function Headers: TStrings; override;
    function Params:TStrings; override;
    function PeerIP:String;
    procedure Redirect(const AURL: String); override;
    function ResponseSize:Int64; override;
    procedure ReturnFile(const AFile:String); override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils, BI.DataSource, BI.Persist;

{ TBIIISContext }

procedure TBIIISContext.Finish;
var tmp : TWebResponse;
begin
  tmp:=TWebResponse(ResponseInfo);
  tmp.Content:=ContentText;

  if ContentType='' then
     if (ContentText<>'') and (ResponseStream=nil) then
        ContentType:='text/html; charset=ISO-8859-1';

  tmp.ContentType:=AnsiString(ContentType);
  tmp.ContentStream:=ResponseStream;
end;

function TBIIISContext.FormParams: String;
begin
  result:=TWebRequest(RequestInfo).ContentFields.Text; //FormParams;
end;

function TBIIISContext.Params: TStrings;
var tmp : TWebRequest;
begin
  tmp:=TWebRequest(RequestInfo);

  if tmp.MethodType=TMethodType.mtGet then
     result:=tmp.QueryFields
  else
     result:=tmp.ContentFields;
end;

function TBIIISContext.PeerIP: String;
begin
  result:=String(TWebRequest(RequestInfo).RemoteAddr);
end;

class function TBIIISContext.Process(const BIWeb: TBIWebCommon;
  const ARequest: TWebRequest; const AResponse: TWebResponse):Boolean;
var tmp : TBIIISContext;
begin
  tmp:=TBIIISContext.Create;
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

function TBIIISContext.ResponseSize: Int64;
begin
  result:=TWebResponse(ResponseInfo).ContentLength;

  if result=-1 then
     if ResponseStream<>nil then
        result:=ResponseStream.Size
     else
        result:=Length(ContentText);
end;

procedure TBIIISContext.ReturnFile(const AFile: String);
var //Enable : Boolean;
    tmp : TWebResponse;
    tmpStream : TStream;
begin
  tmp:=TWebResponse(ResponseInfo);

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
