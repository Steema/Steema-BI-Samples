{*********************************************}
{  TeeBI Software Library                     }
{  HTTP Web data access using Indy TIdHttp    }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.Indy;

interface

uses
  System.Classes, System.SysUtils,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  BI.Web, BI.Persist;

type
  TBIIndy=class(TBIHttp)
  private
    FHttp : TIdHttp; // Indy

    IMaxWork : Int64;

    procedure WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  public
    Constructor Create(const AOwner:TComponent); override;
    Destructor Destroy; override;

    class function FTP(const ADef:TDataDefinition):TBIFtp; override;

    procedure Get(const AURL:String; const AStream:TStream); overload; override;
    function Get(const AURL:String):String; overload; override;
    class function Parse(const AURL:String):TWebURL; override;
    procedure SetProxy(const AProxy:TWebProxy); override;
    procedure SetTimeout(const ATimeout:Integer); override;

    property Http:TIdHTTP read FHttp;
  end;

implementation

uses
  System.Types, BI.Languages.English, IdFtp, IdURI;

Constructor TBIIndy.Create(const AOwner:TComponent);
begin
  inherited;

  FHttp:=TIdHttp.Create(AOwner);

  FHttp.HandleRedirects:=True;

//  FHttp.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FHttp);

  FHttp.OnWorkBegin:=WorkBegin;
  FHttp.OnWorkEnd:=WorkEnd;
  FHttp.OnWork:=Work;
end;

Destructor TBIIndy.Destroy;
begin
  FHttp.Free;
  inherited;
end;

procedure TBIIndy.Get(const AURL: String; const AStream: TStream);
begin
  FHttp.Get(AURL,AStream);
end;

type
  TBIIndyFtp=class(TBIFtp)
  private
    FIdFtp : TIdFtp;
  public
    Constructor Create(const ADef:TDataDefinition); override;
    Destructor Destroy; override;

    procedure Connect; override;
    procedure DisConnect; override;
    function Get(const AFileName:String):TStream; override;
    function IncludedFiles(const AFolder,AIncludeMask:String):TStringDynArray; override;
    function ListFiles(const AFolder,AIncludeMask:String): TStrings; override;
  end;

{ TBIIndyFtp }

Constructor TBIIndyFtp.Create(const ADef:TDataDefinition);
begin
  inherited;
  FIdFtp:=TIdFtp.Create;

  FIdFtp.Host:=ADef['FTPServer'];
  FIdFtp.Port:=StrToIntDef(ADef['FTPPort'],DefaultPort);
  FIdFtp.ConnectTimeout:=StrToIntDef(ADef['Timeout'],TBIWebClient.DefaultTimeout);
  FIdFtp.Username:=ADef['FTPUser'];
  FIdFtp.Password:=ADef['FTPPass'];
end;

destructor TBIIndyFtp.Destroy;
begin
  FIdFtp.Free;
  inherited;
end;

procedure TBIIndyFtp.DisConnect;
begin
  FIdFtp.Disconnect;
end;

function TBIIndyFtp.Get(const AFileName:String):TStream;
begin
  result:=TMemoryStream.Create;
  FIdFtp.Get(AFileName,result);
end;

function TBIIndyFtp.ListFiles(const AFolder,AIncludeMask:String): TStrings;
begin
  if AFolder<>'' then
     FIdFtp.ChangeDir(AFolder);

  result:=TStringList.Create;
  try
    FIdFtp.List(result,AIncludeMask,False);
  except
    on Exception do
    begin
      result.Free;
      raise;
    end;
  end;
end;

function TBIIndyFtp.IncludedFiles(const AFolder,AIncludeMask:String): TStringDynArray;
var tmp : TStrings;
    t : Integer;
begin
  // Pending: ADef['Recursive']  ADef['ExcludeMask']
  FIdFtp.Connect;
  try
    tmp:=ListFiles(AFolder,AIncludeMask);
    try
      SetLength(result,tmp.Count);

      for t:=0 to tmp.Count-1 do
          result[t]:=tmp[t];
    finally
      tmp.Free;
    end;
  finally
    FIdFtP.Disconnect;
  end;
end;

procedure TBIIndyFtp.Connect;
begin
  FIdFtp.Connect;
end;

class function TBIIndy.FTP(const ADef:TDataDefinition): TBIFtp;
begin
  result:=TBIIndyFtp.Create(ADef);
end;

function TBIIndy.Get(const AURL: String): String;
begin
  result:=FHttp.Get(AURL);
end;

class function TBIIndy.Parse(const AURL: String): TWebURL;
var tmp : TIdURI;
begin
  tmp:=TIdURI.Create(AURL);
  try
    result.Host:=tmp.Host;
    result.Port:=StrToIntDef(tmp.Port,-1);
    result.Params:=tmp.Params;
  finally
    tmp.Free;
  end;
end;

procedure TBIIndy.SetProxy(const AProxy: TWebProxy);
begin
  FHttp.ProxyParams.ProxyServer:=AProxy.Host;
  FHttp.ProxyParams.ProxyPort:=AProxy.Port;
  FHttp.ProxyParams.ProxyUsername:=AProxy.User;
  FHttp.ProxyParams.ProxyPassword:=AProxy.Password;
end;

procedure TBIIndy.SetTimeout(const ATimeout: Integer);
begin
  FHttp.ConnectTimeout:=ATimeout;
end;

procedure TBIIndy.WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  IMaxWork:=0;

  if AWorkMode=TWorkMode.wmRead then
     if Assigned(OnProgress) then
        DoProgress(0,0);
end;

procedure TBIIndy.WorkBegin(ASender: TObject; AWorkMode: TWorkMode;
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

procedure TBIIndy.Work(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  if AWorkMode=TWorkMode.wmRead then
     if (IMaxWork>0) and Assigned(OnProgress) then
        DoProgress(AWorkCount,IMaxWork);
end;

end.

