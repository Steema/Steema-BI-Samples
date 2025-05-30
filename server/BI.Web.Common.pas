{*********************************************}
{  TeeBI Software Library                     }
{  Web Server (VCL and Firemonkey)            }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.Common;

interface

uses
  System.Classes, System.Diagnostics, System.SyncObjs, System.UITypes,

  BI.Arrays, BI.Persist,

  BI.Web.Context, BI.Web.Modules, BI.Web.Logs, BI.Web.Scheduler;

type
  TBIWebCommon=class
  private
    procedure CreateModules;
    procedure SetupPublicFolder;
  public
    type
      TWebPublic=record
      private
        FPath : String;
        RealPath : String;

        procedure SetPath(const Value: String);
      public
        Enabled : Boolean;

        class function PathOf(const ARoot,AFileName: String): String; overload; static;
        function PathOf(const AFileName:String):String; overload;

        property Path : String read FPath write SetPath;
      end;

    var
      DefaultModule : TModule;
      Logs : TWebLogs;
      ModuleInstances : Array of TModule;
      PublicFolder : TWebPublic;
      Scheduler : TImportScheduler;

    class var
      Modules : Array of TModuleClass;

    Constructor Create;
    Destructor Destroy; override;

    class procedure AddModule(const AClass:TModuleClass); static;

    procedure AddUI(const Sender:TObject);

    function ModuleInstance(const AClass:TModuleClass):TModule;

    procedure ProcessFile(const ADocument:String; const AContext: TWebContext); virtual;
    procedure ProcessGet(const AContext: TWebContext);
    procedure ProcessPost(const AContext: TWebContext);

    procedure RefreshUI(const Sender:TObject);

    class function ServerName:String; static;
    class function ServerPort:Integer; static;

    procedure SetupModules;
  end;

  TBIWebConfig=class
  public
    class var
       Key : String;

    class procedure CheckCommandLine; static;
    class function CommandLine(const Code:String):String; static;

    class function ReadBoolean(const AName:String; const Default:Boolean):Boolean; static;
    class function ReadInteger(const AName:String; const Default:Integer):Integer; static;
    class function ReadString(const AName:String; const Default:String=''):String; static;

    class procedure WriteBoolean(const AName:String; const Value:Boolean); static;
    class procedure WriteInteger(const AName:String; const Value:Integer); static;
    class procedure WriteString(const AName,Value:String); static;
  end;

implementation

uses
  {System.}SysUtils, {System.}IOUtils, {System.}Types,

  BI.Web,
  BI.UI, BI.Web.Modules.Default;

{ TBIWebConfig }

class function TBIWebConfig.CommandLine(const Code:String):String;
var t : Integer;
begin
  result:='';

  for t:=1 to ParamCount do
      if SameText(ParamStr(t),'-'+Code) then
         if ParamCount>t then
            Exit(Trim(ParamStr(t+1)))
         else
            break;
end;

class procedure TBIWebConfig.CheckCommandLine;
var tmp : String;
begin
  tmp:=CommandLine('R');

  if tmp<>'' then
     Key:=tmp;
end;

class function TBIWebConfig.ReadBoolean(const AName:String; const Default:Boolean):Boolean;
begin
  result:=TBIRegistry.ReadBoolean(Key,AName,Default);
end;

class function TBIWebConfig.ReadInteger(const AName:String; const Default:Integer):Integer;
begin
  result:=TBIRegistry.ReadInteger(Key,AName,Default);
end;

class function TBIWebConfig.ReadString(const AName,Default:String):String;
begin
  result:=TBIRegistry.ReadString(Key,AName,Default);
end;

class procedure TBIWebConfig.WriteBoolean(const AName:String; const Value:Boolean);
begin
  TBIRegistry.WriteBoolean(Key,AName,Value);
end;

class procedure TBIWebConfig.WriteInteger(const AName:String; const Value:Integer);
begin
  TBIRegistry.WriteInteger(Key,AName,Value);
end;

class procedure TBIWebConfig.WriteString(const AName,Value:String);
begin
  TBIRegistry.WriteString(Key,AName,Value);
end;

{ TBIWebCommon }

Constructor TBIWebCommon.Create;
begin
  inherited Create;

  Logs:=TWebLogs.Create;
  Scheduler:=TImportScheduler.Create;

  SetupPublicFolder;

  CreateModules;
end;

Destructor TBIWebCommon.Destroy;
var tmp : TModule;
begin
  Scheduler.Free;
  Logs.Free;

  for tmp in ModuleInstances do
      tmp.Free;

  DefaultModule.Free;

  inherited;
end;

function TBIWebCommon.ModuleInstance(const AClass: TModuleClass): TModule;
var tmp : TModule;
begin
  for tmp in ModuleInstances do
      if tmp is AClass then
         Exit(tmp);

  result:=nil;
end;

procedure TBIWebCommon.CreateModules;
var tmp : TModuleClass;
    t   : Integer;
begin
  SetLength(ModuleInstances,Length(Modules));

  t:=0;

  for tmp in Modules do
  begin
    ModuleInstances[t]:=tmp.Create(Self);
    Inc(t);
  end;

  DefaultModule:=TDefaultModule.Create(Self);
  TDefaultModule(DefaultModule).Logs:=Logs;
end;

procedure TBIWebCommon.SetupPublicFolder;
begin
  PublicFolder.Enabled:=TBIWebConfig.ReadBoolean('PublicEnabled',True);
  PublicFolder.Path:=TBIWebConfig.ReadString('PublicFolder','public');
end;

class procedure TBIWebCommon.AddModule(const AClass: TModuleClass);
var L : Integer;
begin
  L:=Length(Modules);
  SetLength(Modules,L+1);
  Modules[L]:=AClass;
end;

procedure TBIWebCommon.AddUI(const Sender: TObject);
var tmp : TModule;
begin
  for tmp in ModuleInstances do
      if tmp is TUIModule then
         TUIModule(tmp).Add(Sender);
end;

class function TBIWebCommon.ServerName: String;
begin
  result:=TBIWebConfig.ReadString('Server','http://localhost');
end;

class function TBIWebCommon.ServerPort: Integer;
begin
  result:=TBIWebConfig.ReadInteger('Port',TBIWebClient.DefaultPort);
end;

procedure TBIWebCommon.SetupModules;
var tmp : TModule;
begin
  for tmp in ModuleInstances do
      tmp.Setup;
end;

procedure TBIWebCommon.RefreshUI(const Sender:TObject);
var tmp : TModule;
begin
  for tmp in ModuleInstances do
      if tmp is TUIModule then
         TUIModule(tmp).Refresh(Sender);
end;

procedure TBIWebCommon.ProcessPost(const AContext: TWebContext);
var tmp : TModule;
begin
  for tmp in ModuleInstances do
      if tmp.Post(AContext) then
         Exit;

  DefaultModule.Post(AContext);
end;

procedure TBIWebCommon.ProcessGet(const AContext: TWebContext);
var tmp : TModule;
begin
  for tmp in ModuleInstances do
      if tmp.Get(AContext) then
         Exit;

  DefaultModule.Get(AContext);
end;

procedure TBIWebCommon.ProcessFile(const ADocument:String; const AContext: TWebContext);

  procedure ReturnFile(const AFile:String);
  begin
    AContext.ReturnFile(AFile);
  end;

  procedure Return404;
  begin
    //AResponseInfo.ResponseNo:=404;
    //AResponseInfo.ResponseText:='Document not found';
    //AResponseInfo.WriteHeader;

    raise EBIException.Create('Resource not found: '+ADocument);
  end;

  procedure ReturnDefault;
  var tmp : String;
  begin
    if PublicFolder.Enabled then
    begin
      tmp:=PublicFolder.PathOf(ADocument);

      if TFile.Exists(tmp) then
         ReturnFile(tmp)
      else
         Return404;
    end
    else
      raise EBIException.Create('Cannot serve file: '+ADocument);
  end;

var tmp : TModule;
begin
  for tmp in ModuleInstances do
      if tmp.ProcessFile(ADocument,AContext) then
         Exit;

  ReturnDefault;
end;

{ TBIWebCommon.TWebPublic }

class function TBIWebCommon.TWebPublic.PathOf(const ARoot,AFileName: String): String;
var tmp : String;
begin
  if AFileName='' then
     result:=''
  else
  begin
    tmp:=StringReplace(AFileName,'/','\',[rfReplaceAll]);

    if Copy(tmp,1,1)='\' then
       Delete(tmp,1,1);

    tmp:=TPath.Combine(ARoot,tmp);

    result:=TPath.GetFullPath(tmp);
  end;
end;

function TBIWebCommon.TWebPublic.PathOf(const AFileName: String): String;
begin
  result:=PathOf(RealPath,AFileName);
end;

procedure TBIWebCommon.TWebPublic.SetPath(const Value: String);
var tmpReal,
    tmp : String;
begin
  tmp:=Trim(Value);

  if FPath<>tmp then
  begin
    if TDirectory.IsRelativePath(tmp) then
       tmpReal:=TPath.Combine(ExtractFilePath(ParamStr(0)),tmp)
    else
       tmpReal:=tmp;

    tmpReal:=TPath.GetFullPath(tmpReal);

    FPath:=tmp;
    RealPath:=tmpReal;

    if not TDirectory.Exists(RealPath) then
       ForceDirectories(RealPath);
  end;
end;

initialization
  TBIWebConfig.Key:='BIWeb';

  TBIWebConfig.CheckCommandLine;
end.
