unit BIWebModuleUnit;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp,
     BI.Web, BI.Web.Common, BI.Web.IISContext, BI.Web.AllData, BI.Persist;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    BIWeb : TBIWebCommon;
    Data : TAllData;
    History : TBIWebHistory;

    procedure AddHistory(const AContext:TBIWebContext;
                         const Command:String;
                         const Tag:String;
                         const Success:Boolean;
                         const Millisec:Integer; const Size:Int64);
    procedure Log(const S:String);
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Handled:=TBIIISContext.Process(BIWeb,Request,Response);
end;

procedure TWebModule1.AddHistory(const AContext:TBIWebContext;
                         const Command:String;
                         const Tag:String;
                         const Success:Boolean;
                         const Millisec:Integer; const Size:Int64);
var tmpNow : TDateTime;
    IP: String;
begin
  tmpNow:=Now;

  IP:=TBIIISContext(AContext).PeerIP;

  History.Add(tmpNow,IP,Command,Tag,Success,Millisec,Size);

  if not Success then
     Log(DateTimeToStr(Now)+' '+IP+' '+Command+' '+Tag);
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  TBIRegistry.UseRegistry:=False; // Use inifile
  TBIRegistry.IniFile:='C:\inetpub\wwwroot\isapi\TeeBI.ini';

  BIWeb:=TBIWebCommon.Create;

  History:=TBIWebHistory.Create;
  History.Name:='History';

  Data:=TAllData.Create;

  BIWeb.Logs.History:=History;
  BIWeb.Data:=Data;
  BIWeb.Logs.AddHistory:=AddHistory;

  Log('Started: '+DateTimeToStr(Now));
end;

procedure TWebModule1.Log(const S:String);
begin
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  Data.Free;
  History.Free;

  BIWeb.Free;
end;

end.
