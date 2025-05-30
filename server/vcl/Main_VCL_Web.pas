{*********************************************}
{  TeeBI Software Library                     }
{  Web Server (VCL)                           }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Main_VCL_Web;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Data.DB, Vcl.ExtCtrls, Datasnap.DBClient, Vcl.Grids, Vcl.DBGrids,
  Vcl.Menus,

  BI.Web.AllData, BI.Web, BI.Web.Common, BI.Persist, BI.Web.Context,
  VCLBI.Grid, VCLBI.DataControl, BI.Web.SingleInstance,
  BI.Web.Server.Indy;

type
  TFormBIWeb = class(TForm)
    StatusBar1: TStatusBar;
    TrayIcon1: TTrayIcon;
    PageControl1: TPageControl;
    TabConsole: TTabSheet;
    ErrorLog: TMemo;
    TabSheet2: TTabSheet;
    TabSettings: TTabSheet;
    Panel1: TPanel;
    CBAutoScroll: TCheckBox;
    Panel2: TPanel;
    Label2: TLabel;
    LConnections: TLabel;
    Button1: TButton;
    CBActive: TCheckBox;
    Label3: TLabel;
    EPort: TEdit;
    UDPort: TUpDown;
    HistoryGrid: TBIGrid;
    PopupMenu1: TPopupMenu;
    Show1: TMenuItem;
    Exit1: TMenuItem;
    Timer1: TTimer;
    TabScheduler: TTabSheet;
    Panel3: TPanel;
    CBScheduler: TCheckBox;
    TimerScheduler: TTimer;
    BIGrid1: TBIGrid;
    PageSettings: TPageControl;
    TabVersion: TTabSheet;
    LVersion: TLabel;
    CBAutoUpdate: TCheckBox;
    Button2: TButton;
    TabMemory: TTabSheet;
    LMemory: TLabel;
    Label1: TLabel;
    Button4: TButton;
    TabGeneral: TTabSheet;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    CBLogs: TCheckBox;
    CBLogStore: TComboBox;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    CBPublic: TCheckBox;
    EPublic: TEdit;
    CBStartMin: TCheckBox;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure CBActiveClick(Sender: TObject);
    procedure EPortChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBAutoScrollClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Exit1Click(Sender: TObject);
    procedure Show1Click(Sender: TObject);
    procedure CBStartMinClick(Sender: TObject);
    procedure CBLogsClick(Sender: TObject);
    procedure CBLogStoreChange(Sender: TObject);
    procedure CBPublicClick(Sender: TObject);
    procedure EPublicChange(Sender: TObject);
    procedure CBAutoUpdateClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CBSchedulerClick(Sender: TObject);
    procedure TimerSchedulerTimer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    Data : TAllData;
    History : TBIWebHistory;

    BIWeb : TBIWebCommon;

    CloseFromMenu,
    FirstTime : Boolean;

    procedure AddHistory(const AContext:TWebContext;
                         const Command:String;
                         const Tag:String;
                         const Success:Boolean;
                         const Millisec:Integer; const Size:Int64);

    procedure CheckForUpdates;
    procedure CreateAllData(const AStore:String);
    function FaviconStream:TStream;
    function LocalHost:String;
    procedure Log(const S:String);
    procedure RefreshCount;
    procedure RefreshGrid;
    procedure RefreshMemory;

    procedure ServerConnect(const AContext: TWebContext);
    procedure ServerDisconnect(const AContext: TWebContext);
    procedure ServerException(const AContext: TWebContext; const AException: Exception);
    procedure ServerStatus(const ASender: TObject; const AStatusText: string);
    procedure ServerCommandGet(const AContext: TWebContext);

    procedure SetHistoryWidths;
    procedure SetupLogs;
    procedure SetupPublicFolder;
    procedure TryEnableScheduler;
  public
    { Public declarations }
    Server : THttpServer;
  end;

var
  FormBIWeb: TFormBIWeb;

implementation

{$R *.dfm}

uses
  BI.Arrays, BI.DataItem, BI.UI, Unit_Constants, BI.Html, VCLBI.DataManager,
  BI.Web.Modules.Default,

  BI.CSV, BI.JSON, BI.XMLData, BI.Excel,

  {$IFNDEF FPC}
  {$IF CompilerVersion>26}
  BI.DB.Fire,
  {$ELSE}
  BI.DB.SqlExpr,
  {$ENDIF}
  {$ENDIF}

  System.UITypes, VCLBI.Editor.Stores, BI.Languages.English,
  BI.Web.IndyContext;

procedure TFormBIWeb.Log(const S:String);
begin
  TThread.Synchronize(nil,procedure
  begin
    ErrorLog.Lines.Add(S);
  end);
end;

procedure TFormBIWeb.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabSettings then
     RefreshMemory;
end;

procedure TFormBIWeb.AddHistory(const AContext:TWebContext;
                          const Command, Tag: String; const Success: Boolean;
                          const Millisec:Integer; const Size:Int64);
var tmpNow : TDateTime;
    IP: String;
begin
  tmpNow:=Now;

  IP:=TBIIndyContext(AContext).PeerIP;

  History.Add(tmpNow,IP,Command,Tag,Success,Millisec,Size);

  if not Success then
     Log(DateTimeToStr(Now)+' '+IP+' '+Command+' '+Tag);

  if Visible then
     RefreshGrid;
end;

procedure TFormBIWeb.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TUICommon.SavePosition(Self,TBIWebConfig.Key,'MainForm');
end;

procedure TFormBIWeb.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CloseFromMenu then
  begin
    if Server.Active and (Server.ContextsCount>0) then
       CanClose:=TUICommon.YesNo(Format(BIMsg_ServerSureToClose,[Server.ContextsCount]))
    else
       CanClose:=True;
  end
  else
  begin
    CanClose:=False;
    Hide;
  end;
end;

function VCLDefaultStore:String;
begin
  result:=TStore.DefaultName;

  if result='' then
  begin
    TStoreEditor.Edit(nil);

    result:=TStore.DefaultName;

    if result='' then
    begin
      ShowMessage(BIMsg_Web_NoDefaultStore);
      Halt;
    end;
  end;
end;

procedure TFormBIWeb.SetHistoryWidths;
var tmp : TObject;
    tmpG : TDBGrid;
begin
  tmp:=HistoryGrid.Plugin.GetObject;

  if tmp is TDBGrid then
  begin
    tmpG:=TDBGrid(tmp);

    // Set default widths for Text columns, because they are initially empty,
    // which defaults to a text size of 255 chars (too wide)
    tmpG.Columns[1].Width:=100;
    tmpG.Columns[2].Width:=100;
    tmpG.Columns[3].Width:=160;
  end;
end;

procedure TFormBIWeb.FormCreate(Sender: TObject);

  procedure CreateServer;
  begin
    Server:=THttpServer.Engine.Create(Self);

    Server.Port:=TBIWebConfig.ReadInteger('Port',TBIWebClient.DefaultPort);

    Server.OnCommandGet:=ServerCommandGet;
    Server.OnConnect:=ServerConnect;
    Server.OnDisconnect:=ServerDisconnect;
    Server.OnException:=ServerException;
    Server.OnStatus:=ServerStatus;
  end;

  procedure CreateHistory;
  begin
    History:=TBIWebHistory.Create;
    History.Name:='History';

    HistoryGrid.BindTo(History);

    SetHistoryWidths;
  end;

  function DefaultDataStore:String;
  begin
    result:=TBIWebConfig.CommandLine('S');

    if result='' then
       result:=VCLDefaultStore;
  end;

  procedure CreateBIWeb;
  begin
    BIWeb:=TBIWebCommon.Create;
    TDefaultModule(BIWeb.DefaultModule).Data:=Data;

    BIWeb.Logs.History:=History;
    BIWeb.Logs.AddHistory:=AddHistory;

    BIWeb.Scheduler.Refresh(Data.Store);
  end;

begin
  TUICommon.LoadPosition(Self,TBIWebConfig.Key,'MainForm');

  FirstTime:=True;

  LVersion.Caption:='Version: '+TBIWebServer.Version.ToString;

  PageControl1.ActivePage:=TabConsole;

  UDPort.Position:=TBIWebClient.DefaultPort;

  CreateServer;

  StatusBar1.Panels.Add;

  CreateHistory;
  CreateAllData(DefaultDataStore);

  CreateBIWeb;

  BIGrid1.Data:=BIWeb.Scheduler.Data;

  Server.Active:=True;

  CBActive.Checked:=Server.Active;

  CBAutoUpdate.Checked:=TBIWebConfig.ReadBoolean('AutoUpdate', {$IFDEF MSWINDOWS}True{$ELSE}False{$ENDIF});
  CBStartMin.Checked:=TBIWebConfig.ReadBoolean('Minimized', {$IFDEF MSWINDOWS}True{$ELSE}False{$ENDIF});

  SetupLogs;
  SetupPublicFolder;

  Log('Started: '+DateTimeToStr(Now));

  Timer1.Enabled:=CBAutoUpdate.Checked;

  CBScheduler.Checked:=TBIRegistry.ReadBoolean('BIWeb','Scheduler',False);
  TryEnableScheduler;

  if FirstTime then
  begin
    FirstTime:=False;

    if CBStartMin.Checked then
       WindowState:=TWindowState.wsMinimized;
  end;
end;

procedure TFormBIWeb.SetupPublicFolder;
begin
  CBPublic.Checked:=BIWeb.PublicFolder.Enabled;
end;

procedure TFormBIWeb.SetupLogs;
begin
  BIWeb.Logs.Persist:=TBIWebConfig.ReadBoolean('LogPersist',True);
  BIWeb.Logs.Store:=TBIWebConfig.ReadString('LogStore');

  CBLogs.Checked:=BIWeb.Logs.Persist;

  CBLogStore.Clear;
  TStores.AllTo(CBLogStore.Items);

  if BIWeb.Logs.Store<>'' then
     CBLogStore.ItemIndex:=CBLogStore.Items.IndexOf(BIWeb.Logs.Store);
end;

procedure TFormBIWeb.FormDestroy(Sender: TObject);
begin
  Data.Free;
  History.Free;

  BIWeb.Free;
end;

procedure TFormBIWeb.CreateAllData(const AStore:String);
var S : String;
begin
  repeat
    try
      S:=TStore.PathOf(AStore);
      break;
    except
      on E:Exception do
         if TUICommon.YesNo('Error accessing store: '+AStore+'. Configure stores?') then
            TStoreEditor.Edit(Self)
         else
            break;
    end;
  until False;

  // Protection against self-recursivity:
  if SameText(S,'WEB:LOCALHOST') or SameText(S,'WEB:LOCALHOST:'+IntToStr(Server.Port)) then
     raise EBIException.Create('Error: Store path cannot be same server localhost');

  Data.Free;
  Data:=TAllData.Create(AStore);
end;

procedure TFormBIWeb.Button1Click(Sender: TObject);
var tmp : String;
begin
  if Data<>nil then
  begin
    tmp:=Data.Store;

    TDataManager.Edit(Self,tmp);

    if tmp<>TStore.DefaultName then
       CreateAllData(TStore.DefaultName);

    SetupLogs;
  end;
end;

procedure TFormBIWeb.Button2Click(Sender: TObject);
begin
  CheckForUpdates;
end;

function TFormBIWeb.LocalHost:String;
begin
  result:='http://localhost:'+IntToStr(Server.Port);
end;

procedure TFormBIWeb.Button3Click(Sender: TObject);
begin
  TUICommon.GotoURL(Self,LocalHost);
end;

procedure TFormBIWeb.Button4Click(Sender: TObject);
begin
//  TStoreStatus.View(Self);
end;

procedure TFormBIWeb.CBActiveClick(Sender: TObject);
begin
  Server.Active:=CBActive.Checked;
  EPort.Enabled:=not Server.Active;
end;

procedure TFormBIWeb.CBAutoScrollClick(Sender: TObject);
begin
  if Visible and CBAutoScroll.Checked then
     HistoryGrid.DataSource.DataSet.Last;
end;

procedure TFormBIWeb.CBAutoUpdateClick(Sender: TObject);
begin
  TBIRegistry.WriteBoolean('BIWeb','AutoUpdate',CBAutoUpdate.Checked);
end;

procedure TFormBIWeb.CBLogsClick(Sender: TObject);
begin
  BIWeb.Logs.Persist:=CBLogs.Checked;
  TBIRegistry.WriteBoolean('BIWeb','LogPersist',CBLogs.Checked);

  if CBLogs.Checked then
     BIWeb.Logs.TrySave;
end;

procedure TFormBIWeb.CBLogStoreChange(Sender: TObject);
begin
  if CBLogStore.ItemIndex=-1 then
     BIWeb.Logs.Store:=''
  else
     BIWeb.Logs.Store:=CBLogStore.Items[CBLogStore.ItemIndex];

  TBIRegistry.WriteString('BIWeb','LogStore',BIWeb.Logs.Store);

  if CBLogs.Checked then
     BIWeb.Logs.TrySave;
end;

procedure TFormBIWeb.CBPublicClick(Sender: TObject);
begin
  BIWeb.PublicFolder.Enabled:=CBPublic.Checked;
  TBIRegistry.WriteBoolean('BIWeb','PublicEnabled',BIWeb.PublicFolder.Enabled);
end;

procedure TFormBIWeb.TryEnableScheduler;
begin
  BIWeb.Scheduler.Enabled:=CBScheduler.Checked;
  TimerScheduler.Enabled:=BIWeb.Scheduler.Enabled;
end;

procedure TFormBIWeb.CBSchedulerClick(Sender: TObject);
begin
  TBIRegistry.WriteBoolean('BIWeb','Scheduler',CBScheduler.Checked);
  TryEnableScheduler;
end;

procedure TFormBIWeb.CBStartMinClick(Sender: TObject);
begin
  TBIRegistry.WriteBoolean('BIWeb','Minimized',CBStartMin.Checked);
end;

procedure TFormBIWeb.EPortChange(Sender: TObject);
begin
  Server.Port:=UDPort.Position;

  TBIRegistry.WriteInteger('BIWeb','Port',Server.Port);
end;

procedure TFormBIWeb.EPublicChange(Sender: TObject);
begin
  BIWeb.PublicFolder.Path:=EPublic.Text;
  TBIRegistry.WriteString('BIWeb','PublicFolder',BIWeb.PublicFolder.Path);
end;

procedure TFormBIWeb.Exit1Click(Sender: TObject);
begin
  CloseFromMenu:=True;
  Close;
end;

function TFormBIWeb.FaviconStream:TStream;
begin
  result:=TMemoryStream.Create;
//  Favicon.Bitmap.SaveToStream(result);
end;

procedure TFormBIWeb.ServerCommandGet(const AContext: TWebContext);
begin
  if SameText(AContext.GetDocument,'/favicon.ico') then
     AContext.ReturnIcon(FaviconStream)
  else
  try
    try
      TBIIndyContext.Process(BIWeb,AContext);
    except
      on E:Exception do
      begin
        Log(E.Message);
        AContext.SetResponse(E.Message);
      end;
    end;
  finally
    if PageControl1.ActivePage=TabSettings then
       RefreshMemory;
  end;
end;

procedure TFormBIWeb.RefreshCount;
begin
  if Server<>nil then
     TThread.Synchronize(nil,procedure
     begin
       LConnections.Caption:=IntToStr(Server.ContextsCount);
     end);
end;

procedure TFormBIWeb.RefreshGrid;
begin
  TThread.Synchronize(nil,procedure
  begin
    //HistoryGrid.DataSet.Refresh;
    CBAutoScrollClick(Self);
  end);
end;

procedure TFormBIWeb.RefreshMemory;
begin
  LMemory.Caption:=TCommonUI.BytesToString(TMemory.Allocated);
end;

procedure TFormBIWeb.ServerConnect(const AContext: TWebContext);
begin
  RefreshCount;
end;

procedure TFormBIWeb.ServerDisconnect(const AContext: TWebContext);
begin
  RefreshCount;
end;

procedure TFormBIWeb.ServerException(const AContext: TWebContext;
  const AException: Exception);
begin
  Log(AException.Message);
end;

procedure TFormBIWeb.ServerStatus(const ASender: TObject; const AStatusText: string);
begin
  StatusBar1.Panels[0].Text:=AStatusText;
end;

procedure TFormBIWeb.Show1Click(Sender: TObject);
begin
  FormBIWeb.Show;
  RefreshGrid;
end;

procedure TFormBIWeb.CheckForUpdates;
var tmp : String;
begin
  if not TSteema.CheckNewVersion(tmp) then
     Log(DateTimeToStr(Now)+' '+Format(BIMsg_Web_ErrorCheckingUpdates,[tmp]));
end;

procedure TFormBIWeb.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=False;
  CheckForUpdates;
end;

procedure TFormBIWeb.TimerSchedulerTimer(Sender: TObject);
begin
  BIWeb.Scheduler.Process;
end;

procedure TFormBIWeb.TrayIcon1Click(Sender: TObject);
begin
  Show;
end;

end.
