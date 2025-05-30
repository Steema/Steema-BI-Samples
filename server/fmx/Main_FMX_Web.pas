{*********************************************}
{  TeeBI Software Library                     }
{  Web Server (Firemonkey)                    }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Main_FMX_Web;

interface

uses
  {$IFDEF MSWINDOWS}
  WinAPI.Windows,
  FMX.Platform.Win,
  nFMX.Trayicon.Win,

  BI.Excel,
  {$ENDIF}

  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.SyncObjs,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  BI.Web.AllData,

  {$IF FireMonkeyVersion<240}
  {$I FiremonkeyChecks.inc}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation, FMX.EditBox, FMX.NumberBox,
  {$ENDIF}

  {$IFNDEF HASFMX21}
  FMX.ScrollBox,
  {$ENDIF}

  {$IFNDEF HASFMX23}
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  {$ENDIF}

  FMX.StdCtrls, FMX.Layouts, FMX.Memo, FMX.Objects, FMX.ListBox,
  FMXBI.DataManager, FMX.Edit, FMX.TabControl,
  FMX.Menus, BI.Web, FMX.ListView.Types, FMX.ListView,

  BI.Web.Common, BI.Web.SingleInstance, BI.Web.Context,

  FMXBI.Grid, FMXBI.DataControl, BI.Web.Server.Indy,

  FireDAC.UI.Intf,
  FireDAC.FMXUI.Wait, FireDAC.Stan.Intf, FireDAC.Comp.UI, FMX.Memo.Types;

type
  TBIWebMain = class(TForm)
    Layout1: TLayout;
    Label1: TLabel;
    LConnections: TLabel;
    LStatus: TLabel;
    Button1: TButton;
    Label2: TLabel;
    NumberBox1: TNumberBox;
    CBActive: TCheckBox;
    TabControl1: TTabControl;
    TabConsole: TTabItem;
    TabHistory: TTabItem;
    TabSettings: TTabItem;
    ErrorLog: TMemo;
    Layout2: TLayout;
    CBAutoScroll: TCheckBox;
    PopupMenu1: TPopupMenu;
    MenuExit: TMenuItem;
    Timer1: TTimer;
    MenuShow: TMenuItem;
    Favicon: TImage;
    TabSchedule: TTabItem;
    Favicon16: TImage;
    Layout3: TLayout;
    CBScheduler: TCheckBox;
    BIGrid1: TBIGrid;
    TimerScheduler: TTimer;
    Settings: TTabControl;
    TabVersion: TTabItem;
    CBAutoUpdate: TCheckBox;
    Button2: TButton;
    LVersion: TLabel;
    TabMemory: TTabItem;
    BStatus: TButton;
    Label3: TLabel;
    LMemory: TLabel;
    TabGeneral: TTabItem;
    GroupBox1: TGroupBox;
    CBLogs: TCheckBox;
    Label4: TLabel;
    CBLogStore: TComboBox;
    CBStartMin: TCheckBox;
    GroupBox2: TGroupBox;
    CBPublic: TCheckBox;
    Label5: TLabel;
    EPublic: TEdit;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    CBRunStart: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBActiveChange(Sender: TObject);
    procedure NumberBox1ChangeTracking(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure nTrayIcon1Click(Sender: TObject);
    procedure CBAutoUpdateChange(Sender: TObject);
    procedure CBStartMinChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CBAutoScrollChange(Sender: TObject);
    procedure MenuShowClick(Sender: TObject);
    procedure FaviconClick(Sender: TObject);
    procedure BStatusClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TabControl1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBLogsChange(Sender: TObject);
    procedure CBLogStoreChange(Sender: TObject);
    procedure CBPublicChange(Sender: TObject);
    procedure EPublicChange(Sender: TObject);
    procedure CBSchedulerChange(Sender: TObject);
    procedure TimerSchedulerTimer(Sender: TObject);
    procedure CBRunStartChange(Sender: TObject);
  private
    { Private declarations }
    Data : TAllData;
    History : TBIWebHistory;

    HistoryGrid : TBIGrid;

    CloseFromMenu,
    FirstTime : Boolean;

    {$IFDEF MSWINDOWS}
    TrayIcon : TnTrayIcon;
    {$ENDIF}

    procedure AddHistory(const AContext:TWebContext;
                         const Command:String;
                         const Tag:String;
                         const Success:Boolean;
                         const Millisec:Integer;
                         const Size:Int64);

    procedure CheckForUpdates;
    procedure CreateAllData(const AStore:String);

    function FaviconStream : TStream;

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

    procedure SetupLogs;
    procedure SetupPublicFolder;
    procedure TryEnableScheduler;
  protected
    BIWeb : TBIWebCommon;
  public
    { Public declarations }

    Server : THttpServer;
  end;

var
  BIWebMain: TBIWebMain;

implementation

{$R *.fmx}

uses
  {$IFDEF MSWINDOWS}
  FMX.Platform,
  {$ENDIF}

  BI.Arrays, BI.DataItem, BI.Persist, System.IOUtils, BI.UI,
  BI.CSV, BI.Html, BI.JSON, BI.ClientDataset,
  BI.XMLData, BI.DB,

  {$IFNDEF FPC}
  {$IF CompilerVersion>26}
  BI.DB.Fire,
  {$ELSE}
  BI.DB.SqlExpr,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  System.Win.Registry,
  {$ENDIF}

  FMXBI.Status,
  BI.Languages.English, BI.Languages.Spanish, BI.Web.Modules.Default,
  FMXTee.Procs, FMXBI.Editor.Stores, BI.Web.IndyContext;

procedure AppOnTaskbar(const AMainForm : TForm; const Hide:Boolean);
{$IFDEF MSWINDOWS}
var AppHandle : HWND;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}

  {$IF CompilerVersion>25}
  AppHandle:={$IF CompilerVersion>27}ApplicationHWND{$ELSE}WindowHandleToPlatform(AMainForm.Handle).Wnd{$ENDIF};

  if AppHandle<>0 then
  begin
    if Hide then
    begin
      ShowWindow(AppHandle, SW_HIDE);
      SetWindowLong(AppHandle, GWL_EXSTYLE, GetWindowLong(AppHandle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
    end
    else
    begin
      ShowWindow(AppHandle, SW_SHOWNORMAL);
      SetWindowLong(AppHandle, GWL_EXSTYLE, GetWindowLong(AppHandle, GWL_EXSTYLE));
    end;
  end;
  {$ENDIF}

  {$ENDIF}
end;

procedure TBIWebMain.CreateAllData(const AStore:String);
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
     raise EBIException.Create('Error: Store cannot be this same "localhost" server');

  Data.Free;
  Data:=TAllData.Create(AStore);
end;

procedure TBIWebMain.EPublicChange(Sender: TObject);
begin
  BIWeb.PublicFolder.Path:=EPublic.Text;
  TBIWebConfig.WriteString('PublicFolder',BIWeb.PublicFolder.Path);
end;

procedure TBIWebMain.Button1Click(Sender: TObject);
var tmp : String;
begin
  tmp:=Data.Store;

  TDataManager.Edit(Self,tmp);

  if tmp<>TStore.DefaultName then
     CreateAllData(TStore.DefaultName);

  SetupLogs;
end;

procedure TBIWebMain.Button2Click(Sender: TObject);
begin
  CheckForUpdates;
end;

procedure TBIWebMain.BStatusClick(Sender: TObject);
begin
  TStoreStatus.View(Self);
end;

procedure TBIWebMain.CBActiveChange(Sender: TObject);
begin
  Server.Active:=CBActive.IsChecked;
  NumberBox1.Enabled:=not Server.Active;
end;

procedure TBIWebMain.CBAutoScrollChange(Sender: TObject);
begin
  if CBAutoScroll.IsChecked then
     TThread.Synchronize(nil,procedure
     begin
       HistoryGrid.DataSet.Refresh;
       HistoryGrid.DataSet.Last;
     end);
end;

procedure TBIWebMain.CBAutoUpdateChange(Sender: TObject);
begin
  TBIWebConfig.WriteBoolean('AutoUpdate',CBAutoUpdate.IsChecked);
end;

procedure TBIWebMain.CBStartMinChange(Sender: TObject);
begin
  TBIWebConfig.WriteBoolean('Minimized',CBStartMin.IsChecked);
end;

procedure TBIWebMain.CBLogsChange(Sender: TObject);
begin
  BIWeb.Logs.Persist:=CBLogs.IsChecked;
  TBIWebConfig.WriteBoolean('LogPersist',CBLogs.IsChecked);

  if CBLogs.IsChecked then
     BIWeb.Logs.TrySave;
end;

procedure TBIWebMain.CBLogStoreChange(Sender: TObject);
begin
  if CBLogStore.Selected=nil then
     BIWeb.Logs.Store:=''
  else
     BIWeb.Logs.Store:=CBLogStore.Selected.Text;

  TBIWebConfig.WriteString('LogStore',BIWeb.Logs.Store);

  if CBLogs.IsChecked then
     BIWeb.Logs.TrySave;
end;

procedure TBIWebMain.CBPublicChange(Sender: TObject);
begin
  BIWeb.PublicFolder.Enabled:=CBPublic.IsChecked;
  TBIWebConfig.WriteBoolean('PublicEnabled',BIWeb.PublicFolder.Enabled);
end;

const
  RunSystemKey='Software\Microsoft\Windows\CurrentVersion\Run';

function ExeUpper:String;
begin
  result:=UpperCase(Trim(ParamStr(0)));
end;

function CurrentRunSystem:Boolean;
begin
  {$IFDEF MSWINDOWS}
  with TRegistry.Create(KEY_READ) do
  try
    if OpenKeyReadOnly(RunSystemKey) then
       result:=ValueExists(ExeUpper)
    else
       result:=False
  finally
    Free;
  end
  {$ELSE}
  result:=False;
  {$ENDIF}
end;

function FullCommandLine:String;
var t : Integer;
begin
  result:='"'+ParamStr(0)+'"';

  for t:=1 to ParamCount do
      result:=result+' '+ParamStr(t);
end;

procedure ChangeRunSystem(const RunStart:Boolean);
begin
  if CurrentRunSystem<>RunStart then

  {$IFDEF MSWINDOWS}
  with TRegistry.Create do
  try
    if OpenKey(RunSystemKey,True) then
       WriteString(ExeUpper,FullCommandLine);
  finally
    Free;
  end
  {$ENDIF}
end;

procedure TBIWebMain.CBRunStartChange(Sender: TObject);
begin
  TBIWebConfig.WriteBoolean('RunSystem',CBRunStart.IsChecked);
  ChangeRunSystem(CBRunStart.IsChecked);
end;

procedure TBIWebMain.TryEnableScheduler;
begin
  BIWeb.Scheduler.Enabled:=CBScheduler.IsChecked;
  TimerScheduler.Enabled:=BIWeb.Scheduler.Enabled;
end;

procedure TBIWebMain.CBSchedulerChange(Sender: TObject);
begin
  TBIWebConfig.WriteBoolean('Scheduler',CBScheduler.IsChecked);
  TryEnableScheduler;
end;

procedure TBIWebMain.CheckForUpdates;
var tmp : String;
begin
  if not TSteema.CheckNewVersion(tmp) then
     Log(DateTimeToStr(Now)+' '+Format(BIMsg_Web_ErrorCheckingUpdates,[tmp]));
end;

procedure TBIWebMain.FormActivate(Sender: TObject);
begin
  if FirstTime then
  begin
    FirstTime:=False;

    if CBStartMin.IsChecked then
       Hide;
  end;
end;

procedure TBIWebMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TUICommon.SavePosition(Self,TBIWebConfig.Key,'MainForm');
end;

procedure TBIWebMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
    AppOnTaskbar(Self,True);
  end;
end;

function FMXDefaultStore:String;
begin
  result:=TStore.DefaultName;

  if result='' then
  begin
    TStoreEditor.Edit(nil);

    result:=TStore.DefaultName;

    if result='' then
    begin
      TCommonUI.ShowMessage(BIMsg_Web_NoDefaultStore);
      Halt;
    end;
  end;
end;

function TBIWebMain.FaviconStream: TStream;
begin
  result:=TMemoryStream.Create;
  Favicon16.Bitmap.SaveToStream(result);
end;

procedure TBIWebMain.FormCreate(Sender: TObject);

  procedure CreateServer;
  begin
    Server:=THttpServer.Engine.Create(Self);

    Server.Port:=TBIWebCommon.ServerPort;

    Server.OnCommandGet:=ServerCommandGet;
    Server.OnConnect:=ServerConnect;
    Server.OnDisconnect:=ServerDisconnect;
    Server.OnException:=ServerException;
    Server.OnStatus:=ServerStatus;
  end;

  procedure CreateBIWeb;
  begin
    BIWeb:=TBIWebCommon.Create;
    TDefaultModule(BIWeb.DefaultModule).Data:=Data;

    BIWeb.Logs.History:=History;
    BIWeb.Logs.AddHistory:=AddHistory;

    BIWeb.Scheduler.Refresh(Data.Store);

    BIWeb.Logs.Persist:=TBIWebConfig.ReadBoolean('LogPersist',True);
    BIWeb.Logs.Store:=TBIWebConfig.ReadString('LogStore');
  end;

  procedure CreateHistory;
  begin
    History:=TBIWebHistory.Create;
    History.Name:='History';

    HistoryGrid:=TBIGrid.Embedd(Self,TabHistory);
    HistoryGrid.BindTo(History);
  end;

  function DefaultDataStore:String;
  begin
    result:=TBIWebConfig.CommandLine('S');

    if result='' then
       result:=FMXDefaultStore;
  end;

begin
  TUICommon.LoadPosition(Self,TBIWebConfig.Key,'MainForm');

  FirstTime:=True;

  LVersion.Text:='Version: '+TBIWebServer.Version.ToString;

  TabControl1.ActiveTab:=TabConsole;

  NumberBox1.Value:=TBIWebClient.DefaultPort;

  CreateServer;
  CreateHistory;

  CreateAllData(DefaultDataStore);
  CreateBIWeb;

  BIWeb.AddUI(Settings);

  BIWeb.SetupModules;

  Server.Active:=True;

  Log('Started: '+DateTimeToStr(Now));
end;

procedure TBIWebMain.FormDestroy(Sender: TObject);
begin
  Data.Free;
  History.Free;
  BIWeb.Free;
end;

procedure TBIWebMain.FormShow(Sender: TObject);

  {$IFDEF MSWINDOWS}
  procedure CreateTrayIcon;
  begin
    TrayIcon:=TnTrayIcon.Create(Self);
    TrayIcon.Hint:='Steema BIWeb';
    TrayIcon.BalloonText:='BIWeb Server';
    TrayIcon.BalloonTitle:='Steema';
    TrayIcon.IconBalloonType:=Info;
    TrayIcon.Indent:=150;
    TrayIcon.PopUpMenu:=PopupMenu1;
    TrayIcon.OnClick:=nTrayIcon1Click;

    TrayIcon.Show;
  end;
  {$ENDIF}

begin
  BIGrid1.Data:=BIWeb.Scheduler.Data;

  CBActive.IsChecked:=Server.Active;

  {$IFDEF MSWINDOWS}
  CreateTrayIcon;
  {$ELSE}
  CloseFromMenu:=True;
  {$ENDIF}

  AppOnTaskbar(Self,True);

  CBAutoUpdate.IsChecked:=TBIWebConfig.ReadBoolean('AutoUpdate', {$IFDEF MSWINDOWS}True{$ELSE}False{$ENDIF});
  CBStartMin.IsChecked:=TBIWebConfig.ReadBoolean('Minimized', {$IFDEF MSWINDOWS}True{$ELSE}False{$ENDIF});

  CBRunStart.IsChecked:=TBIWebConfig.ReadBoolean('RunSystem',True);
  ChangeRunSystem(CBRunStart.IsChecked);

  SetupLogs;
  SetupPublicFolder;

  Timer1.Enabled:=CBAutoUpdate.IsChecked;

  CBScheduler.IsChecked:=TBIWebConfig.ReadBoolean('Scheduler',False);
  TryEnableScheduler;
end;

procedure TBIWebMain.SetupLogs;
begin
  CBLogs.IsChecked:=BIWeb.Logs.Persist;

  CBLogStore.Clear;
  TStores.AllTo(CBLogStore.Items);

  if BIWeb.Logs.Store<>'' then
     CBLogStore.ItemIndex:=CBLogStore.Items.IndexOf(BIWeb.Logs.Store);
end;

procedure TBIWebMain.SetupPublicFolder;
begin
  CBPublic.IsChecked:=BIWeb.PublicFolder.Enabled;
end;

procedure TBIWebMain.MenuExitClick(Sender: TObject);
begin
  CloseFromMenu:=True;
  Close;
end;

{$IF CompilerVersion<26}
function FormToHWND(Form: TCommonCustomForm): HWND;
begin
  if (Form <> nil) and (Form.Handle is TWinWindowHandle) then
    Result := TWinWindowHandle(Form.Handle).Wnd
  else
    Result := 0;
end;
{$ENDIF}

procedure TBIWebMain.MenuShowClick(Sender: TObject);
begin
  AppOnTaskbar(Self,False);

  if not (TFmxFormState.Showing in FormState) then
     Show;

  {$IFDEF MSWINDOWS}
  SetForegroundWindow(FormToHWND(Self));
  {$ENDIF}

  RefreshGrid;
end;

procedure TBIWebMain.RefreshGrid;
begin
  CBAutoScrollChange(Self);
end;

procedure TBIWebMain.nTrayIcon1Click(Sender: TObject);
begin
  MenuShowClick(Self);
end;

procedure TBIWebMain.NumberBox1ChangeTracking(Sender: TObject);
begin
  Server.Port:=Round(NumberBox1.Value);

  TBIWebConfig.WriteInteger('Port',Server.Port);
end;

procedure TBIWebMain.RefreshCount;
begin
  if Server<>nil then
     TThread.Synchronize(nil,procedure
     begin
       LConnections.Text:=IntToStr(Server.ContextsCount);
     end);
end;

procedure TBIWebMain.RefreshMemory;
begin
  TThread.Synchronize(nil,procedure
  begin
    LMemory.Text:=TCommonUI.BytesToString(TMemory.Allocated);
  end);
end;

procedure TBIWebMain.Log(const S:String);
begin
  if ErrorLog<>nil then
     TThread.Synchronize(nil,procedure
     begin
       ErrorLog.Lines.Add(S);
     end);
end;

procedure TBIWebMain.AddHistory(const AContext:TWebContext;
                                const Command:String;
                                const Tag:String;
                                const Success:Boolean;
                                const Millisec:Integer;
                                const Size:Int64);
const
  MaxItems=1000;

var tmpNow : TDateTime;
    IP: String;
begin
  tmpNow:=Now;

  IP:=TBIIndyContext(AContext).PeerIP;

  History.Add(tmpNow,IP,Command,Tag,Success,Millisec,Size);

  if not Success then
     Log(DateTimeToStr(Now)+' '+IP+' '+Command+' '+Tag);

  if Visible and (TabControl1.ActiveTab=TabHistory) then
     RefreshGrid;
end;

function TBIWebMain.LocalHost:String;
begin
  result:='http://localhost:'+IntToStr(Server.Port);
end;

procedure TBIWebMain.FaviconClick(Sender: TObject);
begin
  TUICommon.GotoURL(nil,LocalHost);
end;

procedure TBIWebMain.ServerCommandGet(const AContext: TWebContext);
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
    if TabControl1.ActiveTab=TabSettings then
       RefreshMemory;
  end;
end;

procedure TBIWebMain.ServerConnect(const AContext: TWebContext);
begin
  if Visible then
     RefreshCount;
end;

procedure TBIWebMain.ServerDisconnect(const AContext: TWebContext);
begin
  if Visible then
     RefreshCount;
end;

procedure TBIWebMain.ServerException(const AContext: TWebContext; const AException: Exception);
begin
  Log(AException.Message);
end;

procedure TBIWebMain.ServerStatus(const ASender: TObject; const AStatusText: string);
begin
  LStatus.Text:=AStatusText;
end;

procedure TBIWebMain.TabControl1Change(Sender: TObject);
begin
  if TabControl1.ActiveTab=TabSettings then
     RefreshMemory;

  if BIWeb<>nil then
     BIWeb.RefreshUI(TabControl1.ActiveTab);
end;

procedure TBIWebMain.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=False;
  CheckForUpdates;
end;

procedure TBIWebMain.TimerSchedulerTimer(Sender: TObject);
begin
  BIWeb.Scheduler.Process;
end;

end.
