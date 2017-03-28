{*********************************************}
{  TeeBI Software Library                     }
{  Web Server (Firemonkey)                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Main_FMX_Web;

interface

uses
  {$IFDEF MSWINDOWS}
  WinAPI.Windows,
  FMX.Platform.Win,
  nFMX.Trayicon.Win,

  BI.Data.Excel,
  {$ENDIF}

  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.SyncObjs,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdCustomHTTPServer,
  IdHTTPServer, BI.Web.AllData, IdContext,

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
  BI.FMX.DataManager, FMX.Edit, FMX.TabControl,
  FMX.Menus, BI.Web, FMX.ListView.Types, FMX.ListView,
  BI.Web.Common, BI.Web.SingleInstance,
  BI.FMX.Grid, BI.FMX.DataControl;

type
  TBIWebMain = class(TForm)
    Server: TIdHTTPServer;
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
    MemoLog: TMemo;
    Button2: TButton;
    CBAutoUpdate: TCheckBox;
    Layout2: TLayout;
    CBAutoScroll: TCheckBox;
    LVersion: TLabel;
    PopupMenu1: TPopupMenu;
    MenuExit: TMenuItem;
    Timer1: TTimer;
    CBStartMin: TCheckBox;
    MenuShow: TMenuItem;
    Favicon: TImage;
    BStatus: TButton;
    TabSchedule: TTabItem;
    Label3: TLabel;
    LMemory: TLabel;
    Favicon16: TImage;
    GroupBox1: TGroupBox;
    CBLogs: TCheckBox;
    Label4: TLabel;
    CBLogStore: TComboBox;
    GroupBox2: TGroupBox;
    CBPublic: TCheckBox;
    Label5: TLabel;
    EPublic: TEdit;
    Layout3: TLayout;
    CBScheduler: TCheckBox;
    BIGrid1: TBIGrid;
    TimerScheduler: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ServerConnect(AContext: TIdContext);
    procedure ServerDisconnect(AContext: TIdContext);
    procedure ServerStatus(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure ServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
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
  private
    { Private declarations }
    Data : TAllData;
    History : TBIWebHistory;

    HistoryGrid : TBIGrid;

    CloseFromMenu,
    FirstTime : Boolean;

    BIWeb : TBIWebCommon;

    {$IFDEF MSWINDOWS}
    TrayIcon : TnTrayIcon;
    {$ENDIF}

    procedure AddHistory(const AContext:TBIWebContext;
                         const Command:String;
                         const Tag:String;
                         const Success:Boolean;
                         const Millisec:Integer;
                         const Size:Int64);

    procedure CheckForUpdates;
    procedure CreateAllData(const AStore:String);

    function FaviconStream:TStream;
    procedure Log(const S:String);
    procedure RefreshCount;
    procedure RefreshGrid;
    procedure RefreshMemory;
    procedure SetupLogs;
    procedure SetupPublicFolder;
    procedure TryEnableScheduler;
  public
    { Public declarations }
  end;

var
  BIWebMain: TBIWebMain;

implementation

{$R *.fmx}

uses
  {$IFDEF MSWINDOWS}
  FMX.Platform,
  {$ENDIF}
  BI.Arrays, BI.Data, BI.Persist, System.IOUtils, BI.UI,
  BI.Data.CSV, BI.Data.Html, BI.Data.JSON, BI.Data.ClientDataset,
  BI.Data.XML, BI.Data.DB,

  {$IFNDEF FPC}
  {$IF CompilerVersion>26}
  BI.Data.DB.FireDAC,
  {$ELSE}
  BI.Data.DB.SqlExpr,
  {$ENDIF}
  {$ENDIF}

  BI.FMX.Status,
  BI.Languages.English, BI.Languages.Spanish,
  FMXTee.Procs, BI.FMX.Editor.Stores, BI.Web.IndyContext;

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
  if SameText(S,'WEB:LOCALHOST') or SameText(S,'WEB:LOCALHOST:'+IntToStr(Server.DefaultPort)) then
     raise EBIException.Create('Error: Store cannot be this same "localhost" server');

  Data.Free;
  Data:=TAllData.Create(AStore);
end;

procedure TBIWebMain.EPublicChange(Sender: TObject);
begin
  BIWeb.PublicFolder.Path:=EPublic.Text;
  TBIRegistry.WriteString('BIWeb','PublicFolder',BIWeb.PublicFolder.Path);
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
  TBIRegistry.WriteBoolean('BIWeb','AutoUpdate',CBAutoUpdate.IsChecked);
end;

procedure TBIWebMain.CBStartMinChange(Sender: TObject);
begin
  TBIRegistry.WriteBoolean('BIWeb','Minimized',CBStartMin.IsChecked);
end;

procedure TBIWebMain.CBLogsChange(Sender: TObject);
begin
  BIWeb.Logs.Persist:=CBLogs.IsChecked;
  TBIRegistry.WriteBoolean('BIWeb','LogPersist',CBLogs.IsChecked);

  if CBLogs.IsChecked then
     BIWeb.Logs.TrySave;
end;

procedure TBIWebMain.CBLogStoreChange(Sender: TObject);
begin
  if CBLogStore.Selected=nil then
     BIWeb.Logs.Store:=''
  else
     BIWeb.Logs.Store:=CBLogStore.Selected.Text;

  TBIRegistry.WriteString('BIWeb','LogStore',BIWeb.Logs.Store);

  if CBLogs.IsChecked then
     BIWeb.Logs.TrySave;
end;

procedure TBIWebMain.CBPublicChange(Sender: TObject);
begin
  BIWeb.PublicFolder.Enabled:=CBPublic.IsChecked;
  TBIRegistry.WriteBoolean('BIWeb','PublicEnabled',BIWeb.PublicFolder.Enabled);
end;

procedure TBIWebMain.TryEnableScheduler;
begin
  BIWeb.Scheduler.Enabled:=CBScheduler.IsChecked;
  TimerScheduler.Enabled:=BIWeb.Scheduler.Enabled;
end;

procedure TBIWebMain.CBSchedulerChange(Sender: TObject);
begin
  TBIRegistry.WriteBoolean('BIWeb','Scheduler',CBScheduler.IsChecked);
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
  TUICommon.SavePosition(Self,'FMXBIWeb');
end;

procedure TBIWebMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CloseFromMenu then
  begin
    if Server.Active and (Server.Contexts.Count>0) then
       CanClose:=TUICommon.YesNo(Format(BIMsg_ServerSureToClose,[Server.Contexts.Count]))
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

procedure TBIWebMain.FormCreate(Sender: TObject);
begin
  TUICommon.LoadPosition(Self,'FMXBIWeb');

  FirstTime:=True;

  LVersion.Text:='Version: '+TBIWebServer.Version.ToString;

  TabControl1.ActiveTab:=TabConsole;

  NumberBox1.Value:=TBIWebClient.DefaultPort;

  Server.DefaultPort:=TBIRegistry.ReadInteger('BIWeb','Port',TBIWebClient.DefaultPort);

  {$IFNDEF MSWINDOWS}
  // http://codeverge.com/embarcadero.delphi.firemonkey/indy-http-server-android-xe5/1056236
  Server.Bindings.Add.IPVersion := id_IPv4;
  {$ENDIF}

  History:=TBIWebHistory.Create;
  History.Name:='History';

  HistoryGrid:=TBIGrid.Embedd(Self,TabHistory);
  HistoryGrid.BindTo(History);
end;

procedure TBIWebMain.FormDestroy(Sender: TObject);
begin
  Data.Free;
  History.Free;
  BIWeb.Free;
end;

procedure TBIWebMain.FormShow(Sender: TObject);
var S : String;
begin
  if ParamCount>0 then
     S:=ParamStr(1)
  else
     S:='';

  if Trim(S)='' then
     S:=FMXDefaultStore;

  CreateAllData(S);

  BIWeb:=TBIWebCommon.Create;
  BIWeb.Data:=Data;

  BIWeb.Logs.History:=History;
  BIWeb.Logs.AddHistory:=AddHistory;

  BIWeb.Scheduler.Refresh(S);
  BIGrid1.Data:=BIWeb.Scheduler.Data;

  Server.Active:=True;

  CBActive.IsChecked:=Server.Active;

  {$IFDEF MSWINDOWS}
  TrayIcon:=TnTrayIcon.Create(Self);
  TrayIcon.Hint:='Steema BIWeb';
  TrayIcon.BalloonText:='BIWeb Server';
  TrayIcon.BalloonTitle:='Steema';
  TrayIcon.IconBalloonType:=Info;
  TrayIcon.Indent:=150;
  TrayIcon.PopUpMenu:=PopupMenu1;
  TrayIcon.OnClick:=nTrayIcon1Click;

  TrayIcon.Show;
  {$ELSE}
  CloseFromMenu:=True;
  {$ENDIF}

  AppOnTaskbar(Self,True);

  CBAutoUpdate.IsChecked:=TBIRegistry.ReadBoolean('BIWeb','AutoUpdate', {$IFDEF MSWINDOWS}True{$ELSE}False{$ENDIF});
  CBStartMin.IsChecked:=TBIRegistry.ReadBoolean('BIWeb','Minimized', {$IFDEF MSWINDOWS}True{$ELSE}False{$ENDIF});

  SetupLogs;
  SetupPublicFolder;

  Log('Started: '+DateTimeToStr(Now));

  Timer1.Enabled:=CBAutoUpdate.IsChecked;

  CBScheduler.IsChecked:=TBIRegistry.ReadBoolean('BIWeb','Scheduler',False);
  TryEnableScheduler;
end;

procedure TBIWebMain.SetupLogs;
begin
  BIWeb.Logs.Persist:=TBIRegistry.ReadBoolean('BIWeb','LogPersist',True);
  CBLogs.IsChecked:=BIWeb.Logs.Persist;

  BIWeb.Logs.Store:=TBIRegistry.ReadString('BIWeb','LogStore','');

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
  BIWebMain.Show;

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
  Server.DefaultPort:=Round(NumberBox1.Value);

  TBIRegistry.WriteInteger('BIWeb','Port',Server.DefaultPort);
end;

procedure TBIWebMain.RefreshCount;
begin
  if Server<>nil then
     TThread.Synchronize(nil,procedure
     begin
       LConnections.Text:=IntToStr(Server.Contexts.Count);
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
  if MemoLog<>nil then
     TThread.Synchronize(nil,procedure
     begin
       MemoLog.Lines.Add(S);
     end);
end;

procedure TBIWebMain.AddHistory(const AContext:TBIWebContext;
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

procedure TBIWebMain.FaviconClick(Sender: TObject);
begin
  TUICommon.GotoURL(nil,'http://localhost:'+IntToStr(Server.DefaultPort));
end;

function TBIWebMain.FaviconStream:TStream;
begin
  result:=TMemoryStream.Create;
  Favicon16.Bitmap.SaveToStream(result);
end;

procedure TBIWebMain.ServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  if SameText(ARequestInfo.Document,'/favicon.ico') then
  begin
    //AResponseInfo.Pragma:='Cache-Control: public';
    AResponseInfo.ContentType:='image/x-icon';
    AResponseInfo.ContentStream:=FaviconStream; // "data:;base64,iVBORw0KGgo="
  end
  else
  try
    try
      TBIIndyContext.Process(BIWeb,AContext,ARequestInfo,AResponseInfo);
    except
      on E:Exception do
      begin
        Log(E.Message);
        AResponseInfo.ContentText:=E.Message;
      end;
    end;
  finally
    if TabControl1.ActiveTab=TabSettings then
       RefreshMemory;
  end;
end;

procedure TBIWebMain.ServerConnect(AContext: TIdContext);
begin
  if Visible then
     RefreshCount;
end;

procedure TBIWebMain.ServerDisconnect(AContext: TIdContext);
begin
  if Visible then
     RefreshCount;
end;

procedure TBIWebMain.ServerStatus(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);
begin
  LStatus.Text:=AStatusText;
end;

procedure TBIWebMain.TabControl1Change(Sender: TObject);
begin
  if TabControl1.ActiveTab=TabSettings then
     RefreshMemory;
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
