{*********************************************}
{  TeeBI Software Library                     }
{  Web Server (VCL)                           }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Main_VCL_Web;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer, IdContext, Vcl.StdCtrls,
  Vcl.ComCtrls, BI.Web.AllData, BI.Web, BI.Web.Common,
  BI.Persist, Data.DB, Vcl.ExtCtrls, Datasnap.DBClient, Vcl.Grids, Vcl.DBGrids,
  BI.VCL.Grid, Vcl.Menus, BI.VCL.DataControl;

type
  TFormBIWeb = class(TForm)
    Server: TIdHTTPServer;
    StatusBar1: TStatusBar;
    TrayIcon1: TTrayIcon;
    PageControl1: TPageControl;
    TabConsole: TTabSheet;
    ErrorLog: TMemo;
    TabSheet2: TTabSheet;
    TabSettings: TTabSheet;
    Label1: TLabel;
    LMemory: TLabel;
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
    LVersion: TLabel;
    HistoryGrid: TBIGrid;
    PopupMenu1: TPopupMenu;
    Show1: TMenuItem;
    Exit1: TMenuItem;
    CBStartMin: TCheckBox;
    GroupBox1: TGroupBox;
    CBLogs: TCheckBox;
    Label4: TLabel;
    CBLogStore: TComboBox;
    GroupBox2: TGroupBox;
    CBPublic: TCheckBox;
    Label5: TLabel;
    EPublic: TEdit;
    CBAutoUpdate: TCheckBox;
    Button2: TButton;
    Timer1: TTimer;
    TabScheduler: TTabSheet;
    Panel3: TPanel;
    CBScheduler: TCheckBox;
    TimerScheduler: TTimer;
    BIGrid1: TBIGrid;
    procedure FormCreate(Sender: TObject);
    procedure ServerConnect(AContext: TIdContext);
    procedure ServerDisconnect(AContext: TIdContext);
    procedure ServerException(AContext: TIdContext; AException: Exception);
    procedure ServerStatus(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure ServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
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
  private
    { Private declarations }
    Data : TAllData;
    History : TBIWebHistory;

    BIWeb : TBIWebCommon;

    CloseFromMenu,
    FirstTime : Boolean;

    procedure AddHistory(const AContext:TIdContext;
                         const Command:String;
                         const Tag:String;
                         const Success:Boolean;
                         const Millisec:Integer; const Size:Int64);


    procedure CheckForUpdates;
    procedure CreateAllData(const AStore:String);
    function FaviconStream:TStream;
    procedure Log(const S:String);
    procedure RefreshCount;
    procedure RefreshGrid;
    procedure RefreshMemory;
    procedure SetHistoryWidths;
    procedure SetupLogs;
    procedure SetupPublicFolder;
    procedure TryEnableScheduler;
  public
    { Public declarations }
  end;

var
  FormBIWeb: TFormBIWeb;

implementation

{$R *.dfm}

uses
  BI.Arrays, BI.Data, BI.UI, Unit_Constants, BI.Data.Html, BI.VCL.DataManager,
  BI.Data.CSV, BI.Data.JSON, BI.Data.XML,

  {$IFNDEF FPC}
  {$IF CompilerVersion>26}
  BI.Data.DB.FireDAC,
  {$ELSE}
  BI.Data.DB.SqlExpr,
  {$ENDIF}
  {$ENDIF}

  System.UITypes, BI.VCL.Editor.Stores, BI.Languages.English;

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

procedure TFormBIWeb.AddHistory(const AContext:TIdContext;
                          const Command, Tag: String; const Success: Boolean;
                          const Millisec:Integer; const Size:Int64);
var tmpNow : TDateTime;
    IP: String;
begin
  tmpNow:=Now;

  IP:=AContext.Connection.Socket.Binding.PeerIP;

  History.Add(tmpNow,IP,Command,Tag,Success,Millisec,Size);

  if not Success then
     Log(DateTimeToStr(Now)+' '+IP+' '+Command+' '+Tag);

  if Visible then
     RefreshGrid;
end;

procedure TFormBIWeb.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TUICommon.SavePosition(Self,'VCLBIWeb');
end;

procedure TFormBIWeb.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
var S : String;
begin
  TUICommon.LoadPosition(Self,'VCLBIWeb');

  FirstTime:=True;

  LVersion.Caption:='Version: '+TBIWebServer.Version.ToString;

  PageControl1.ActivePage:=TabConsole;

  UDPort.Position:=TBIWebClient.DefaultPort;
  Server.DefaultPort:=TBIWebClient.DefaultPort;

  StatusBar1.Panels.Add;

  if ParamCount>0 then
     S:=ParamStr(1)
  else
     S:='';

  if Trim(S)='' then
     S:=VCLDefaultStore;

  History:=TBIWebHistory.Create;
  History.Name:='History';

  HistoryGrid.BindTo(History);

  SetHistoryWidths;

  CreateAllData(S);

  BIWeb:=TBIWebCommon.Create;
  BIWeb.Logs.History:=History;
  BIWeb.Data:=Data;
  BIWeb.Logs.AddHistory:=AddHistory;

  BIWeb.Scheduler.Refresh(S);
  BIGrid1.Data:=BIWeb.Scheduler.Data;

  Server.Active:=True;

  CBActive.Checked:=Server.Active;

  CBAutoUpdate.Checked:=TBIRegistry.ReadBoolean('BIWeb','AutoUpdate', {$IFDEF MSWINDOWS}True{$ELSE}False{$ENDIF});
  CBStartMin.Checked:=TBIRegistry.ReadBoolean('BIWeb','Minimized', {$IFDEF MSWINDOWS}True{$ELSE}False{$ENDIF});

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
  BIWeb.PublicFolder.Enabled:=TBIRegistry.ReadBoolean('BIWeb','PublicEnabled',True);
  CBPublic.Checked:=BIWeb.PublicFolder.Enabled;

  BIWeb.PublicFolder.Path:=TBIRegistry.ReadString('BIWeb','PublicFolder','public');
  CBPublic.Checked:=BIWeb.PublicFolder.Enabled;
end;

procedure TFormBIWeb.SetupLogs;
begin
  BIWeb.Logs.Persist:=TBIRegistry.ReadBoolean('BIWeb','LogPersist',True);
  CBLogs.Checked:=BIWeb.Logs.Persist;

  BIWeb.Logs.Store:=TBIRegistry.ReadString('BIWeb','LogStore','');

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
  S:=TStore.PathOf(AStore);

  // Protection against self-recursivity:
  if SameText(S,'WEB:LOCALHOST') or SameText(S,'WEB:LOCALHOST:'+IntToStr(Server.DefaultPort)) then
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
  Server.DefaultPort:=UDPort.Position;

  TBIRegistry.WriteInteger('BIWeb','Port',Server.DefaultPort);
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

procedure TFormBIWeb.ServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  if SameText(ARequestInfo.Document,'/favicon.ico') then
  begin
    AResponseInfo.ContentType:='image/x-icon';
    AResponseInfo.ContentStream:=FaviconStream; // "data:;base64,iVBORw0KGgo="
  end
  else
  try
    try
      if ARequestInfo.Document<>'/' then
         BIWeb.ProcessFile(ARequestInfo.Document,AContext,AResponseInfo)
      else
      if ARequestInfo.CommandType=hcGET then
         BIWeb.ProcessGet(AContext,AResponseInfo,ARequestInfo)
      else
      if ARequestInfo.CommandType=hcPOST then
         BIWeb.ProcessPost(AContext,AResponseInfo,ARequestInfo);

    except
      on E:Exception do
      begin
        Log(E.Message);
        AResponseInfo.ContentText:=E.Message;
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
       LConnections.Caption:=IntToStr(Server.Contexts.Count);
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

procedure TFormBIWeb.ServerConnect(AContext: TIdContext);
begin
  RefreshCount;
end;

procedure TFormBIWeb.ServerDisconnect(AContext: TIdContext);
begin
  RefreshCount;
end;

procedure TFormBIWeb.ServerException(AContext: TIdContext;
  AException: Exception);
begin
  if ErrorLog<>nil then
     ErrorLog.Lines.Add(AException.Message);
end;

procedure TFormBIWeb.ServerStatus(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);
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

{$IFDEF MSWINDOWS}
var
  Mutex : THandle=0;

initialization
  Mutex:=CreateMutex(nil,True,'BIWeb.ts');

  if GetLastError=ERROR_ALREADY_EXISTS then
  begin
    MessageDlg(BIMsg_ServerAlreadyRunning, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
    Halt;
  end;

finalization
  if Mutex<>0 then
     CloseHandle(Mutex);

{$ENDIF}
end.
