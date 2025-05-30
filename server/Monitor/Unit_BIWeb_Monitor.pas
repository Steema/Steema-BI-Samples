unit Unit_BIWeb_Monitor;

interface

{
 This application does a check every 60 seconds:

 1) BI_FMX_Web.exe is running.

    If if is not running (possible crash), it attempts to execute it again.

 2) A new executable update is available.

    If there is a new file (New_BI_FMX_Web.exe) it then renames the
    current executable to "old", and renames the "new" to the default name.

    Then it attempts to execute the new executable.

}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TBIWebMonitor = class(TForm)
    Timer1: TTimer;
    Memo1: TMemo;
    Panel1: TPanel;
    CBCheckRun: TCheckBox;
    Label1: TLabel;
    ESeconds: TEdit;
    UDSeconds: TUpDown;
    Label2: TLabel;
    CBUpdates: TCheckBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBCheckRunClick(Sender: TObject);
    procedure ESecondsChange(Sender: TObject);
    procedure CBUpdatesClick(Sender: TObject);
  private
    { Private declarations }

    Every,
    Seconds : Integer;

    Home : String;

    function IsBIWebRunning:Boolean;
    procedure CloseBIWeb;
    procedure CopyUpdate;
    procedure DoCheck;
    procedure FindHome;
    procedure InitTimer;
    procedure Log(const AMessage:String);
    function PathOf(const AFileName:String):String;
    procedure ReStart;
    function UpdateAvailable:Boolean;
    procedure DoUpdate;
  public
    { Public declarations }
  end;

var
  BIWebMonitor: TBIWebMonitor;

implementation

{$R *.dfm}

uses
  System.IOUtils, Winapi.ShellApi, BI.Persist, VCLBI.Grid;

const
  BIWebName='BI_FMX_Web.exe';
  OldName='Old_'+BIWebName;
  NewName='New_'+BIWebName;

function TBIWebMonitor.IsBIWebRunning:Boolean;
const
  MutexError='Internal error: Cannot create Mutex';

var Mutex : THandle;
begin
  Mutex:=CreateMutex(nil,True,'BIWeb.ts');
  try
    result:=GetLastError=ERROR_ALREADY_EXISTS;

    if not result then
       if Mutex=0 then
       begin
         Log(MutexError);
         raise Exception.Create(MutexError);
       end;
  finally
    if Mutex<>0 then
       CloseHandle(Mutex);
  end;
end;

function TBIWebMonitor.PathOf(const AFileName:String):String;
begin
  result:=TPath.Combine(Home,AFileName);
end;

procedure RunExecutable(const AFileName:String);
begin
  ShellExecute(0,'open',PWideChar(AFileName),nil,nil,SW_SHOWNORMAL);
end;

procedure TBIWebMonitor.ReStart;
begin
  Log('Restarting BIWeb');
  RunExecutable(PathOf(BIWebName));
end;

procedure TBIWebMonitor.Log(const AMessage:String);
const
  LogFile='BIWeb_Monitor.log';

var f : System.TextFile;
    tmp : String;
begin
  tmp:=DateTimeToStr(Now)+' , '+AMessage; // CSV

  Memo1.Lines.Add(tmp);

  AssignFile(f,LogFile);
  try
    if TFile.Exists(LogFile) then
       Append(f)
    else
       Rewrite(f);

    WriteLn(f,tmp);
    Flush(f);
  finally
    CloseFile(f);
  end;
end;

procedure TBIWebMonitor.Button1Click(Sender: TObject);
begin
  DoCheck;
  Seconds:=0;
end;

procedure TBIWebMonitor.CBCheckRunClick(Sender: TObject);
begin
  TBIRegistry.WriteBoolean('BIWebMonitor','CheckRunning',CBCheckRun.Checked);
  InitTimer;
end;

procedure TBIWebMonitor.CBUpdatesClick(Sender: TObject);
begin
  TBIRegistry.WriteBoolean('BIWebMonitor','CheckUpdates',CBUpdates.Checked);
  InitTimer;
end;

procedure TBIWebMonitor.CloseBIWeb;
var WindowHandle : HWND;
begin
  WindowHandle:=FindWindow(nil, PChar('BIWeb Server'));

  if WindowHandle<>0 then
  begin
    if not PostMessage(WindowHandle, WM_CLOSE, 0, 0) then
       Log('Cannot close BIWeb');
  end;
end;

procedure TBIWebMonitor.CopyUpdate;
begin
  Log('Copying update');

  // Rename existing to "Old"
  if FileExists(PathOf(BIWebName)) then
  begin
    // Remove "Old" if it exists
    if FileExists(PathOf(OldName)) then
       TFile.Delete(PathOf(OldName));

    // Rename current to "Old"
    RenameFile(PathOf(BIWebName),PathOf(OldName));
  end;

  // Rename new to default
  if FileExists(PathOf(NewName)) then
     RenameFile(PathOf(NewName),PathOf(BIWebName));
end;

procedure TBIWebMonitor.DoCheck;
begin
  if CBUpdates.Checked and UpdateAvailable then
     DoUpdate;

  if CBCheckRun.Checked and (not IsBIWebRunning) then
  begin
    Log('Not running. Trying to restart');
    ReStart;
  end;
end;

procedure TBIWebMonitor.DoUpdate;
begin
  CloseBIWeb;
  CopyUpdate;
  Restart;
end;

procedure TBIWebMonitor.ESecondsChange(Sender: TObject);
begin
  TBIRegistry.WriteInteger('BIWebMonitor','Seconds',Every);
  InitTimer;
end;

procedure TBIWebMonitor.FindHome;

  function DeveloperPath:String;
  begin
    result:=TPath.Combine(Home,'FMX\Win32\Debug');
  end;

var tmp : String;
begin
  Home:=TDirectory.GetCurrentDirectory;

  repeat
    if TFile.Exists(TPath.Combine(Home,BIWebName)) then
       Exit
    else
    // Try developer / debug
    if TFile.Exists(TPath.Combine(DeveloperPath,BIWebName)) then
    begin
      Home:=DeveloperPath;
      Exit;
    end
    else
       Home:=TDirectory.GetParent(Home);

  until Home='';

  tmp:='Error: Cannot find directory with '+BIWebName;

  Log(tmp);

  ShowMessage(tmp);

  Application.Terminate;
end;

procedure TBIWebMonitor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Log('Closing BIWeb monitor');
  TUICommon.SavePosition(Self,'BIWebMonitor');
end;

procedure TBIWebMonitor.InitTimer;
begin
  Every:=UDSeconds.Position;
  Timer1.Enabled:=CBCheckRun.Checked or CBUpdates.Checked;
end;

procedure TBIWebMonitor.FormCreate(Sender: TObject);
begin
  Log('--------');
  Log('Starting BIWeb monitor');

  FindHome;

  Log('Home directory: '+Home);

  TUICommon.LoadPosition(Self,'BIWebMonitor');

  CBCheckRun.Checked:=TBIRegistry.ReadBoolean('BIWebMonitor','CheckRunning',True);
  CBUpdates.Checked:=TBIRegistry.ReadBoolean('BIWebMonitor','CheckUpdates',True);
  UDSeconds.Position:=TBIRegistry.ReadInteger('BIWebMonitor','Seconds',60);

  InitTimer;

  DoCheck;
end;

procedure TBIWebMonitor.Timer1Timer(Sender: TObject);
begin
  Inc(Seconds);

  if Seconds>Every then
  begin
    Timer1.Enabled:=False;
    try
      DoCheck;
      Seconds:=0;
    finally
      Timer1.Enabled:=True;
    end;
  end;
end;

function TBIWebMonitor.UpdateAvailable: Boolean;
begin
  result:=TFile.Exists(PathOf(NewName));
end;

end.
