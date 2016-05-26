// ---------------------------------------------------------------------------//
// Автор: Nix0N                                                               //
// e-mail: livtavit@mail.ru                                                   //
// www.nixcode.ru                                                             //
// Версия: 0.1                                                                //
// Платформа: Windows Firemonkey
// ---------------------------------------------------------------------------//

unit nFMX.Trayicon.Win;

interface

uses
  System.SysUtils, System.Classes, Winapi.ShellAPI, Winapi.Windows,
  Winapi.Messages, FMX.Platform.Win, FMX.Dialogs, FMX.Menus, FMX.Forms,
  FMX.Objects;

const
  WM_TRAYICON = WM_USER + 1;

type
  TBalloonIconType = (None, Info, Warning, Error, User, BigWarning, BigError);

type
  TnTrayIcon = class(TComponent)
  private
    nHint: string;
    nBalloonTitle: string;
    nBalloonText: string;
    nBalloonIconType: TBalloonIconType;
    nTrayIcon: TNotifyIconData;
    nTrayMenu: TPopupMenu;
    nVersion: string;
    nIndent: Integer;
    nOnClick: TNotifyEvent;
    nOnDblClick: TNotifyEvent;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Show;
    procedure Hide;
    procedure ShowBallonHint;
  published
    property Hint: string read nHint write nHint;
    property BalloonText: string read nBalloonText write nBalloonText;
    property BalloonTitle: string read nBalloonTitle write nBalloonTitle;
    property IconBalloonType: TBalloonIconType read nBalloonIconType
      write nBalloonIconType;
    property Indent: Integer read nIndent write nIndent;
    property PopUpMenu: TPopupMenu read nTrayMenu write nTrayMenu;
    property Version: string read nVersion write nVersion;
    property OnClick: TNotifyEvent read nOnClick write nOnClick;
    property OnDblClick: TNotifyEvent read nOnDblClick write nOnDblClick;
  end;

var
  mOldWndProc: LONG_PTR;
  mHWND: HWND;
  mPopUpMenu: TPopupMenu;
  mFirstRun: Boolean = True;
  mOnClick, mOnDblClick: TNotifyEvent;
  mIndent: Integer;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('NixCode', [TnTrayIcon]);
end;

function MyWndProc(HWND: HWND; Msg: UINT; WParam: WParam; LParam: LParam)
  : LRESULT; stdcall;
var
  CurPos: TPoint;
begin
  //Result := 0;
  if Msg = WM_TRAYICON then
  begin
    if LParam = WM_LBUTTONDBLCLK then
      mOnDblClick(nil);
    if (Msg = WM_TRAYICON) and (LParam = WM_LBUTTONDOWN) then
      mOnClick(nil);
    if LParam = WM_RBUTTONDOWN then
    begin
      SetForegroundWindow(mHWND);
      GetCursorPos(CurPos);

      if mPopUpMenu<>nil then
         mPopUpMenu.PopUp(CurPos.X, CurPos.Y - mIndent);
    end;
  end;
  Result := CallWindowProc(Ptr(mOldWndProc), HWND, Msg, WParam, LParam);
end;

constructor TnTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  nVersion := '0.1';
  nIndent := 75;
end;

procedure TnTrayIcon.Show;
begin
  mHWND := WindowHandleToPlatform((Self.Owner as TForm).Handle).Wnd;

  mPopUpMenu := nTrayMenu;
  mIndent := nIndent;
  mOnClick := nOnClick;
  mOnDblClick := nOnDblClick;

  with nTrayIcon do
  begin
    cbSize := SizeOf;
    Wnd := mHWND;
    uID := 1;
    uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
    dwInfoFlags := NIIF_NONE;
    uCallbackMessage := WM_TRAYICON;
    hIcon := GetClassLong(mHWND, GCL_HICONSM);
    StrLCopy(szTip, PChar(nHint), High(szTip));
  end;

  Shell_NotifyIcon(NIM_ADD, @nTrayIcon);

  if mFirstRun then
  begin
    mOldWndProc := GetWindowLongPtr(mHWND, GWL_WNDPROC);
    SetWindowLongPtr(mHWND, GWL_WNDPROC, LONG_PTR(@MyWndProc));
    mFirstRun := False;
  end;
end;

procedure TnTrayIcon.ShowBallonHint;
begin
  with nTrayIcon do
  begin
    StrLCopy(szInfo, PChar(nBalloonText), High(szInfo));
    StrLCopy(szInfoTitle, PChar(nBalloonTitle), High(szInfoTitle));
    uFlags := NIF_INFO;
    case nBalloonIconType of
      None:
        dwInfoFlags := 0;
      Info:
        dwInfoFlags := 1;
      Warning:
        dwInfoFlags := 2;
      Error:
        dwInfoFlags := 3;
      User:
        dwInfoFlags := 4;
      BigWarning:
        dwInfoFlags := 5;
      BigError:
        dwInfoFlags := 6;
    end;
  end;
  Shell_NotifyIcon(NIM_MODIFY, @nTrayIcon);
end;

procedure TnTrayIcon.Hide;
begin
  Shell_NotifyIcon(NIM_DELETE, @nTrayIcon);
end;

destructor TnTrayIcon.Destroy;
begin
  Shell_NotifyIcon(NIM_DELETE, @nTrayIcon);
  inherited;
end;

end.
