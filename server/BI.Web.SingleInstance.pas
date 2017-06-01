{*********************************************}
{  TeeBI Software Library                     }
{  Prevent running more than one instance     }
{  (Windows)                                  }
{                                             }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.SingleInstance;

interface

uses
  {$IFDEF MSWINDOWS}
  WinAPI.Windows,
  {$ENDIF}

  BI.Languages.English, BI.UI;

implementation

{$IFDEF MSWINDOWS}
var
  Mutex : THandle=0;

initialization
  Mutex:=CreateMutex(nil,True,'BIWeb.ts');

  if GetLastError=ERROR_ALREADY_EXISTS then
  begin
    TCommonUI.ShowMessage(BIMsg_ServerAlreadyRunning);
    Halt;
  end;

finalization
  if Mutex<>0 then
     CloseHandle(Mutex);
{$ENDIF}
end.
