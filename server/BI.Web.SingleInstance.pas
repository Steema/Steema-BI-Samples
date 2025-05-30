{*********************************************}
{  TeeBI Software Library                     }
{  Prevent running more than one instance     }
{  (Windows)                                  }
{                                             }
{  Copyright (c) 2015-2025 by Steema Software }
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

uses
  {System.}SysUtils;

function CommandLine(const Code:String):Boolean;
var t : Integer;
begin
  result:=False;

  for t:=1 to ParamCount do
      if SameText(ParamStr(t),'-'+Code) then
         Exit(True);
end;

{$IFDEF MSWINDOWS}
var
  Mutex : THandle=0;

initialization
  if not CommandLine('Multi') then
  begin
    Mutex:=CreateMutex(nil,True,'BIWeb.ts');

    if GetLastError=ERROR_ALREADY_EXISTS then
    begin
      TCommonUI.ShowMessage(BIMsg_ServerAlreadyRunning);
      Halt;
    end;
  end;

finalization
  if Mutex<>0 then
     CloseHandle(Mutex);
{$ENDIF}
end.
