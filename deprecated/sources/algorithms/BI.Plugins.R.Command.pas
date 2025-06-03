{*********************************************}
{  TeeBI Software Library                     }
{  Plugin for R language "cmd batch" mode     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Plugins.R.Command deprecated;

// Important NOTE:
(*
   This unit is now obsolete, superseded by BI.Plugins.R.opaR unit.

   opaR is a native integration between R.dll and Delphi,
   offering much more capable performance and, most important,
   removing the "cmd batch" mode limitations of single outputs
   from R scripts.
*)

interface

uses
  System.Classes, BI.Arrays, BI.DataItem, BI.Plugins.R;

// The TRCommand class detects if R is installed or not by looking at system
// Registry or "R_HOME" environmental variable.

// TRCommand.Create constructor can be called passing a custom path of the R bin
// folder.

type
  TR_getDLLVersion=function: IntPtr; cdecl;
  TR_setStartTime=procedure; cdecl;

  TRCommand=class(TBIREngine)
  private
    FPath : String;
    IR : THandle;

    getDLLVersion : TR_getDLLVersion;
    setStartTime : TR_setStartTime;

    ErrorCode : Cardinal;

    ILine,
    IPos : Integer;

    IOutputs : TDataArray;

    IScript : TStrings;

    procedure DoGetOutputs;
    class function FindDLL(const APath:String): String; static;
    procedure LoadDLL;
  protected
    procedure CheckDLL;
    function ExecuteFile(const AScriptFile:String; out ExitCode:LongWord):Boolean;
    function NextItem(const HasBrackets:Boolean):String;
    procedure ParseItem(const AIndex:TLoopInteger; const AData:TDataItem; const HasBrackets:Boolean);
    function Execute(const AScript:String; out ExitCode:LongWord):Boolean; overload;
    function Execute(const AScript:TStrings; out ExitCode:LongWord):Boolean; overload;
  public
    Constructor Create(const APath:String='');
    Destructor Destroy; override;

    class function DLLPath:String; static;
    function Finish:Boolean; override;

    procedure AddVariable(const AName:String; const Index:TNativeIntArray;
                          const AData:TDataArray; const UseMissing:Boolean=True); override;

    procedure GetVariable(const AName:String; const AData:TDataItem); override;
    procedure LoadPackage(const APackage:String); override;
    procedure ParseOutput(const ADest:TDataItem); override;
    procedure ParseRawMap(const AMap,ADest:TDataItem); override;

    procedure Start; override;
    procedure Statement(const AStatement:String); override;
    function Version:String; override;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, System.Win.Registry,
  {$ENDIF}
  System.SysUtils, System.IOUtils,
  {$IFNDEF IOS}
  {$IFNDEF ANDROID}
  System.AnsiStrings,
  {$ENDIF}
  {$ENDIF}
  BI.UI;

type
  {$IFDEF IOS}
  PAnsiChar=PChar;
  {$ENDIF}

  {$IFDEF ANDROID}
  PAnsiChar=PChar;
  {$ENDIF}

  Bool=LongBool;

  {$MINENUMSIZE 4}
  TStartupAction=(NoRestore,Restore,Default,SaveDefault,NoSave,Save,Ask,Suicide);
  TUiMode=(RGui,RTerminal,LinkDll);

  TRUnixStart=packed record
    R_Quiet,
    R_Slave,
    R_Interactive,
    R_Verbose,
    LoadSiteFile,
    LoadInitFile,
    DebugInitFile : Bool;

    RestoreAction:TStartupAction;
    SaveAction:TStartupAction;

    vsize,
    nsize,
    max_vsize,
    max_nsize,
    ppsize: UInt32;

    NoRenviron:Integer;
  end;

  blah1=function(const A,B:PAnsiChar; C,D:Integer):Integer; cdecl;
  blah2=procedure(const A:PAnsiChar; B:Integer); cdecl;
  blah3=procedure; cdecl;
  blah4=procedure(const A:PAnsiChar); cdecl;
  blah5=function(const A:PAnsiChar):Integer; cdecl;
  blah6=procedure(A:Integer); cdecl;
  blah7=procedure(const A:PAnsiChar; B,C:Integer); cdecl;

  TRStart=packed record
    Common:TRUnixStart;

    rhome,
    home: Integer;

    ReadConsole:blah1;
    WriteConsole:blah2;
    CallBack:blah3;
    ShowMessage:blah4;
    YesNoCancel:blah5;
    Busy:blah6;

    CharacterMode:TUiMode;

    WriteConsoleEx:blah7;
  end;

  TR_SetParams=procedure(var Start:TRStart); cdecl;

  // TRf_initialize_R=
  // Tsetup_Rmainloop=

function RReadConsole(const A,B:PAnsiChar; C,D:Integer):Integer; cdecl;
begin
  result:=0;
end;

procedure RWriteConsole(const A:PAnsiChar; B:Integer); cdecl;
begin
end;

procedure RCallback; cdecl;
begin
end;

procedure RShowMessage(const A:PAnsiChar); cdecl;
begin
  TCommonUI.ShowMessage(String(A));
end;

function RYesNoCancel(const A:PAnsiChar):Integer; cdecl;
begin
  // 1 for Yes, -1 for No and 0 for Cancel:
  result:=0;
end;

procedure RBusy(A:Integer); cdecl;
begin
end;

procedure RWriteConsoleEx(const A:PAnsiChar; B,C:Integer); cdecl;
begin
end;

type
  TR_DefParams=procedure(var P:TRStart); cdecl;

{ TRCommand }

Constructor TRCommand.Create(const APath: String='');
begin
  inherited Create;

  if APath='' then
     FPath:=DllPath
  else
     FPath:=APath;

  IScript:=TStringList.Create;
end;

Destructor TRCommand.Destroy;
begin
  IScript.Free;
  inherited;
end;

{$IFNDEF MSWINDOWS}
procedure ZeroMemory(Destination: Pointer; const Length: NativeUInt);
begin
  FillChar(Destination^, Length, 0);
end;
{$ENDIF}

procedure TRCommand.CheckDLL;

  {$IFNDEF IOS}
  {$IFNDEF ANDROID}
  {$DEFINE DOSTARTUP}
  {$ENDIF}
  {$ENDIF}

  {$IFDEF DOSTARTUP}
  procedure StartUp;
  var setParams:TR_SetParams;
      tmp : TRStart;
      tmpSt : AnsiString;
      R_DefParams:TR_DefParams;
  begin
    ZeroMemory(@tmp,SizeOf(tmp));

    @R_DefParams:=GetProcAddress(IR,'R_DefParams');
    R_DefParams(tmp);

    tmp.Common.R_Quiet:=True;
    tmp.Common.R_Slave:=True;
    tmp.Common.R_Interactive:=False;
    tmp.Common.R_Verbose:=not tmp.Common.R_Quiet;

    tmp.Common.RestoreAction:=TStartupAction.Restore;
    tmp.Common.SaveAction:=TStartupAction.NoSave;

    tmpSt:=Copy(AnsiString(FPath),1,Length(FPath));

    tmp.rhome:=Integer(@tmpSt[1]);
    tmp.home:=Integer(@tmpSt[1]);

    tmp.ReadConsole:=RReadConsole;
    tmp.WriteConsole:=RWriteConsole;
    tmp.CallBack:=RCallBack;
    tmp.ShowMessage:=RShowMessage;
    tmp.YesNoCancel:=RYesNoCancel;
    tmp.Busy:=RBusy;

    tmp.WriteConsoleEx:=RWriteConsoleEx;

    setParams:=GetProcAddress(IR,'R_SetParams');
    setParams(tmp);
  end;
  {$ENDIF}

begin
  if IR=0 then
  begin
    LoadDLL;
    setStartTime;

    {$IFDEF DOSTARTUP}
    StartUp;
    {$ENDIF}
  end;
end;

class function TRCommand.FindDLL(const APath:String): String;
const
  RDLL='R.dll';

var tmp : String;
begin
  tmp:=TPath.Combine(APath,RDLL);

  if FileExists(tmp) then
     result:=tmp
  else
     result:='';
end;

function TRCommand.Finish:Boolean;
begin
  inherited;

  result:=Execute(IScript,ErrorCode);

  if result then
     DoGetOutputs;
end;

procedure TRCommand.GetVariable(const AName: String; const AData: TDataItem);
begin
  // WRONG:
  // This only works for ONE variable, so it unqualifies R cmd batch mode
  // as a reliable way to obtain outputs. It only works for ONE output.
  //
  // The AName parameter in this case is simply lost.

  IOutputs.Add(AData);
  Statement('print('+AName+')');
end;

class function TRCommand.DllPath:String;
var RHome : String;

    {$IFDEF MSWINDOWS}
    tmp : String;
    R : TRegistry;
    {$ENDIF}
begin
  RHome:=GetEnvironmentVariable('R_HOME');

  if RHome='' then
     result:=''
  else
     result:=FindDll(RHome);

  if result='' then
  begin
    {$IFDEF MSWINDOWS}
    R:=TRegistry.Create(KEY_READ);
    try
      R.RootKey:=HKEY_LOCAL_MACHINE;

      if R.OpenKeyReadOnly('Software\R-core\R') then
         tmp:=R.ReadString('InstallPath')
      else
      if R.OpenKeyReadOnly('Software\R-core\R64') then
         tmp:=R.ReadString('InstallPath')
      else
         tmp:='';

      if tmp<>'' then
         {$IFDEF CPUX64}
         result:=FindDll(TPath.Combine(tmp,'bin\x64'));
         {$ELSE}
         result:=FindDll(TPath.Combine(tmp,'bin\i386'));
         {$ENDIF}
    finally
      R.Free;
    end;
    {$ENDIF}
  end;
end;

// WRONG:
// Using R "cmd batch" mode, there is only possible (without much work)
// to obtain ONE output. This unqualifies most algorithms that return more
// than one vector or dataframe.
procedure TRCommand.DoGetOutputs;
var tmp : TDataItem;
begin
  for tmp in IOutputs do
      ParseOutput(tmp);
end;

procedure TRCommand.LoadDLL;
var Old : String;
begin
  if IR=0 then
  begin
    Old:=TDirectory.GetCurrentDirectory;
    try
      TDirectory.SetCurrentDirectory(TPath.GetDirectoryName(FPath));
      IR:=SafeLoadLibrary(FPath);
    finally
      TDirectory.SetCurrentDirectory(Old);
    end;

    if IR=0 then
       raise Exception.Create('Error: Cannot load R library: '+FPath)
    else
    begin
      @getDllVersion:=GetProcAddress(IR,'getDLLVersion');
      @setStartTime:=GetProcAddress(IR,'R_setStartTime');
    end;
  end;
end;

procedure TRCommand.LoadPackage(const APackage: String);
begin
  AddPackage(IScript,APackage);
end;

function TRCommand.Version: String;
begin
  CheckDLL;
  result:=String(PAnsiChar(getDllVersion));
end;

function StartRAndWait(const RPath,CommandLine:String; out ExitCode:LongWord):Boolean;
{$IFDEF MSWINDOWS}
var Proc_info: TProcessInformation;
    Startinfo: TStartupInfo;
    RExe : String;
begin
  result:=False;
  ExitCode:=0;

  { Initialize the structures }
  ZeroMemory(@proc_info, SizeOf(TProcessInformation));
  ZeroMemory(@startinfo, SizeOf(TStartupInfo));

  Startinfo.cb:=SizeOf(TStartupInfo);
  Startinfo.dwFlags:=STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  Startinfo.wShowWindow:=SW_HIDE;

  RExe:=TPath.Combine(TPath.GetDirectoryName(RPath),'R.exe');

  { Attempt to create the process. If successful wait for it to end}
  if CreateProcess(Nil, PChar(RExe+' ' + CommandLine), nil,
     nil,False, CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS, nil,
     nil, StartInfo, proc_info) then
  try
    WaitForSingleObject(proc_info.hProcess, INFINITE);
    GetExitCodeProcess(proc_info.hProcess, ExitCode);
    result:=True;
  finally
    CloseHandle(proc_info.hThread);
    CloseHandle(proc_info.hProcess);
  end;
{$ELSE}
begin
  result:=False;
  ExitCode:=0;

  // PENDING
{$ENDIF}
end;

type
  TR_tryEval=function(Statement,Environment:IntPtr; var Error:Bool):IntPtr; cdecl;

function TRCommand.ExecuteFile(const AScriptFile: String; out ExitCode:LongWord): Boolean;
var tmpOutput : String;
begin
  tmpOutput:=TPath.GetTempFileName;
  result:=StartRAndWait(FPath,'CMD BATCH --quiet --slave --vanilla --no-timing '+AScriptFile+' '+tmpOutput+' >$null',ExitCode);

  if result then
  begin
    try
      Output.LoadFromFile(tmpOutput);
    finally
      TFile.Delete(tmpOutput);
    end;
  end
  else
    Output.Clear;
end;

function TRCommand.Execute(const AScript:TStrings; out ExitCode:LongWord): Boolean;
var tmpName : String;
begin
  tmpName:=TPath.ChangeExtension(TPath.GetTempFileName,'.R');

  AScript.SaveToFile(tmpName);
  try
    result:=ExecuteFile(tmpName,ExitCode);
  finally
    TFile.Delete(tmpName);
  end;
end;

function TRCommand.Execute(const AScript:String; out ExitCode:LongWord): Boolean;
var tmpIn : TStrings;
begin
  tmpIn:=TStringList.Create;
  try
    tmpIn.Text:=AScript;
    result:=Execute(tmpIn,ExitCode);
  finally
    tmpIn.Free;
  end;
end;

function TRCommand.NextItem(const HasBrackets:Boolean):String;
var tmp : String;
    L : Integer;

  procedure SkipBlanks;
  begin
    while IPos<=L do
          if tmp[IPos]=' ' then
             Inc(IPos)
          else
             break;
  end;

  function GetItem:String;
  var InString : Boolean;
  begin
    result:='';

    InString:=False;

    while IPos<=L do
    begin
      if tmp[IPos]=' ' then
      begin
        if not InString then
           break;
      end
      else
      if tmp[IPos]='"' then
         InString:=not InString
      else
         result:=result+tmp[IPos];

      Inc(IPos);
    end;

    if IPos>=L then
    begin
      Inc(ILine);
      IPos:=0;
    end;
  end;

begin
  if ILine>=Output.Count then
     raise EBIException.Create('End of output lines at: '+IntToStr(ILine));

  tmp:=Output[ILine];
  L:=Length(tmp);

  if IPos=0 then
  begin
    if HasBrackets then
    begin
      IPos:=Pos('[',tmp);

      if IPos>0 then
      begin
        repeat
          Inc(IPos);

          if tmp[IPos]=']' then
          begin
            Inc(IPos);
            break;
          end;

        until IPos>L;

        SkipBlanks;
      end
      else
        raise EBIException.Create('Cannot find "[" in R output: '+tmp);
    end
    else
    begin
      IPos:=1;
      SkipBlanks;
    end;
  end;

  result:=GetItem;

  SkipBlanks;

  if IPos>L then
  begin
    Inc(ILine);
    IPos:=0;
  end;
end;

procedure TRCommand.ParseItem(const AIndex:TLoopInteger; const AData:TDataItem; const HasBrackets:Boolean);
var tmp : String;
begin
  tmp:=NextItem(HasBrackets);

  case AData.Kind of
      dkInt32: AData.Int32Data[AIndex]:=StrToInt(tmp);
      dkInt64: AData.Int64Data[AIndex]:=StrToInt64(tmp);
     dkSingle: AData.SingleData[AIndex]:=StrToFloat(tmp);
     dkDouble: AData.DoubleData[AIndex]:=StrToFloat(tmp);
   dkExtended: AData.ExtendedData[AIndex]:=StrToFloat(tmp);
       dkText: AData.TextData[AIndex]:=tmp;
   dkDateTime: AData.DateTimeData[AIndex]:=StrToDateTime(tmp);
    dkBoolean: AData.BooleanData[AIndex]:=StrToBool(tmp);
  end;
end;

function AttributeNames(const Prefix:String; const Max:Integer):String;
var t : TLoopInteger;
begin
  result:='';

  for t:=1 to Max do
  begin
    result:=result+Prefix+IntToStr(t);

    if t<Max then
       result:=result+',';
  end;
end;

procedure TRCommand.AddVariable(const AName:String; const Index:TNativeIntArray;
                       const AData:TDataArray; const UseMissing:Boolean);

  function Vector(const AData:TDataItem):String;
  begin
    result:=DataToVector(Index,AData,UseMissing,'NA');
  end;

var t : Integer;
begin
  if High(AData)=0 then
     IScript.Add(AName+'=c('+Vector(AData[0])+')')
  else
  begin
    for t:=0 to High(AData) do
        IScript.Add(AName+IntToStr(t)+'=c('+Vector(AData[t])+')');

    IScript.Add(AName+'=cbind('+AttributeNames(AName,High(AData))+')');
  end;
end;

procedure TRCommand.ParseOutput(const ADest:TDataItem);
var t : TLoopInteger;
begin
  ILine:=0;
  IPos:=0;

  for t:=0 to ADest.Count-1 do
      ParseItem(t,ADest,True);
end;

procedure TRCommand.ParseRawMap(const AMap,ADest: TDataItem);

  procedure ParseRawItem(const AIndex:TLoopInteger);
  var tmp : String;
      tmpNum : TNativeInteger;
  begin
    tmp:=NextItem(False);

    {$IFDEF CPUX64}
    if TryStrToInt64(tmp,tmpNum) then
    {$ELSE}
    if TryStrToInt(tmp,tmpNum) then
    {$ENDIF}
    begin
      Dec(tmpNum);
      ADest.CopyFrom(AIndex,AMap.DataMap,tmpNum);
    end
    else
      raise EBIException.Create('Error parsing number: '+tmp);
  end;

var t : TLoopInteger;
begin
  ILine:=0;
  IPos:=0;

  AMap.Stats;

  for t:=0 to ADest.Count-1 do
      ParseRawItem(t);
end;

procedure TRCommand.Start;
begin
  inherited;

  IScript.Clear;

  if TBIREngine.Output<>nil then
     TBIREngine.Output.Clear;

  IOutputs:=nil;
end;

procedure TRCommand.Statement(const AStatement: String);
begin
  IScript.Add(AStatement);
end;

end.
