unit BI.Data.Process;

interface

uses
  BI.Data, WinAPI.PsAPI;

type
  TProcessList=class(TDataProvider)
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    class function PathOf(const ID:Cardinal):String; static;
    class function MemoryInfoOf(const ID:Cardinal; out Info:TProcessMemoryCounters):Boolean; static;
  end;

implementation

uses
  System.SysUtils, TlHelp32, WinAPI.Windows;

{ TProcessList }

class function TProcessList.MemoryInfoOf(const ID:Cardinal; out Info:TProcessMemoryCounters):Boolean;
begin
  result:=False;

  if ID>0 then
  begin
    Info.cb:=SizeOf(Info);

    if GetProcessMemoryInfo(OpenProcess(PROCESS_QUERY_INFORMATION,False,ID),@Info,SizeOf(Info)) then
       result:=True
    //else
       // RaiseLastOSError;
  end;
end;

class function TProcessList.PathOf(const ID:Cardinal):String;
var tmp : THandle;
    tmpMod : TModuleEntry32;
begin
  tmp:=CreateToolHelp32Snapshot(TH32CS_SNAPMODULE,ID);
  try
    tmpMod.dwSize:=SizeOf(tmpMod);

    if Module32First(tmp,tmpMod) then
       result:=tmpMod.szExePath
    else
       result:='?';
       //RaiseLastOSError;

  finally
    CloseHandle(tmp);
  end;
end;

procedure TProcessList.Load(const AData: TDataItem; const Children: Boolean);
var tmpName,
    tmpID,
    tmpParent,
    tmpThreads,
    tmpMemory,
    tmpPageFault,
    tmpPath : TDataItem;

    tmp : THandle;
    tmpProc : TProcessEntry32;
    tmpCount : Integer;

    Info : TProcessMemoryCounters;
begin
  AData.Clear;
  AData.AsTable:=True;

  tmpName:=AData.Items.Add('Name',TDataKind.dkText);
  tmpID:=AData.Items.Add('ID',TDataKind.dkInt64);
  tmpParent:=AData.Items.Add('Parent',TDataKind.dkInt64);
  tmpThreads:=AData.Items.Add('Threads',TDataKind.dkInt64);
  tmpMemory:=AData.Items.Add('Working Set',TDataKind.dkInt64);
  tmpPageFault:=AData.Items.Add('Page Faults',TDataKind.dkInt64);
  tmpPath:=AData.Items.Add('Path',TDataKind.dkText);

  tmpProc.dwSize:=SizeOf(tmpProc);

  tmp:=CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS,0);
  try
    tmpCount:=0;

    if Process32First(tmp,tmpProc) then
    repeat
      AData.Resize(tmpCount+1);

      tmpName.TextData[tmpCount]:=tmpProc.szExeFile;
      tmpID.Int64Data[tmpCount]:=tmpProc.th32ProcessID;
      tmpParent.Int64Data[tmpCount]:=tmpProc.th32ParentProcessID;
      tmpThreads.Int64Data[tmpCount]:=tmpProc.cntThreads;

      if MemoryInfoOf(tmpProc.th32ProcessID,Info) then
      begin
        tmpMemory.Int64Data[tmpCount]:=Info.WorkingSetSize;
        tmpPageFault.Int64Data[tmpCount]:=Info.PageFaultCount;
      end
      else
      begin
        tmpMemory.Missing[tmpCount]:=True;
        tmpPageFault.Missing[tmpCount]:=True;
      end;

      tmpPath.TextData[tmpCount]:=PathOf(tmpProc.th32ProcessID);

      Inc(tmpCount);

    until not Process32Next(tmp,tmpProc)
    else
      RaiseLastOSError;
  finally
    CloseHandle(tmp);
  end;
end;

end.
