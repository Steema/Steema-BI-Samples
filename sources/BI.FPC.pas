{*********************************************}
{  TeeBI Software Library                     }
{  FreePascal compiler support                }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FPC;

interface

uses
  SysUtils, Classes, Graphics, StreamEx, BI.Arrays.Strings;

type
  TProc<T>=procedure(const Value:T);
  TProc<T,V>=procedure(const Value1:T; const Value2:V);

  TObjectProc<T>=procedure({const} Value:T) of object;

  TStopWatch=record
  private
    Time : Int64;
  public
    function ElapsedMilliseconds:Int64;
    class function StartNew:TStopWatch; static;
  end;

  TPath=record
  public
    class function ChangeExtension(const AFileName,AExtension:String):String; static;
    class function Combine(const Path1,Path2:String):String; inline; static;
    class function GetDirectoryName(const AFileName:String):String; static;
    class function GetExtension(const AFileName:String):String; static;
    class function GetFileName(const AFileName:String):String; static;
    class function GetFileNameWithoutExtension(const AFileName:String):String; static;
    class function GetHomePath:String; static;
    class function GetTempPath:String; static;
    class function IsRelativePath(const APath:String):Boolean; static;
  end;

  TFile=record
  public
    class function Delete(const AFileName:String):Boolean; static;
    class function Exists(const AFileName:String):Boolean; static;
    class function GetLastWriteTime(const AFileName:String):TDateTime; static;
    class function Move(const AFileName,ANew:String):Boolean; static;
    class function Size(const AFileName:String):Int64; static;
  end;

  TDirectory=record
  public
    class function Exists(const AFolder:String):Boolean; static;
    class function GetFiles(const APath,APattern:String):TStringArray; static;
    class function GetDirectories(const AFolder:String):TStringArray; static;
    class procedure Move(const AFromDir,AToDir:String); static; static;
  end;

  TZipMode=(zmRead, zmWrite);

  TZipFile=class
  private
    IStream : TStream;

    procedure DoOpenStream(Sender: TObject; var AStream: TStream);
    function Get(const Index:Integer):String;
  public
    procedure Add(const AStream:TStream; const AName:String);
    procedure Close;
    procedure Extract(const AIndex:Integer; const Path:String=''; CreateSubDirs:Boolean=True);
    function FileCount:Integer;
    class function IsValid(const ZipFileName: string): Boolean; static;
    procedure Open(const AStream:TStream; const Mode:TZipMode); overload;
    procedure Open(const AFileName:String; const Mode:TZipMode); overload;
    procedure Read(const AName:String; out ABytes:TBytes);

    property FileName[Index:Integer]:String read Get;
  end;

  TColor=type Cardinal;

  TStreamReader=class(StreamEx.TStreamReader)
  public
    Constructor CreateFile(const AFileName:String);

    function EndOfStream:Boolean;
  end;

  TAlphaColor=TColor;
  //TAlphaColor=type Cardinal;

  TColorRec = packed record
    A, B, G, R: Byte;
  end;

  TAlphaColorRec=packed record
  public
    A, B, G, R: Byte;

  const
    Alpha  = 0;
    Green  = $8000;
    Red    = $FF;
    Yellow = $FFFF;
    Null   = $00000000;
  end;

  TAlphaColors=TAlphaColorRec;

function IsQuoted(const S:String):Boolean;

implementation

uses
  //StrUtils,
  Masks, LazFileUtils, zipper;

function TStopWatch.ElapsedMilliseconds:Int64;
begin
  result:=GetTickCount64-Time;
end;

class function TStopWatch.StartNew:TStopWatch;
begin
  result.Time:=GetTickCount64;
end;

{ TPath }

class function TPath.ChangeExtension(const AFileName,AExtension:String):String;
begin
  result:=ChangeFileExt(AFileName,AExtension);
end;

class function TPath.Combine(const Path1,Path2:String):String;
begin
  result:=ConcatPaths([Path1, Path2]);
end;

class function TPath.GetDirectoryName(const AFileName:String):String;
begin
  result:=ExtractFileDir(AFileName);
end;

class function TPath.GetExtension(const AFileName:String):String;
begin
  result:=ExtractFileExt(AFileName);
end;

class function TPath.GetFileName(const AFileName: String): String;
begin
  result:=ExtractFileName(AFileName);
end;

class function TPath.GetFileNameWithoutExtension(const AFileName:String):String;
var Ext: String;
begin
  result:=AFileName;

  Ext:=GetExtension(result);

  if Ext<>'' then
     Delete(result,Length(result)-Length(Ext)+1,Length(Ext));
end;

class function TPath.GetHomePath: String;
begin
  result:=GetUserDir;
end;

class function TPath.GetTempPath: String;
begin
  result:=GetTempDir(False);
end;

class function TPath.IsRelativePath(const APath: String): Boolean;
begin
  result:=SameText(ExtractRelativePath(APath,GetCurrentDir),ExtractFileDir(APath));
end;

{ TFile }

class function TFile.Delete(const AFileName:String):Boolean;
begin
  result:=SysUtils.DeleteFile(AFileName);
end;

class function TFile.Exists(const AFileName:String):Boolean;
begin
  result:=FileExists(AFileName);
end;

class function TFile.GetLastWriteTime(const AFileName:String):TDateTime;
begin
  result:=FileDateToDateTime(FileAge(AFileName));
end;

class function TFile.Move(const AFileName,ANew:String):Boolean;
begin
  result:=RenameFile(AFileName,ANew);
end;

class function TFile.Size(const AFileName: String): Int64;
begin
  result:=FileSizeUtf8(AFileName);
end;

{ TZipFile }

procedure TZipFile.Add(const AStream:TStream; const AName:String);
begin
end;

procedure TZipFile.Close;
begin
end;

procedure TZipFile.DoOpenStream(Sender: TObject; var AStream: TStream);
begin
  AStream:=IStream;
end;

procedure TZipFile.Extract(const AIndex:Integer; const Path: string; CreateSubDirs: Boolean);
begin
//  Extract(IndexOf(FileName), Path, CreateSubdirs);
end;

function TZipFile.FileCount: Integer;
begin
  result:=0;
end;

function TZipFile.Get(const Index: Integer): String;
begin
  result:='';
end;

class function TZipFile.IsValid(const ZipFileName: string): Boolean;
begin
  result:=True;
end;

procedure TZipFile.Open(const AFileName: String; const Mode: TZipMode);
begin

end;

procedure TZipFile.Open(const AStream:TStream; const Mode:TZipMode);
begin
  IStream:=AStream;
end;

procedure TZipFile.Read(const AName: String; out ABytes: TBytes);
var Zip : TUnZipper;
begin
  Zip:=TUnZipper.Create;
  try
    Zip.OnOpenInputStream:=DoOpenStream;
    Zip.Examine;
  finally
    Zip.Free;
  end;
end;

{ TDirectory }

class function TDirectory.Exists(const AFolder:String):Boolean;
begin
  result:=TFile.Exists(AFolder);
end;

class procedure TDirectory.Move(const AFromDir,AToDir:String);
begin
  RenameFile(AFromDir,AToDir);
end;

class function TDirectory.GetFiles(const APath, APattern: String): TStringArray;
var tmp : TSearchRec;
    L : Integer;
    Mask: TMask;
begin
  result:=nil;

  if FindFirstUTF8(APath+'\*.*',faAnyFile,tmp)=0 then
  begin
    Mask:=TMask.Create(APattern);
    try
      repeat
        if (tmp.Attr and FaDirectory) <> FaDirectory then
        begin
          if Mask.Matches(tmp.Name) then
          begin
            L:=Length(result);
            SetLength(result,L+1);
            result[L]:=tmp.Name;
          end;
        end;
      until FindNextUTF8(tmp)<>0;
    finally
      Mask.Free;
    end;

    FindCloseUTF8(tmp);
  end;
end;

class function TDirectory.GetDirectories(const AFolder:String):TStringArray;
var tmp : TSearchRec;
    L : Integer;
begin
  result:=nil;

  if FindFirstUTF8(AFolder+'\*.*',faAnyFile,tmp)=0 then
  begin
    repeat
      if (tmp.Attr and FaDirectory) = FaDirectory then
      begin
        if (tmp.Name<>'.') and (tmp.Name<>'..') then
        begin
          L:=Length(result);
          SetLength(result,L+1);
          result[L]:=tmp.Name;
        end;
      end;
    until FindNextUTF8(tmp)<>0;

    FindCloseUTF8(tmp);
  end;
end;

function IsQuoted(const S:String):Boolean;
begin
  result:=(Copy(S,1,1)='"') and (Copy(S,Length(S),1)='"') or
          (Copy(S,1,1)='''') and (Copy(S,Length(S),1)='''');
end;

{ TStreamReader }

Constructor TStreamReader.CreateFile(const AFileName:String);
begin
  inherited Create(TFileStream.Create(AFileName,fmOpenRead+fmShareDenyWrite));
end;

function TStreamReader.EndOfStream:Boolean;
begin
  result:=IsEof;
end;

end.
