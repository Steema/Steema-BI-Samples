{*********************************************}
{  TeeBI Software Library                     }
{  FreePascal compiler support                }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.FPC;

interface

uses
  SysUtils, Classes, StreamEx;

type
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

  TStringArray=Array of String;

  TDirectory=record
  public
    class function Exists(const AFolder:String):Boolean; static;
    class function GetFiles(const APath,APattern:String):TStringArray; static;
    class function GetDirectories(const AFolder:String):TStringArray; static;
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
    Constructor Create(const AFileName:String);

    function EndOfStream:Boolean;
  end;

function IsQuoted(const S:String):Boolean;
function SplitString(const S,Delimiter:String):TStringArray;

implementation
