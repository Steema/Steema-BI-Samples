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
  System.Classes, BI.Arrays, BI.Data, BI.Plugins.R;

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

    procedure AddVariable(const AName:String; const Index:TInt64Array;
                          const ADatas:TDataArray; const UseMissing:Boolean=True); override;

    procedure GetVariable(const AName:String; const AData:TDataItem); override;
    procedure LoadPackage(const APackage:String); override;
    procedure ParseOutput(const ADest:TDataItem); override;
    procedure ParseRawMap(const AMap,ADest:TDataItem); override;

    procedure Start; override;
    procedure Statement(const AStatement:String); override;
    function Version:String;
  end;

implementation
