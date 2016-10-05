{*********************************************}
{  TeeBI Software Library                     }
{  CSV (TSV) data import and export           }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.CSV;

interface

uses
  System.Classes, System.Types,
  {$IFDEF FPC}
  BI_FPC,
  {$ELSE}
  System.Diagnostics,
  {$ENDIF}
  System.SysUtils, BI.Data, BI.DataSource, BI.Arrays, BI.Arrays.Strings,
  BI.Persist;

type
  EBICSV=class(EBIException);

  TTextHeaders=(Auto,Yes,No);

  TCSVHeader=record
  private
    Detected : Boolean;
    HasHeader : TTextHeaders;
  public
    Count : Integer;
    Headers : TTextHeaders;
  end;

  TBICSV=class(TBITextSource)
  private
    DeltaCapacity : Integer;

    tmpCount,
    tmpCurrent : TInteger;

    PendingGuess : Boolean;

    Data : TDataItem;
    Start : TInteger;

    FItems : TDataItems;

    t1 : TStopwatch;

    class function BISplitString(const AString:String; const ADelimiter,AQuote:Char):TStringArray;
    function Dequoted(const S:String):String;
    procedure GuessDataKinds(const AData:TDataItem; const Values:TStringArray);
    function GuessDelimiter(const AText:String):TStringArray;
    function GuessHeaders(const AString:String):Boolean;

    function FinishRowByRow(const ACount:TInteger):TDataArray;
    procedure InitRowByRow;
    procedure RowByRow(const AString:String; var ARow:TInteger);
  protected
    function InternalImportFile(const FileName:String):TDataArray; override;
  public
  const
    AutomaticDelimiter=#0;

  var
    Delimiter : Char;
    Quote : Char;

    Header : TCSVHeader;

    Constructor Create(const Definition:TDataDefinition=nil; const MultiThread:Boolean=False); override;

    class function FileFilter:TFileFilters; override;

    function Import(const Folder:String; Recursive:Boolean=False):TDataArray; overload;
    function Import(const Strings:TStrings):TDataArray; override;
    function ImportFile(const AFileName:String):TDataArray; override;
    function ImportText(const AText:String):TDataItem;

    class function Supports(const Extension:String):Boolean; override;
  end;

  TBICSVExport=class(TBITextExport)
  private
    {$IFDEF FPC}
    IItems : TStrings;
    procedure FpcAddRows(const AIndex:TInteger);
    {$ENDIF}

    function Headers(const Prefix:String; const AItems:TDataArray):String;
    function Quoted(const S:String):String; inline;
    function Row(const AIndex:TInteger; const AItems:TDataArray):String;
  protected
    procedure DoEmit(const AItems: TStrings); override;
  public
    Delimiter : Char;
    Header : Boolean;
    Quote : Char;

    Constructor Create; override;

    class function Supports(const Extension:String):Boolean; override;

    class function FileFilter:TFileFilters; override;
  end;

implementation
