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
  System.SysUtils, BI.Data, BI.DataSource, BI.Arrays, BI.Persist;

type
  EBICSV=class(EBIException);

  TTextHeaders=(Auto,Yes,No);

  TCSVHeader=record
  private
    HasHeader : TTextHeaders;
  public
    Count : Integer;
    Detected : Boolean;
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

    t1 : TStopwatch;

    class function BISplitString(const AString:String; const ADelimiter,AQuote:Char):TStringDynArray;
    function Dequoted(const S:String):String;
    procedure GuessDataKinds(const AData:TDataItem; const Values:TStringDynArray {TArray<String>});
    function GuessDelimiter(const AText:String):TStringDynArray;
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
    Delimiter,
    Quote : Char;

    Header : TCSVHeader;

    Constructor Create(const Definition:TDataDefinition=nil; const MultiThread:Boolean=False); override;

    class function ExportFormat:TBIExport; override;

    function Import(const Folder:String; Recursive:Boolean=False):TDataArray; overload;
    function Import(const Strings:TStrings):TDataArray; override;

    class function Supports(const Extension:String):Boolean; override;
  end;

  TBICSVExport=class(TBIExport)
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
    Delimiter,
    Quote : Char;

    Constructor Create; override;
  end;

implementation
