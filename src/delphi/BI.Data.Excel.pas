{*********************************************}
{  TeeBI Software Library                     }
{  Microsoft Excel data import and export     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.Excel;

interface

uses
  System.Classes, System.SysUtils,
  {$IFDEF MSWINDOWS}
  WinAPI.ActiveX, System.Win.ComObj,
  {$ENDIF}
  BI.DataSource, BI.Data, BI.Arrays, BI.Persist;

type
  EExcelException=class(EBIException);

  TBIExcel=class(TBITextSource)
  protected
    function DoImportFile(const FileName:String):TDataArray; override;
  public
    HeaderCount : Integer;
    Range     : String;
    WorkSheet : String;

    Constructor Create(const Definition:TDataDefinition=nil; const MultiThread:Boolean=False); override;

    class function ExportFormat:TBIExport; override;
    function Import(const Folder:String; Recursive:Boolean=False):TDataArray; overload;

    class function Supports(const Extension: String): Boolean; override;
  end;

  TBIExcelEngineClass=class of TBIExcelEngine;

  TBIExcelEngine=class
  public
    class procedure SaveToFile(const AData:TDataItem; const AFileName:String); virtual;
  end;

  TBIExcelExport=class(TBIExport)
  protected
    procedure DoEmit(const AItems: TStrings); override;
  public
    class var Engine : TBIExcelEngineClass;

    class procedure SaveToFile(const AData:TDataItem; const AFileName:String); static;
  end;

implementation
