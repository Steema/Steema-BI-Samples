{*********************************************}
{  TeeBI Software Library                     }
{  Microsoft Excel data import and export     }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Excel;

interface

uses
  System.Classes, System.SysUtils,
  {$IFDEF MSWINDOWS}
  WinAPI.ActiveX, System.Win.ComObj,
  {$ENDIF}
  BI.DataSource, BI.DataItem, BI.Arrays, BI.Persist;

type
  EExcelException=class(EBIException);

  TBIExcel=class(TBITextSource)
  private
    FHeaderCount : Integer;
  protected
    function DoImportFile(const FileName:String):TDataArray; override;
  public
    Range     : String;
    WorkSheet : String;

    Constructor Create(const Definition:TDataDefinition=nil; const MultiThread:Boolean=False); override;

    class function FileFilter: TFileFilters; override;
    function Import(const Folder:String; Recursive:Boolean=False):TDataArray; overload;

    class function Supports(const Extension: String): Boolean; override;

    property HeaderCount:Integer read FHeaderCount write FHeaderCount default 1;
  end;

  TBIExcelEngineClass=class of TBIExcelEngine;

  TBIExcelEngine=class
  public
    class procedure SaveToFile(const AData:TDataItem; const AFileName:String); virtual;
  end;

  TBIExcelExport=class(TBIExport)
  public
    class
      var Engine : TBIExcelEngineClass;

    Constructor Create; override;

    class function FileFilter: TFileFilters; override;

    procedure SaveToFile(const AFileName:String); override;
    class function Supports(const Extension: String): Boolean; override;
  end;

implementation
