{*********************************************}
{  TeeBI Software Library                     }
{  TClientDataSet data import and export      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.ClientDataset;

interface

uses
  System.Classes, System.SysUtils, Data.DB, DBClient,
  BI.Data.Dataset, BI.Data, BI.Summary, BI.Persist, BI.Expression,
  BI.DataSource;

type
  TBIClientDataset=class(TBIDatasetSource)
  private
    class procedure DoFillData(const DataSet: TClientDataSet;
                               const AData: TDataItem;
                               const ADataArray: TDataArray;
                               const Filter:TExpression=nil); static;
  protected
    function DoImportFile(const FileName:String):TDataArray; override;
  public
    class function FileFilter:TFileFilters; override;

    class procedure FillData(const DataSet:TClientDataSet; const AData:TDataItem); overload; static;
    class procedure FillData(const DataSet: TClientDataSet; const AData: TDataArray); overload; static;
    class procedure FillData(const DataSet:TClientDataSet; const AData:TDataItem; const Filter:TExpression=nil); overload; static;

    function Import(const Folder:String; Recursive:Boolean):TDataArray; overload;
    class function Supports(const Extension:String):Boolean; override;
  end;

  TClientDatasetExport=class(TBIExport)
  public
    Format : TDataPacketFormat;

    Constructor Create; override;

    class function FileFilter: TFileFilters; override;
    procedure SaveToFile(const AFileName:String); override;
  end;

implementation
