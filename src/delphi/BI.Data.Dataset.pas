{*********************************************}
{  TeeBI Software Library                     }
{  TDataSet data import                       }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.Dataset;

interface

uses
  System.Classes, System.SysUtils, Data.DB, BI.Arrays, BI.Data, BI.DataSource;

type
  TBIDataSetSource=class(TBIFileSource)
  private
    procedure GuessFields(const DataSet:TDataSet; const Data:TDataItem);
    procedure LoadData(const DataSet:TDataSet; const Data:TDataItem);
  public
    class procedure AddItemFields(const Fields:TFieldDefs; const AItems:TDataArray); static;
    class procedure AddItemField(const Fields:TFieldDefs; const AData:TDataItem); static;

    class function FieldKind(const FieldType:TFieldType):TDataKind; static;
    class function FieldOfData(const AData:TDataItem; const ADataSet:TDataSet):TField; static;

    class function FromDataSet(const ADataSet:TDataSet; const AName:String=''):TDataItem;

    function Import(const DataSet:TDataSet; const Name:String=''):TDataItem; overload;
    function Import(const Connection:TCustomConnection):TDataArray; overload;
  end;

implementation
