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
    procedure AddField(const AData:TDataItem; const AField:TField);
    procedure AddFields(const AData:TDataItem; const AFields:TFields);
    function DataFromADT(const AField:TField):TDataItem;
    procedure GuessFields(const ADataSet:TDataSet; const AData:TDataItem);
    procedure LoadData(const ADataSet:TDataSet; const AData:TDataItem);
  public
    class procedure AddItemFields(const AFields:TFieldDefs; const AItems:TDataArray); static;
    class procedure AddItemField(const AFields:TFieldDefs; const AData:TDataItem); static;

    class function FieldKind(const AFieldType:TFieldType):TDataKind; static;
    class function FieldOfData(const AData:TDataItem; const ADataSet:TDataSet):TField; static;

    class function FromDataSet(const ADataSet:TDataSet; const AName:String=''):TDataItem;
    class function FromField(const AField:TField; const AName:String=''):TDataItem;

    function Import(const AField:TField; const AName:String=''):TDataItem; overload;
    function Import(const ADataSet:TDataSet; const AName:String=''):TDataItem; overload;
    function Import(const AConnection:TCustomConnection):TDataArray; overload;
  end;

implementation
