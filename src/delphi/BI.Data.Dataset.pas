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
    class procedure Add(const AFields:TFieldDefs; const AItems:TDataArray); overload; static;
    class procedure Add(const AFields:TFieldDefs; const AData:TDataItem); overload; static;

    class function FieldKind(const AFieldType:TFieldType):TDataKind; static;
    class function FieldOf(const AData:TDataItem; const ADataSet:TDataSet):TField; static;

    class function From(const ADataSet:TDataSet; const AName:String=''):TDataItem; overload; static;
    class function From(const AField:TField; const AName:String=''):TDataItem; overload; static;
    class function From(const AFields:Array of TField; const AName:String=''):TDataItem; overload; static;
    class function From(const AConnection:TCustomConnection; const AName:String=''):TDataItem; overload; static;

    function Import(const AField:TField; const AName:String=''):TDataItem; overload;
    function Import(const ADataSet:TDataSet; const AName:String=''):TDataItem; overload;
    function Import(const AConnection:TCustomConnection):TDataArray; overload;
  end;

implementation
