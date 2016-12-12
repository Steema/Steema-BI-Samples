{*********************************************}
{  TeeBI Software Library                     }
{  SuperObject JSON Driver                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.JSON.SuperObject;

// https://github.com/hgourvest/superobject

(* USAGE:

uses
  BI.Data, BI.Data.JSON, BI.Data.JSON.SuperObject;

var J : TBIJSON;
    Data : TDataArray;

  J:=TBIJSON.CreateEngine(TSuperObject.Create);
  try
    Data:=J.ImportFile('sample.json');
  finally
    J.Free;
  end;
*)

interface

uses
  BI.Data.JSON,

  // Unit not found? Download SuperObject from: http://www.progdigy.com
  SuperObject;

type
  TSuperObjectJSON=class(TJSONEngine)
  private
    JSON : ISuperObject;
    Parents : Array of ISuperObject;

    procedure Push;
  protected
    procedure ArrayChild(const Index:Integer); override;
    function AsBoolean:Boolean; override;
    function AsDouble:Double; override;
    function AsString:String; override;
    function EnterArray:Integer; override;
    function EnterObject:Integer; override;
    function IsArray:Boolean; override;
    function IsBoolean:Boolean; override;
    function IsNull:Boolean; override;
    function IsNumber:Boolean; override;
    function IsObject:Boolean; override;
    function IsString:Boolean; override;
    function ObjectChild(const Index:Integer):String; override;
    procedure Parse(const Text:String); override;
    procedure Pop; override;
  public
    Destructor Destroy; override;
  end;

implementation
