{*********************************************}
{  TeeBI Software Library                     }
{  JSON data Standard (FPC) driver            }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.JSON.FPC;

interface

uses
  fpJSON,
  BI.JSON;

type
  TFPCJSON=class(TJSONEngine)
  private
    JSON : TJSONData;
    Parents : Array of TJSONData;

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
