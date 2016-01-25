{*********************************************}
{  TeeBI Software Library                     }
{  JSON data Standard (Delphi) driver         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.JSON.Standard;

interface

{$IF CompilerVersion>21}
{$DEFINE D14}
{$ENDIF}

{$IF CompilerVersion>24}
{$DEFINE D18}
{$ENDIF}

{$IF CompilerVersion>26}
{$DEFINE D15}
{$DEFINE D20}
{$ENDIF}

{$IF CompilerVersion>29}
{$DEFINE HASBOOL}
{$ENDIF}

{$IFDEF D14}
 {$DEFINE HASJSON}
{$ELSE}
{$IFDEF D7}
 {$IFDEF HASLKJSON}
  {$DEFINE HASJSON}
 {$ENDIF}
{$ENDIF}
{$ENDIF}

uses
  {$IFDEF HASJSON}
   {$IFDEF D20}
   System.JSON,
   {$ELSE}
   {$IFDEF D14}
   DBXJSON,
   {$ELSE}
   uLkJSON,
   {$ENDIF}
   {$ENDIF}
  {$ENDIF}

  BI.Data.JSON;

type
  TStandardJSON=class(TJSONEngine)
  private
    JSON : TJSONValue;
    Parents : Array of TJSONValue;

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
