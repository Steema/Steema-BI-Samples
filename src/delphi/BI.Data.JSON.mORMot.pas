{*********************************************}
{  TeeBI Software Library                     }
{  mORMot JSON Driver                         }
{  http://www.synopse.info                    }
{                                             }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.JSON.mORMot;

(*
  WARNING !!
  Unfinished code
*)

interface

// This unit implements an "engine" class that can be used to
// import JSON content to a TDataItem using Synpose mORMot JSON
// parser.

// Download mORMot framework from:
// http://www.synopse.info

{
Usage:

uses
  BI.Data, BI.Data.JSON, BI.Data.JSON.mORMot,
  BI.DataSource;

var Data : TDataItem;
    tmp : TBIJSON;
begin
  tmp:=TBIJSON.CreateEngine(TmORMotJSONEngine.Create);
  try
    Data:=TBISource.FromData(tmp.ImportFile('Sample.json'));

    BIGrid1.Data:=Data;
  finally
    tmp.Free;
  end;
}

uses
  BI.Data.JSON, SynCommons;

type
  TmORMotJSONEngine=class(TJSONEngine)
  private
    JSON : PUTF8Char;
    Parents : Array of PUTF8Char;

    EndOfObject : PUTF8Char;

    WasString : Boolean;

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
  end;

implementation
