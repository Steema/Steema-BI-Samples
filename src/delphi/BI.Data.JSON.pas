{*********************************************}
{  TeeBI Software Library                     }
{  JSON data import                           }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.JSON;

interface

uses
  System.Classes, BI.Arrays, BI.Data, BI.DataSource, BI.Persist;

type
  EBIJSON=class(EBIException);

  TJSONEngine=class abstract
  protected
    procedure ArrayChild(const Index:Integer); virtual; abstract;
    function AsBoolean:Boolean; virtual; abstract;
    function AsDouble:Double; virtual; abstract;
    function AsString:String; virtual; abstract;
    function EnterArray:Integer; virtual; abstract;
    function EnterObject:Integer; virtual; abstract;
    function IsArray:Boolean; virtual; abstract;
    function IsBoolean:Boolean; virtual; abstract;
    function IsNull:Boolean; virtual; abstract;
    function IsNumber:Boolean; virtual; abstract;
    function IsObject:Boolean; virtual; abstract;
    function IsString:Boolean; virtual; abstract;
    function ObjectChild(const Index:Integer):String; virtual; abstract;
    procedure Parse(const Text:String); virtual; abstract;
    procedure Pop; virtual; abstract;
  end;

  TJSONEngineClass=class of TJSONEngine;

  TBIJSONFormat=(&Normal, &Array);

  TBIJSON=class(TBIHierarchicalSource)
  private
    JSON : TJSONEngine;

    procedure AppendArray(const AIndex:TInteger; const AData:TDataItem);
    procedure AppendTo(const AData:TDataItem);
    procedure CheckEngine;
    procedure DoAppend(const Index:TInteger; const Data:TDataItem);
  public
    class var
      EngineClass : TJSONEngineClass;

    Format : TBIJSONFormat;

    Constructor Create(const Definition:TDataDefinition=nil; const MultiThread:Boolean=False); override;

    Constructor CreateEngine(const AEngine:TJSONEngine);
    Destructor Destroy; override;

    class function FileFilter:TFileFilters; override;

    class function FromFile(const AFileName:String; const AFormat:TBIJSONFormat):TDataItem; overload;

    function Import(const Folder:String; Recursive:Boolean=False):TDataArray; overload;
    function Import(const Strings:TStrings):TDataArray; overload; override;
    function ImportFile(const FileName:String):TDataArray; override;

    function ImportText(const Text:String): TDataItem;

    class function Supports(const Extension:String):Boolean; override;
  end;

  TBIJSONExport=class(TBITextExport)
  private
    const
      Tab=#9;

    var
      IsFirst : Boolean;
      IItems : TStrings;

    procedure EmitDetail(const Data:TDataItem; const AIndex:TCursorIndex; const AItems:TStrings; const Indent:String);
    procedure EmitRow(const AIndex:TInteger; const Items:TDataArray; const AItems:TStrings; const Indent:String);
    procedure EmitRows(const AIndex:TInteger);
    function Escape(const S:String):String;
  protected
    procedure DoEmit(const AItems: TStrings); override;
  public
    Header : Boolean;

    class function AsString(const AData: TDataItem; const Header:Boolean): String; overload; static;
    class function FileFilter: TFileFilters; override;
    class function Supports(const Extension:String):Boolean; override;
  end;

implementation
