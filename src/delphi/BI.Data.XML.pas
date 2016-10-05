{*********************************************}
{  TeeBI Software Library                     }
{  XML data import                            }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.XML;

interface

uses
  System.Classes,
  BI.Arrays, BI.Data, BI.DataSource;

type
  EBIXML=class(EBIException);

  TXmlEngine=class abstract
  public
    Constructor Create(const AOwner:TComponent); virtual; abstract;

    function Attribute(const Index:Integer):String; virtual; abstract;
    function AttributeCount:Integer; virtual; abstract;
    function AttributeName(const Index: Integer):String; virtual; abstract;
    function Count:Integer; virtual; abstract;
    procedure FromString(const Text:String); virtual; abstract;
    function GetAttribute(const AName:String):String;
    function HasParent:Boolean; virtual; abstract;
    function HasText:Boolean; virtual; abstract;
    function IsValid:Boolean; virtual; abstract;
    procedure Item(const Index: Integer); virtual; abstract;
    procedure LoadFromFile(const Filename: String); virtual; abstract;
    function Name:String; virtual; abstract;
    procedure Parent; virtual; abstract;
    procedure Root; virtual; abstract;
    function Text:String; virtual; abstract;
  end;

  TBIXML=class(TBIHierarchicalSource)
  private
    FExclude : TTextArray;

    procedure DoAppend(const Data:TDataItem; const XML:TXmlEngine; const CallProgress:Boolean);
  protected
    XML : TXmlEngine;
  public
    Constructor CreateEngine(const AEngine:TXmlEngine);
    Destructor Destroy; override;

    class function FileFilter: TFileFilters; override;
    function Import(const Folder:String; Recursive:Boolean=False):TDataArray; overload;
    function Import(const Strings:TStrings):TDataArray; overload; override;

    function ImportText(const Text:String): TDataItem;

    function Parse(const Text:String): Boolean;
    class function Supports(const Extension:String):Boolean; override;

    property ExcludeNodes:TTextArray read FExclude;
  end;

  TBIXMLEmit=(Header,Simple);

  TBIXMLExport=class(TBITextExport)
  private
    const
       Tab=#9;

    var
      IItems : TStrings;

    procedure EmitDetail(const Data:TDataItem; const AIndex:TCursorIndex; const Items:TStrings; const Indent:String);
    procedure EmitRow(const AIndex:TInteger; const AData:TDataArray; const Items:TStrings; const Indent:String);
    procedure EmitRows(const AIndex:TInteger);
  protected
    procedure DoEmit(const AItems: TStrings); override;
  public
    Emit : TBIXMLEmit;

    class function FileFilter: TFileFilters; override;
    class function Supports(const Extension:String):Boolean; override;
  end;

implementation
