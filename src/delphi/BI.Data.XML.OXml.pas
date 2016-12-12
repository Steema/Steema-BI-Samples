{*********************************************}
{  TeeBI Software Library                     }
{  OXml Driver                                }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.XML.OXml;


// https://code.google.com/p/omnixml

(* USAGE:

uses
  BI.Data, BI.Data.XML, BI.Data.XML.Omni;

var X : TBIXML; Data : TDataArray;

  X:=TBIXML.CreateEngine(TOmniXML.Create(Self));  // <-- ( Self or nil )
  try
    Data:=X.ImportFile('sample.xml');
  finally
    X.Free;
  end;
*)

interface


uses
  System.Classes, BI.Data.XML,

  // Unit not found? Download OXml from: http://www.kluug.net
  OXmlPDOM;

type
  TOXml=class(TXMLEngine)
  private
    Doc : TXmlDocument;
    Node : PXmlNode;
  public
    function Attribute(const Index:Integer):String; override;
    function AttributeCount:Integer; override;
    function AttributeName(const Index: Integer): String; override;
    function Count:Integer; override;
    procedure FromString(const Text:String); override;
    function HasParent:Boolean; override;
    function HasText:Boolean; override;
    procedure Item(const Index: Integer); override;
    function IsValid:Boolean; override;
    procedure LoadFromFile(const Filename: String); override;
    function Name:String; override;
    procedure Parent; override;
    procedure Root; override;
    function Text:String; override;
  public
    Constructor Create(const AOwner:TComponent); override;
    Destructor Destroy; override;
  end;

implementation
