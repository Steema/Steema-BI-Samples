{*********************************************}
{  TeeBI Software Library                     }
{  XML data MSXML (Standard) driver           }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.XMLData.MSXML;

interface

uses
  System.Classes, BI.XMLData, Xml.XmlDoc, Xml.XMLIntf;

type
  TMsXML=class(TXMLEngine)
  private
    Doc : TXmlDocument;
    Node : IXmlNode;

    IOwner : TComponent;
  public
    Constructor Create(const AOwner:TComponent); override;
    Destructor Destroy; override;

    function Attribute(const Index:Integer):String; override;
    function AttributeCount:Integer; override;
    function AttributeName(const Index: Integer): String; override;
    function Count:Integer; override;
    procedure FromString(const Text:String); override;
    function HasParent:Boolean; override;
    function HasText:Boolean; override;
    function IsValid:Boolean; override;
    procedure Item(const Index: Integer); override;
    procedure LoadFromFile(const Filename: String); override;
    function Name:String; override;
    procedure Parent; override;
    procedure Root; override;
    function Text:String; override;
  end;

implementation
