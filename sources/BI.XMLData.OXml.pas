{*********************************************}
{  TeeBI Software Library                     }
{  OXml Driver                                }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.XMLData.OXml;


// This unit contains support for XML import and export using Google's OmniXML

// https://code.google.com/p/omnixml

(* USAGE:

uses
  BI.DataItem, BI.XMLData, BI.XMLData.Omni;

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
  System.Classes, BI.XMLData,

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

uses
  OXmlUtils;

{ TOXml }

Constructor TOXml.Create(const AOwner:TComponent);
begin
  inherited;

  Doc:=TXmlDocument.Create;

  Doc.WhiteSpaceHandling:=TXMLWhiteSpaceHandling.wsPreserveInTextOnly;
end;

Destructor TOXml.Destroy;
begin
  Doc.Free;
  inherited;
end;

procedure TOXml.FromString(const Text: String);
begin
  Doc.LoadFromXML(Text);
  Root;
end;

function TOXml.IsValid:Boolean;
begin
  result:=Node<>nil;
end;

procedure TOXml.Root;
begin
  Node:=Doc.DocumentElement;
end;

function TOXml.Attribute(const Index: Integer): String;
begin
  result:=Node.AttributeFromBegin[Index].Text;
end;

function TOXml.AttributeName(const Index: Integer): String;
begin
  result:=Node.AttributeFromBegin[Index].NodeName;
end;

function TOXml.AttributeCount:Integer;
begin
  result:=Node.AttributeCount;
end;

function TOXml.Count:Integer;
begin
  result:=Node.ChildNodes.Count;
end;

function TOXml.HasParent:Boolean;
begin
  result:=Node.ParentNode<>nil;
end;

procedure TOXml.LoadFromFile(const Filename: String);
begin
  Doc.LoadFromFile(Filename);
  Root;
end;

function TOXml.Name: String;
begin
  result:=Node.NodeName;
end;

procedure TOXml.Item(const Index: Integer);
begin
  Node:=Node.ChildNodes[Index];
end;

procedure TOXml.Parent;
begin
  Node:=Node.ParentNode;
end;

function TOXml.Text:String;
begin
  result:=Node.Text;
end;

function TOXml.HasText:Boolean;
begin
  result:=Node.IsTextElement
end;

end.
