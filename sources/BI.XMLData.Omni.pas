{*********************************************}
{  TeeBI Software Library                     }
{  Omni XML Driver                            }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.XMLData.Omni;

// This unit supports OmniXML library, for these two versions:

// Current version:
// http://www.kluug.net/omnixml.php

// Previous version:
// https://code.google.com/p/omnixml

(* USAGE:

uses
  BI.DataItem, BI.XMLData, BI.XMLData.Omni;

var X : TBIXML;
    Data : TDataArray;

  X:=TBIXML.CreateEngine(TOmniXML.Create);
  try
    Data:=X.ImportFile('sample.xml');
  finally
    X.Free;
  end;
*)

interface

uses
  System.Classes, BI.XMLData,

  // Unit not found? Download OmniXML from:
  // http://omnixml.com
  // http://www.MihaRemec.com

  OmniXML;

type
  TOmniXML=class(TXMLEngine)
  private
    Doc : TXmlDocument;
    Node : IXmlNode;
  public
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
  public
    Constructor Create(const AOwner:TComponent); override;
    Destructor Destroy; override;
  end;

implementation

Constructor TOmniXML.Create(const AOwner:TComponent);
begin
  inherited;

  Doc:=TXmlDocument.Create;

  {$IF Declared(TXMLWhiteSpaceHandling)}
  Doc.WhiteSpaceHandling:=TXMLWhiteSpaceHandling.wsPreserveAll;
  {$ELSE}
  Doc.PreserveWhiteSpace:=True;
  {$ENDIF}
end;

Destructor TOmniXML.Destroy;
begin
  Doc.Free;
  inherited;
end;

procedure TOmniXML.FromString(const Text: String);
begin
  Doc.LoadXML(Text);
  Root;
end;

function TOmniXML.IsValid:Boolean;
begin
  result:=Node<>nil;
end;

procedure TOmniXML.Root;
begin
  Node:=Doc.DocumentElement;
end;

function TOmniXML.Attribute(const Index: Integer): String;
begin
  result:=Node.Attributes.Item[Index].Text;
end;

function TOmniXML.AttributeName(const Index: Integer): String;
begin
  result:=Node.Attributes.Item[Index].NodeName;
end;

function TOmniXML.AttributeCount:Integer;
begin
  result:=Node.Attributes.Length;
end;

function TOmniXML.Count:Integer;
begin
  result:=Node.ChildNodes.Length;
end;

function TOmniXML.HasParent:Boolean;
begin
  result:=Node.ParentNode<>nil;
end;

procedure TOmniXML.LoadFromFile(const Filename: String);
begin
  Doc.Load(Filename);
  Root;
end;

function TOmniXML.Name: String;
begin
  result:=Node.NodeName;
end;

procedure TOmniXML.Item(const Index: Integer);
begin
  Node:=Node.ChildNodes.Item[Index];
end;

procedure TOmniXML.Parent;
begin
  Node:=Node.ParentNode;
end;

function TOmniXML.Text:String;
begin
  result:=Node.Text;
end;

function TOmniXML.HasText:Boolean;
begin
  result:=(Node.NodeType=ELEMENT_NODE) and (Node.ChildNodes.Length=1) and
          (Node.ChildNodes.Item[0].NodeType=TEXT_NODE);
end;

end.
