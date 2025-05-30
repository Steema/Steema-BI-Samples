{*********************************************}
{  TeeBI Software Library                     }
{  XML data MSXML (Standard) driver           }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.XMLData.MSXML;

interface

// This unit contains support for the standard XML import and export
// driver in Microsoft Windows.

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

{$IFDEF MSWINDOWS}
uses
  ActiveX;
{$ENDIF}

Constructor TMsXML.Create(const AOwner:TComponent);
begin
  inherited;

  if AOwner=nil then
  begin
    IOwner:=TComponent.Create(nil);
    Doc:=TXmlDocument.Create(IOwner);
  end
  else
    Doc:=TXmlDocument.Create(AOwner);

  Doc.ParseOptions:=[]; //TParseOption.poPreserveWhiteSpace];
end;

Destructor TMsXML.Destroy;
begin
  Doc.Free;
  IOwner.Free;

  inherited;
end;

procedure TMsXML.FromString(const Text: String);
begin
  // Just in case we're being called from a Thread:
  {$IFDEF MSWINDOWS}
  CoInitialize(nil);
  try
    Doc.LoadFromXML(Text);
  finally
    CoUninitialize;
  end;
  {$ELSE}
  Doc.LoadFromXML(Text);
  {$ENDIF}

  Root;
end;

function TMsXml.IsValid:Boolean;
begin
  result:=Node<>nil;
end;

procedure TMsXml.Root;
begin
  Node:=Doc.DocumentElement;
  // BAD: Node.Normalize;
end;

function TMsXML.Attribute(const Index: Integer): String;
begin
  result:=Node.AttributeNodes[Index].Text;
end;

function TMsXML.AttributeName(const Index: Integer): String;
begin
  result:=Node.AttributeNodes[Index].NodeName;
end;

function TMsXml.AttributeCount:Integer;
begin
  result:=Node.AttributeNodes.Count;
end;

function TMsXml.Count:Integer;
begin
  result:=Node.ChildNodes.Count;
end;

function TMsXml.HasParent:Boolean;
begin
  result:=Node.ParentNode<>nil;
end;

procedure TMsXml.LoadFromFile(const Filename: String);
begin
  // Just in case we're being called from a Thread:
  {$IFDEF MSWINDOWS}
  CoInitialize(nil);
  try
    Doc.LoadFromFile(Filename);
  finally
    CoUninitialize;
  end;
  {$ELSE}
  Doc.LoadFromFile(Filename);
  {$ENDIF}

  Root;
end;

function TMsXML.Name: String;
begin
  result:=Node.NodeName;
end;

procedure TMsXml.Item(const Index: Integer);
begin
  Node:=Node.ChildNodes.Get(Index);
end;

procedure TMsXml.Parent;
begin
  Node:=Node.ParentNode;
end;

function TMsXML.Text:String;
begin
  result:=Node.Text;
end;

function TMsXML.HasText:Boolean;
begin
  result:=(Node.GetNodeType=ntText) or Node.IsTextElement;
end;

end.
