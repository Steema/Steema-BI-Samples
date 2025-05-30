{*********************************************}
{  TeeBI Software Library                     }
{  XML data FPC  driver                       }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.XMLData.FPC;

interface

// This unit contains support for the standard XML import and export
// code in FreePascal compiler and Lazarus RTL.

(* USAGE:

uses
  BI.DataItem, BI.XMLData, BI.XMLData.FPC;

var X : TBIXML; Data : TDataArray;

  X:=TBIXML.CreateEngine(TFPCXML.Create(Self));  // <-- ( Self or nil )
  try
    Data:=X.ImportFile('sample.xml');
  finally
    X.Free;
  end;
*)

uses
  System.Classes, BI.XMLData, XMLRead, DOM;

type
  TFPCXML=class(TXMLEngine)
  private
    Doc : TXmlDocument;
    Node : TDOMNode;

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

    class function Normalize(const S:String):String;
  end;

implementation

uses
  XmlUtils;

{ TFPCXML }

Constructor TFPCXML.Create(const AOwner:TComponent);
begin
  inherited;
end;

Destructor TFPCXML.Destroy;
begin
  Doc.Free;
  IOwner.Free;

  inherited;
end;

procedure TFPCXML.FromString(const Text: String);
var S : TStringStream;
begin
  S:=TStringStream.Create(Text);
  try
    ReadXMLFile(Doc,S);
    Root;
  finally
    S.Free;
  end;
end;

function TFPCXML.IsValid:Boolean;
begin
  result:=Node<>nil;
end;

procedure TFPCXML.Root;
begin
  Node:=Doc.DocumentElement;
  // BAD: Node.Normalize;
end;

function TFPCXML.Attribute(const Index: Integer): String;
begin
  result:=Node.Attributes[Index].NodeValue;
end;

function TFPCXML.AttributeName(const Index: Integer): String;
begin
  result:=Node.Attributes[Index].NodeName;
end;

function TFPCXML.AttributeCount:Integer;
begin
  result:=Node.Attributes.Length;
end;

function TFPCXML.Count:Integer;
begin
  result:=Node.ChildNodes.Count;
end;

function TFPCXML.HasParent:Boolean;
begin
  result:=Node.ParentNode<>nil;
end;

procedure TFPCXML.LoadFromFile(const Filename: String);
begin
  ReadXMLFile(Doc,FileName);
  Root;
end;

function TFPCXML.Name: String;
begin
  result:=Node.NodeName;
end;

class function TFPCXML.Normalize(const S: String): String;
var tmp : WideString;
begin
  tmp:=S;
  NormalizeSpaces(tmp);
  result:=tmp;
end;

procedure TFPCXML.Item(const Index: Integer);
begin
  Node:=Node.ChildNodes.Item[Index];
end;

procedure TFPCXML.Parent;
begin
  Node:=Node.ParentNode;
end;

function TFPCXML.Text:String;
begin
  result:=Node.NodeValue;
end;

function TFPCXML.HasText:Boolean;

  function IsTextElement(const ANode:TDOMNode):Boolean;
  begin
    result:= (ANode.NodeType = ELEMENT_NODE) and
             (ANode.ChildNodes.Length = 1) and
             (ANode.ChildNodes[0].NodeType = TEXT_NODE);
  end;

begin
  result:=IsTextElement(Node);
end;

end.
