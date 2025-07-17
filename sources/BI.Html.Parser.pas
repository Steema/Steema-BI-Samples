{*********************************************}
{  TeeBI Software Library                     }
{  HTML data import and export                }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Html.Parser;

interface

// This unit implements an "html engine" capable of parsing HTML text,
// using the sandbil HTML-Parser available at GitHub:

{
  https://github.com/sandbil/HTML-Parser

  HTML-Parser is licensed with a MIT license
}

{$IFDEF FPC}
// PENDING TO USE A FREEPASCAL HTML PARSER
uses
  BI.HTML;

type
  THTMLParserEngine=class(THTMLEngine)
  end;

implementation
{$ELSE}

uses
  BI.HTML,
  parser;  // <-- HTML-Parser unit

type
  THTMLParserEngine=class(THTMLEngine)
  public
    DOM : TDOMTree;
    Node : TDomTreeNode;

    Constructor CreateParser; override;
    Destructor Destroy; override;

    function Attribute(const Index:Integer):String; override;
    function AttributeCount:Integer; override;
    function AttributeName(const Index: Integer):String; override;
    function Count:Integer; override;
    procedure FromString(const Text:String); override;
    function GetAttribute(const AName:String):String;
    function HasParent:Boolean; override;
    function HasText:Boolean; override;
    function IsValid:Boolean; override;
    procedure Item(const Index: Integer); override;
    procedure LoadFromFile(const Filename: String); override;
    function Name:String; override;
    procedure Parent; override;
    function Parse(const Text:String):Boolean; override;
    procedure Root; override;
    function Text:String; override;
  end;

implementation

{ THTMLParserEngine }

Constructor THTMLParserEngine.CreateParser;
begin
  // Do not call inherited;

  DOM:=TDomTree.Create;
  Root;
end;

Destructor THTMLParserEngine.Destroy;
begin
  DOM.Free;
  inherited;
end;

function THTMLParserEngine.Attribute(const Index: Integer): String;
begin
  result:=''; // TODO: Node.Attributes[Index].Text;
end;

function THTMLParserEngine.AttributeCount: Integer;
begin
  result:=Node.Attributes.Count;
end;

function THTMLParserEngine.AttributeName(const Index: Integer): String;
begin
  result:=''; // TODO: Node.Attributes[Index]
end;

function THTMLParserEngine.Count: Integer;
begin
  result:=Node.Child.Count;
end;

procedure THTMLParserEngine.FromString(const Text: String);
begin
  Node.RunParse(Text);
end;

function THTMLParserEngine.GetAttribute(const AName: String): String;
begin
  result:=Node.GetAttrValue(AName);
end;

function THTMLParserEngine.HasParent: Boolean;
begin
  result:=Node.Parent<>nil;
end;

function THTMLParserEngine.HasText: Boolean;
begin
  result:=Node.Child.Count=0;
end;

function THTMLParserEngine.IsValid: Boolean;
begin
  result:=Node<>nil;
end;

procedure THTMLParserEngine.Item(const Index: Integer);
begin
  Node:=Node.Child[Index];
end;

procedure THTMLParserEngine.LoadFromFile(const Filename: String);
begin
  // TODO
end;

function THTMLParserEngine.Name: String;
begin
  result:=Node.Tag;
end;

procedure THTMLParserEngine.Parent;
begin
  Node:=Node.Parent;
end;

function THTMLParserEngine.Parse(const Text: String): Boolean;
begin
  result:=Node.RunParse(Text);
end;

procedure THTMLParserEngine.Root;
begin
  Node:=DOM.RootNode;
end;

function THTMLParserEngine.Text: String;
begin
  result:=Node.Text;
end;
{$ENDIF}

initialization
  TBIHTML.EngineClass:=THTMLParserEngine;
finalization
  TBIHTML.EngineClass:=nil;
end.
