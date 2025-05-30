{*********************************************}
{  TeeBI Software Library                     }
{  mORMot JSON Driver                         }
{  http://www.synopse.info                    }
{                                             }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.JSON.mORMot;

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
  BI.DataItem, BI.JSON, BI.JSON.mORMot,
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
  BI.JSON, SynCommons;

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

{ TmORMotJSONEngine }

procedure TmORMotJSONEngine.ArrayChild(const Index: Integer);
begin
  JSON:=JSONArrayItem(JSON,Index);
end;

function TmORMotJSONEngine.AsBoolean: Boolean;
begin
 // ??
end;

function TmORMotJSONEngine.AsDouble: Double;
begin
 // ??
end;

function TmORMotJSONEngine.AsString: String;
begin
  result:=UTF8ToString(JSON);
end;

function TmORMotJSONEngine.EnterArray: Integer;
begin
  Push;
  result:=JSONArrayCount(JSON);
end;

function TmORMotJSONEngine.EnterObject: Integer;
begin

end;

function TmORMotJSONEngine.IsArray: Boolean;
begin
 // ??
end;

function TmORMotJSONEngine.IsBoolean: Boolean;
begin
 // ??
end;

function TmORMotJSONEngine.IsNull: Boolean;
begin
 // ??
end;

function TmORMotJSONEngine.IsNumber: Boolean;
begin
 // ??
end;

function TmORMotJSONEngine.IsObject: Boolean;
begin
 // ??
end;

function TmORMotJSONEngine.IsString: Boolean;
begin
 // ??
end;

function TmORMotJSONEngine.ObjectChild(const Index: Integer): String;
//var tmp : PUTF8Char;
begin
  // ??
  // GetJSONField(JSON,tmp,@WasString,EndOfObject);
end;

procedure TmORMotJSONEngine.Parse(const Text: String);
var P : PUTF8Char;
begin
  P := GotoNextNotSpace(PUTF8Char(StringToUTF8(Text)));
  JSON:=GetJSONFieldOrObjectOrArray(P,@WasString,nil,True);
end;

procedure TmORMotJSONEngine.Pop;
var L : Integer;
begin
  L:=High(Parents);

  if L>-1 then
  begin
    JSON:=Parents[L];
    SetLength(Parents,L);
  end
  else
    raise EBIJSON.Create('Internal error JSON=nil');
end;

procedure TmORMotJSONEngine.Push;
var L : Integer;
begin
  L:=Length(Parents);
  SetLength(Parents,L+1);
  Parents[L]:=JSON;
end;

end.
