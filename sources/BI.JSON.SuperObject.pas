{*********************************************}
{  TeeBI Software Library                     }
{  SuperObject JSON Driver                    }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.JSON.SuperObject;

// https://github.com/hgourvest/superobject

(* USAGE:

uses
  BI.DataItem, BI.JSON, BI.JSON.SuperObject;

var J : TBIJSON;
    Data : TDataArray;

  J:=TBIJSON.CreateEngine(TSuperObject.Create);
  try
    Data:=J.ImportFile('sample.json');
  finally
    J.Free;
  end;
*)

interface

uses
  BI.JSON,

  // Unit not found? Download SuperObject from: http://www.progdigy.com
  SuperObject;

type
  TSuperObjectJSON=class(TJSONEngine)
  private
    JSON : ISuperObject;
    Parents : Array of ISuperObject;

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
    Destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils, BI.Arrays;

{ TSuperObjectJSON }

procedure TSuperObjectJSON.ArrayChild(const Index: Integer);
begin
  Push;
  JSON:=JSON.AsArray[Index];
end;

function TSuperObjectJSON.AsBoolean: Boolean;
begin
  result:=JSON.AsBoolean;
end;

function TSuperObjectJSON.AsDouble: Double;
begin
  result:=JSON.AsDouble;
end;

function TSuperObjectJSON.AsString: String;
begin
  result:=JSON.AsString;
end;

destructor TSuperObjectJSON.Destroy;
begin
  //JSON.Free;
  inherited;
end;

function TSuperObjectJSON.EnterArray: Integer;
begin
  result:=JSON.AsArray.Length;
end;

function TSuperObjectJSON.EnterObject: Integer;
begin
  result:=JSON.AsObject.count;
end;

function TSuperObjectJSON.IsArray: Boolean;
begin
  result:=JSON.DataType=TSuperType.stArray;
end;

function TSuperObjectJSON.IsBoolean: Boolean;
begin
  result:=JSON.DataType=TSuperType.stBoolean;
end;

function TSuperObjectJSON.IsNull: Boolean;
begin
  result:=JSON.DataType=TSuperType.stNull;
end;

function TSuperObjectJSON.IsNumber: Boolean;
begin
  case JSON.DataType of
    stDouble,
    stCurrency,
    stInt: result:=True;
  else
    result:=False;
  end;
end;

function TSuperObjectJSON.IsObject: Boolean;
begin
  result:=JSON.DataType=TSuperType.stObject;
end;

function TSuperObjectJSON.IsString: Boolean;
begin
  result:=JSON.DataType=TSuperType.stString;
end;

function TSuperObjectJSON.ObjectChild(const Index: Integer): String;
var e : TSuperAvlIterator;
    t : Integer;
    tmp : TSuperAvlEntry;
begin
  Push;

  //PENDING: SuperObject does not provide a way to access object pairs by Index,
  //         without using an enumerator or "for" loop.

  e:=JSON.AsObject.GetEnumerator;
  try
    e.First;

    for t:=0 to Index-1 do
        e.MoveNext;

    tmp:=e.Current;

    JSON:=tmp.Value;
    result:=tmp.Name;
  finally
    e.Free;
  end;
end;

procedure TSuperObjectJSON.Parse(const Text: String);
begin
//  JSON.Free;
  JSON:=SO(Text);
end;

procedure TSuperObjectJSON.Pop;
var L : Integer;
begin
  L:=High(Parents);

  if L>-1 then
  begin
    JSON:=Parents[L];
    SetLength(Parents,L);
  end
  else
    raise EBIException.Create('Internal error JSON=nil');
end;

procedure TSuperObjectJSON.Push;
var L : Integer;
begin
  L:=Length(Parents);
  SetLength(Parents,L+1);
  Parents[L]:=JSON;
end;

end.
