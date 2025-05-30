{*********************************************}
{  TeeBI Software Library                     }
{  JSON data Standard (FPC) driver            }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.JSON.FPC;

interface

uses
  fpJSON,
  BI.JSON;

type
  TFPCJSON=class(TJSONEngine)
  private
    JSON : TJSONData;
    Parents : Array of TJSONData;

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
  SysUtils,
  jsonParser,
  BI.Arrays;

procedure TFPCJSON.ArrayChild(const Index: Integer);
begin
  Push;
  JSON:=TJSONArray(JSON)[Index];
end;

function TFPCJSON.AsBoolean: Boolean;
begin
  result:=TJSONBoolean(JSON).AsBoolean;
end;

function TFPCJSON.AsDouble: Double;
begin
  result:=TJSONNumber(JSON).AsFloat;
end;

function TFPCJSON.AsString: String;
begin
  result:=TJSONString(JSON).AsString;
end;

destructor TFPCJSON.Destroy;
begin
  JSON.Free;
  inherited;
end;

function TFPCJSON.ObjectChild(const Index:Integer):String;
begin
  Push;

  JSON:=TJSONObject(JSON).Items[Index];
  result:=TJSONObject(JSON).Names[Index];
end;

procedure TFPCJSON.Push;
var L : Integer;
begin
  L:=Length(Parents);
  SetLength(Parents,L+1);
  Parents[L]:=JSON;
end;

procedure TFPCJSON.Pop;
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

function TFPCJSON.EnterArray: Integer;
begin
  result:=TJSONArray(JSON).Count;
end;

function TFPCJSON.EnterObject: Integer;
begin
  result:=TJSONObject(JSON).Count;
end;

function TFPCJSON.IsArray: Boolean;
begin
  result:=JSON is TJSONArray;
end;

function TFPCJSON.IsBoolean: Boolean;
begin
  result:=JSON is TJSONBoolean;
end;

function TFPCJSON.IsNull: Boolean;
begin
  result:=JSON is TJSONNull;
end;

function TFPCJSON.IsNumber: Boolean;
begin
  result:=JSON is TJSONNumber;
end;

function TFPCJSON.IsObject: Boolean;
begin
  result:=JSON is TJSONObject;
end;

function TFPCJSON.IsString: Boolean;
begin
  result:=JSON is TJSONString;
end;

procedure TFPCJSON.Parse(const Text:String);
var P : TJSONParser;
begin
  JSON.Free;

  P:=TJSONParser.Create(Text);
  try
    JSON:=P.Parse;

    if JSON=nil then
       raise Exception.Create('Error parsing JSON text');
  finally
    P.Free;
  end;
end;

end.
