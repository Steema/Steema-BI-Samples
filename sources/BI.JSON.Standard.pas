{*********************************************}
{  TeeBI Software Library                     }
{  JSON data Standard (Delphi) driver         }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.JSON.Standard;

interface

{$IF CompilerVersion>21}
{$DEFINE D14}
{$ENDIF}

{$IF CompilerVersion>24}
{$DEFINE D18}
{$ENDIF}

{$IF CompilerVersion>26}
{$DEFINE D15}
{$DEFINE D20}
{$ENDIF}

{$IF CompilerVersion>29}
{$DEFINE HASBOOL}
{$ENDIF}

{$IFDEF D14}
 {$DEFINE HASJSON}
{$ELSE}
{$IFDEF D7}
 {$IFDEF HASLKJSON}
  {$DEFINE HASJSON}
 {$ENDIF}
{$ENDIF}
{$ENDIF}

uses
  {$IFDEF HASJSON}
   {$IFDEF D20}
   System.JSON,
   {$ELSE}
   {$IFDEF D14}
   DBXJSON,
   {$ELSE}
   uLkJSON,
   {$ENDIF}
   {$ENDIF}
  {$ENDIF}

  BI.JSON;

type
  TStandardJSON=class(TJSONEngine)
  private
    JSON : TJSONValue;
    Parents : Array of TJSONValue;

    IRoot : TJSONValue;

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
  {$IF CompilerVersion>32}
  {System.}Generics.Collections,
  {$ENDIF}
  SysUtils, BI.Arrays;

{ TStandardJSON }

Destructor TStandardJSON.Destroy;
begin
  IRoot.Free;
  inherited;
end;

procedure TStandardJSON.ArrayChild(const Index: Integer);
begin
  Push;
  JSON:=TJSONArray(JSON).{$IFDEF D20}Items[Index]{$ELSE}Get(Index){$ENDIF};
end;

function TStandardJSON.AsBoolean: Boolean;
begin
  {$IFDEF HASBOOL}
  result:=TJSONBool(JSON).AsBoolean;
  {$ELSE}
  result:=SameText(JSON.Value,'TRUE');
  {$ENDIF}
end;

function TStandardJSON.AsDouble: Double;
begin
  result:=TJSONNumber(JSON).AsDouble;
end;

function TStandardJSON.AsString: String;
begin
  result:=TJSONString(JSON).Value;
end;

function TStandardJSON.ObjectChild(const Index:Integer):String;
var tmp : TJSONPair;
begin
  Push;

  tmp:=TJSONObject(JSON).{$IFDEF D20}Pairs[Index]{$ELSE}Get(Index){$ENDIF};

  JSON:=tmp.JsonValue;
  result:=tmp.JsonString.Value;
end;

procedure TStandardJSON.Push;
var L : Integer;
begin
  L:=Length(Parents);
  SetLength(Parents,L+1);
  Parents[L]:=JSON;
end;

procedure TStandardJSON.Pop;
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

function TStandardJSON.EnterArray: Integer;
begin
  result:=TJSONArray(JSON).{$IFDEF D20}Count{$ELSE}Size{$ENDIF};
end;

function TStandardJSON.EnterObject: Integer;
begin
  result:=TJSONObject(JSON).{$IFDEF D20}Count{$ELSE}Size{$ENDIF};
end;

function TStandardJSON.IsArray: Boolean;
begin
  result:=JSON is TJSONArray;
end;

function TStandardJSON.IsBoolean: Boolean;
begin
  {$IFDEF HASBOOL}
  result:=JSON is TJSONBool;
  {$ELSE}
  result:=SameText(JSON.Value,'TRUE') or SameText(JSON.Value,'FALSE');
  {$ENDIF}
end;

function TStandardJSON.IsNull: Boolean;
begin
  result:=JSON is TJSONNull;
end;

function TStandardJSON.IsNumber: Boolean;
begin
  result:=JSON is TJSONNumber;
end;

function TStandardJSON.IsObject: Boolean;
begin
  result:=JSON is TJSONObject;
end;

function TStandardJSON.IsString: Boolean;
begin
  result:=JSON is TJSONString;
end;

{$IFDEF D14}
{$IFNDEF D15}
// Internal use, to detect "Zero Based Strings"
var
  ZBS : Boolean=False; // Zero-based strings
  FirstStringChar : Integer=1;
{$ENDIF}
{$ENDIF}

procedure TStandardJSON.Parse(const Text:String);

  {$IFDEF D14}
  {$IFNDEF D15}
  function CleanJSON(const AText:String):String;
  var t,
      Offset : Integer;
      C : Char;
  begin
    result:='';

    if ZBS then
       Offset:=-1
    else
       Offset:=0;

    for t:=FirstStringChar to Length(AText)-Offset do
    begin
      C:=AText[t];

      if (C<>#13) and (C<>#10) and (C<>' ') then
         result:=result+C;
    end;
  end;
  {$ENDIF}
  {$ENDIF}

begin
  JSON.Free;

  {$IFDEF D14}
    {$IFDEF D15}
    JSON:=TJSONObject.ParseJSONValue(Text);

    // Not UTF8:
    // result:=TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(Text), 0, []);

    {$ELSE}
    JSON:=TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(CleanJSON(Text)),0);
    {$ENDIF}
  {$ELSE}
  JSON:=TlkJSON.ParseText(Text);
  {$ENDIF}

  if JSON=nil then
     raise EBIJSON.Create('Cannot parse JSON text. Not valid JSON format');

  IRoot:=JSON;
end;

{$IFDEF D14}
{$IFNDEF D15}
procedure CheckZBS;
begin
  {$IFDEF D18}
  ZBS:=Low(String)=0;
  {$ELSE}
  ZBS:=False;
  {$ENDIF}

  if ZBS then
     FirstStringChar:=0
  else
     FirstStringChar:=1;
end;

initialization
  CheckZBS;
{$ENDIF}
{$ENDIF}
end.
