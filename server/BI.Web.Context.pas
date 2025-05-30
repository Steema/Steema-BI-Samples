{*********************************************}
{  TeeBI Software Library                     }
{  Abstract Web Server class                  }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.Context;

interface

uses
  System.Classes;

type
  // Abstract class (see BI.Web.IndyContext for implementation)
  TWebContext=class
  public
    Context : TObject;
    ContentText: String;
    ContentType: String;
    ResponseInfo: TObject;
    RequestInfo: TObject;
    ResponseStream: TStream;

    procedure AddCookie(const AHost,AName,AValue:String); virtual; abstract;
    function FormParams: String; virtual; abstract;
    function GetContentType:String; virtual; abstract;
    function GetCookie(const AName:String):String; virtual; abstract;
    function GetDocument:String; virtual; abstract;
    function GetStream:TStream; virtual; abstract;
    function Headers: TStrings; virtual; abstract;
    function Params:TStrings; virtual; abstract;
    function Post(const AURL:String; const AParams:TStrings):String; virtual;
    procedure Redirect(const AURL: String); virtual; abstract;
    function ResponseSize: Int64; virtual; abstract;
    procedure ReturnFile(const AFile:String); virtual; abstract;
    procedure ReturnIcon(const AStream:TStream); virtual; abstract;
    procedure SetResponse(const AText: String); overload; virtual; abstract;
    procedure SetResponse(const AType: String; const AStream:TStream); overload; virtual; abstract;
  end;

implementation

{ TWebContext }

function TWebContext.Post(const AURL: String; const AParams: TStrings): String;
begin
  result:='';
end;

end.
