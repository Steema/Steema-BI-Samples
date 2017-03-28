{*********************************************}
{  TeeBI Software Library                     }
{  HTTP Web data access using Indy TIdHttp    }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.FPC;

interface

uses
  System.Classes, System.SysUtils,
  fpHttpClient, BI.Web, BI.Persist;

type
  EHttpAbort=class(Exception);

  TBIFPCWeb=class(TBIHttp)
  private
    FHttp : TFPHttpClient; // FPC

    //IMaxWork : Int64;
  public
    Constructor Create(const AOwner:TComponent); override;
    Destructor Destroy; override;

    class function FTP(const ADef:TDataDefinition):TBIFtp; override;

    procedure Get(const AURL:String; const AStream:TStream); overload; override;
    function Get(const AURL:String):String; overload; override;
    class function Parse(const AURL:String):TWebURL; override;
    procedure SetProxy(const AProxy:TWebProxy); override;
    procedure SetTimeout(const ATimeout:Integer); override;
  end;

implementation
