{*********************************************}
{  TeeBI Software Library                     }
{  HTTP Web data access using Indy TIdHttp    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.FPC;

interface

uses
  System.Classes, System.SysUtils,
  fpHttpClient, BI.Web;

type
  EHttpAbort=class(Exception);

  TBIFPCWeb=class(TBIHttp)
  private
    FHttp : TFPHttpClient; // FPC

    //IMaxWork : Int64;

    procedure DoProgress(const ACurrent,ATotal:Int64);
  public
    Constructor Create(const AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Get(const AURL:String; const AStream:TStream); overload; override;
    function Get(const AURL:String):String; overload; override;
    procedure SetProxy(const AProxy:TWebProxy); override;
  end;

implementation
