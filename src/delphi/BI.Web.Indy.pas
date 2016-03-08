{*********************************************}
{  TeeBI Software Library                     }
{  HTTP Web data access using Indy TIdHttp    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.Indy;

interface

uses
  System.Classes, System.SysUtils,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  BI.Web;

type
  EHttpAbort=class(Exception);

  TBIIndy=class(TBIHttp)
  private
    FHttp : TIdHttp; // Indy

    IMaxWork : Int64;

    procedure DoProgress(const ACurrent,ATotal:Int64);
    procedure WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  public
    Constructor Create(const AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Get(const AURL:String; const AStream:TStream); overload; override;
    function Get(const AURL:String):String; overload; override;
    procedure SetProxy(const AProxy:TWebProxy); override;

    property Http:TIdHTTP read FHttp;
  end;

implementation
