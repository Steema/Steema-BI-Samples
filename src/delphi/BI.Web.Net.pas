{*********************************************}
{  TeeBI Software Library                     }
{  HTTP Web data access using System.Net      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Web.Net;

interface

{
 By default, Indy TIdHttp client component is used.

 To use the standard RTL THttp class instead of Indy:

 uses BI.Web.Net;
 TBIHttp.Engine:=TBIHttpClient;

}

{$IF CompilerVersion>27}
{$DEFINE THREADING}
{$ENDIF}

uses
  System.Classes, BI.Web,

  {$IFDEF THREADING}
  System.Threading,
  {$ENDIF}

  System.Net.HttpClient, System.Net.HttpClientComponent;

type
  TBIHttpClient=class(TBIHttp)
  private
    FHttp : TNetHttpClient;  // System.Net

    {$IFDEF THREADING}
    FTask : ITask;

    procedure ReceivedData(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
    procedure RequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
    {$ENDIF}
  public
    Constructor Create(const AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Get(const AURL:String; const AStream:TStream); overload; override;
    function Get(const AURL:String):String; overload; override;
    procedure SetProxy(const AProxy:TWebProxy); override;

    property Http:TNetHTTPClient read FHttp;
  end;

implementation
