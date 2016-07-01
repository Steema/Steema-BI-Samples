library BIWeb_Apache;

uses
  {$IFDEF MSWINDOWS}
  Winapi.ActiveX,
  System.Win.ComObj,
  {$ENDIF }
  Web.WebBroker,
  Web.HTTPD24Impl,
  BIWeb.ModuleUnit.Apache in 'BIWeb.ModuleUnit.Apache.pas' {WebModule1: TWebModule},
  BI.Web.AllData in '..\..\BI.Web.AllData.pas',
  BI.Web.Common in '..\..\BI.Web.Common.pas',
  BI.Web.ApacheContext in 'BI.Web.ApacheContext.pas';

{$R *.res}

// httpd.conf entries:
//
(*
 LoadModule teebi_module modules/mod_teebi.dll

 <Location /xyz>
    SetHandler mod_teebi-handler
 </Location>
*)
//
// These entries assume that the output directory for this project is the apache/modules directory.
//
// httpd.conf entries should be different if the project is changed in these ways:
//   1. The TApacheModuleData variable name is changed
//   2. The project is renamed.
//   3. The output directory is not the apache/modules directory
//

// Declare exported variable so that Apache can access this module.
var
  GModuleData: TApacheModuleData;
exports
  GModuleData name 'teebi_module';

begin
{$IFDEF MSWINDOWS}
  CoInitFlags := COINIT_MULTITHREADED;
{$ENDIF}
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end.
