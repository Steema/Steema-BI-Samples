library BI_VCL_IIS_Web;

uses
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.Win.ISAPIApp,
  Web.Win.ISAPIThreadPool,
  BIWebModuleUnit in 'BIWebModuleUnit.pas' {WebModule1: TWebModule},
  BI.Web.AllData in '..\..\BI.Web.AllData.pas',
  BI.Web.Common in '..\..\BI.Web.Common.pas',
  BI.Web.IISContext in 'BI.Web.IISContext.pas';

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end.
