program BI_FMX_Web;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main_FMX_Web in 'Main_FMX_Web.pas' {BIWebMain},
  BI.Web.AllData in '..\BI.Web.AllData.pas',
  BI.Web.Common in '..\BI.Web.Common.pas',
  BI.Web.Common.Chart in '..\BI.Web.Common.Chart.pas',
  nFMX.Trayicon.Win in 'TrayIcon\nFMX.Trayicon.Win.pas',
  BI.Web.IndyContext in '..\BI.Web.IndyContext.pas',
  BI.Web.SingleInstance in '..\BI.Web.SingleInstance.pas',
  BI.Web.Context in '..\BI.Web.Context.pas',
  BI.Web.Logs in '..\BI.Web.Logs.pas',
  BI.Web.Modules.Default in '..\BI.Web.Modules.Default.pas',
  BI.Web.Modules in '..\BI.Web.Modules.pas',
  BI.Web.Scheduler in '..\BI.Web.Scheduler.pas',
  BI.Web.Server.Indy in '..\BI.Web.Server.Indy.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TBIWebMain, BIWebMain);
  Application.Run;
end.
