program BI_FMX_Web;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main_FMX_Web in 'Main_FMX_Web.pas' {BIWebMain},
  BI.Web.AllData in '..\BI.Web.AllData.pas',
  BI.Web.Common in '..\BI.Web.Common.pas',
  nFMX.Trayicon.Win in 'TrayIcon\nFMX.Trayicon.Win.pas',
  BI.Web.IndyContext in '..\BI.Web.IndyContext.pas',
  BI.Web.SingleInstance in '..\BI.Web.SingleInstance.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TBIWebMain, BIWebMain);
  Application.Run;
end.
