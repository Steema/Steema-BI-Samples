program BI_VCL_Web;

uses
  Vcl.Forms,
  Main_VCL_Web in 'Main_VCL_Web.pas' {FormBIWeb},
  BI.Web.AllData in '..\BI.Web.AllData.pas',
  Unit_Constants in 'Unit_Constants.pas' {FormConstants},
  BI.Web.Common in '..\BI.Web.Common.pas',
  BI.Web.SingleInstance in '..\BI.Web.SingleInstance.pas',
  BI.Web.IndyContext in '..\BI.Web.IndyContext.pas',
  BI.Web.Server.Indy in '..\BI.Web.Server.Indy.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormBIWeb, FormBIWeb);
  Application.CreateForm(TFormConstants, FormConstants);
  Application.Run;
end.
