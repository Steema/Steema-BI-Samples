program BI_VCL_Web;

uses
  Vcl.Forms,
  Main_VCL_Web in 'Main_VCL_Web.pas' {FormBIWeb},
  BI.Web.AllData in '..\BI.Web.AllData.pas',
  Unit_Constants in 'Unit_Constants.pas' {FormConstants},
  BI.Web.Common in '..\BI.Web.Common.pas';

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
