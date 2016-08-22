program BIWeb_Tests;

uses
  Vcl.Forms,
  Unit_BIWeb_Tests in 'Unit_BIWeb_Tests.pas' {FormBIWebTests},
  BI.Web.Tests in 'BI.Web.Tests.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormBIWebTests, FormBIWebTests);
  Application.Run;
end.
