program TeeBI_Composer_Example;

uses
  Vcl.Forms,
  Unit_Composer_Main in 'Unit_Composer_Main.pas' {MainForm},
  BI.Tests.SummarySamples in '..\..\..\..\tests\BI.Tests.SummarySamples.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
