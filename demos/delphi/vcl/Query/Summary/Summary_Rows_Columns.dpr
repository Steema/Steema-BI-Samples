program Summary_Rows_Columns;

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF }
  Vcl.Forms,
  Unit_Summary in 'Unit_Summary.pas' {FormSummary},
  Vcl.Themes,
  Vcl.Styles,
  BI.Tests.SummarySamples in '..\..\github\tests\BI.Tests.SummarySamples.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSummary, FormSummary);
  Application.Run;
end.
