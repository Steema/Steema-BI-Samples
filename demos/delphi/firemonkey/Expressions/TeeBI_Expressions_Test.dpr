program TeeBI_Expressions_Test;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main_Unit in 'Main_Unit.pas' {FormMain},
  BI.FMX.Expression.Tree in 'BI.FMX.Expression.Tree.pas',
  BI.Expression.Benchmark in 'BI.Expression.Benchmark.pas',
  BI.Expressions.Samples in '..\..\..\..\tests\BI.Expressions.Samples.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
