program TeeBI_kNN_example;

uses
  Vcl.Forms,
  TeeBI_kNN_Unit in 'TeeBI_kNN_Unit.pas' {FormkNN},
  BI.Plugins.R in '..\..\..\..\..\..\Sources\Algorithms\BI.Plugins.R.pas',
  BI.Plugins.R.Command in '..\..\..\..\..\..\Sources\Algorithms\BI.Plugins.R.Command.pas',
  BI.Plugins.R.opaR in '..\..\..\..\..\..\Sources\Algorithms\BI.Plugins.R.opaR.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormkNN, FormkNN);
  Application.Run;
end.
