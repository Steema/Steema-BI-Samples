program Custom_Function_Expressions;

uses
  Vcl.Forms,
  Unit_Main in 'Unit_Main.pas' {CustomFuncTest},
  BI.Expression.Custom in 'BI.Expression.Custom.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TCustomFuncTest, CustomFuncTest);
  Application.Run;
end.
