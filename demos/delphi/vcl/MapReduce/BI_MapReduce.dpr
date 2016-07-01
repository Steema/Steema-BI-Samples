program BI_MapReduce;

uses
  Vcl.Forms,
  Unit_Main in 'Unit_Main.pas' {DemoForm};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDemoForm, DemoForm);
  Application.Run;
end.
