program BIWeb_Stress_Test;

{.$DEFINE FASTMM}

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {MainForm};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  NeverSleepOnMMThreadContention:=True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
