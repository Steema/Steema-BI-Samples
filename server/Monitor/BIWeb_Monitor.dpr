program BIWeb_Monitor;

uses
  Vcl.Forms,
  Unit_BIWeb_Monitor in 'Unit_BIWeb_Monitor.pas' {BIWebMonitor};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TBIWebMonitor, BIWebMonitor);
  Application.Run;
end.
