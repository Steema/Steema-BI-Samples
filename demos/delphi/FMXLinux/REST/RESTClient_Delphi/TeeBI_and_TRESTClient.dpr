program TeeBI_and_TRESTClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_Main_Form in 'Unit_Main_Form.pas' {REST_BIWeb};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TREST_BIWeb, REST_BIWeb);
  Application.Run;
end.
