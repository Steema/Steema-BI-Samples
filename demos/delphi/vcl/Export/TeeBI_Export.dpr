program TeeBI_Export;

uses
  Vcl.Forms,
  Unit_Main in 'Unit_Main.pas' {ExportDemo},
  WebBrowser_Load in 'WebBrowser_Load.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TExportDemo, ExportDemo);
  Application.Run;
end.
