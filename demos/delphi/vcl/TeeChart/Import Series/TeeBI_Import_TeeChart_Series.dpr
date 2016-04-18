program TeeBI_Import_TeeChart_Series;

uses
  Vcl.Forms,
  Unit_Main in 'Unit_Main.pas' {SeriesImport};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSeriesImport, SeriesImport);
  Application.Run;
end.
