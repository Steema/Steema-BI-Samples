program TeeBI_Import_TeeChart_Series;

uses
  Vcl.Forms,
  Unit_Main in 'Unit_Main.pas' {SeriesImport},
  Unit_XYTest in 'Unit_XYTest.pas' {FormXYTest};

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
