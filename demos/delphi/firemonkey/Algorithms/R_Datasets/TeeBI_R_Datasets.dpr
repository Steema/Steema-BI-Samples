program TeeBI_R_Datasets;

uses
  FMX.Forms,
  Unit_BI_Datasets in 'Unit_BI_Datasets.pas' {RDatasetsDemo},
  BI.Plugins.R.Command in '..\..\..\..\..\..\Sources\Algorithms\BI.Plugins.R.Command.pas',
  BI.Plugins.R in '..\..\..\..\..\..\Sources\Algorithms\BI.Plugins.R.pas',
  BI.Plugins.R.opaR in '..\..\..\..\..\..\Sources\Algorithms\BI.Plugins.R.opaR.pas',
  FMXBI.R.Console in '..\..\..\..\..\..\Sources\FMX\FMXBI.R.Console.pas' {BIRConsole};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TRDatasetsDemo, RDatasetsDemo);
  Application.Run;
end.
