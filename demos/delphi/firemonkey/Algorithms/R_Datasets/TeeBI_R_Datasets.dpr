program TeeBI_R_Datasets;

uses
//  System.StartUpCopy,
  FMX.Forms,
  Unit_BI_Datasets in 'Unit_BI_Datasets.pas' {RDatasetsDemo};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TRDatasetsDemo, RDatasetsDemo);
  Application.Run;
end.
