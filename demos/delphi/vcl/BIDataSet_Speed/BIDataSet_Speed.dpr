program BIDataSet_Speed;

uses
  Vcl.Forms,
  Unit_BIDataSet_vs_FireDAC in 'Unit_BIDataSet_vs_FireDAC.pas' {DatasetSpeed};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDatasetSpeed, DatasetSpeed);
  Application.Run;
end.
