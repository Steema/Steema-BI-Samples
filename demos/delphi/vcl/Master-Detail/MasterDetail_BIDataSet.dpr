program MasterDetail_BIDataSet;

uses
  Vcl.Forms,
  Master_Detail_Unit in 'Master_Detail_Unit.pas' {MasterDetailForm};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMasterDetailForm, MasterDetailForm);
  Application.Run;
end.
