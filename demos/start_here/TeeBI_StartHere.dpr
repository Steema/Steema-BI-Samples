program TeeBI_StartHere;

uses
  Vcl.Forms,
  Unit_StartHere in 'Unit_StartHere.pas' {MainForm},
  Unit_SampleData in 'Unit_SampleData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
