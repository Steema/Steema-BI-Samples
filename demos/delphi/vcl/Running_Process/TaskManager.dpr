program TaskManager;

uses
  Vcl.Forms,
  Unit_TaskManager in 'Unit_TaskManager.pas' {FormTaskManager},
  BI.Process in 'BI.Process.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormTaskManager, FormTaskManager);
  Application.Run;
end.
