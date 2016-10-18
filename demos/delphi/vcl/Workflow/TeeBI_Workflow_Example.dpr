program TeeBI_Workflow_Example;

uses
  Vcl.Forms,
  Unit_Workflow_Example in 'Unit_Workflow_Example.pas' {FormWorkflow};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormWorkflow, FormWorkflow);
  Application.Run;
end.
