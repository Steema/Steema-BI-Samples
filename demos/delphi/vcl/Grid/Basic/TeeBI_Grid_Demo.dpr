program TeeBI_Grid_Demo;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {GridDemoForm};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TGridDemoForm, GridDemoForm);
  Application.Run;
end.
