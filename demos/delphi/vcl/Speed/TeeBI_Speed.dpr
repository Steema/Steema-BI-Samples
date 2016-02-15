program TeeBI_Speed;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FormSpeed};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSpeed, FormSpeed);
  Application.Run;
end.
