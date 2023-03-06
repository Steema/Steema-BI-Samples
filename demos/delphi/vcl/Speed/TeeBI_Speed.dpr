program TeeBI_Speed;

uses
  //FastMM5,
  //nxReplacementMemoryManager,
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FormSpeed},
  BI.Tests.Speed in '..\..\..\..\tests\BI.Tests.Speed.pas';

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
