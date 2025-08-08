program TeeBI_Speed_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {FormSpeed},
  BI.Tests.Speed in '..\..\..\..\tests\BI.Tests.Speed.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TFormSpeed, FormSpeed);
  Application.Run;
end.
