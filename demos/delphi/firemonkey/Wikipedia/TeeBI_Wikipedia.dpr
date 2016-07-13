program TeeBI_Wikipedia;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_Main in 'Unit_Main.pas' {Form35};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TForm35, Form35);
  Application.Run;
end.
