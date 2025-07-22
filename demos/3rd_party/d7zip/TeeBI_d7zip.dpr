program TeeBI_d7zip;

uses
  Vcl.Forms,
  Unit_Main_d7zip in 'Unit_Main_d7zip.pas' {Form35};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm35, Form35);
  Application.Run;
end.
