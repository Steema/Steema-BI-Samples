program TeeBI_SynLZ;

uses
  Vcl.Forms,
  Unit_Main_SynLZ in 'Unit_Main_SynLZ.pas' {Form35};

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
