program TeeChart_Connect;

uses
  Vcl.Forms,
  Unit_Chart_Connect in 'Unit_Chart_Connect.pas' {FormTeeConnect};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormTeeConnect, FormTeeConnect);
  Application.Run;
end.
