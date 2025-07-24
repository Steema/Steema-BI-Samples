program TeeBI_USA_Counties;

uses
  Vcl.Forms,
  Main_Unit in 'Main_Unit.pas' {USADemo_Form};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TUSADemo_Form, USADemo_Form);
  Application.Run;
end.
