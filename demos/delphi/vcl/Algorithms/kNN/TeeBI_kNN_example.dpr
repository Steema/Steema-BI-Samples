program TeeBI_kNN_example;

uses
  Vcl.Forms,
  TeeBI_kNN_Unit in 'TeeBI_kNN_Unit.pas' {FormkNN};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormkNN, FormkNN);
  Application.Run;
end.
