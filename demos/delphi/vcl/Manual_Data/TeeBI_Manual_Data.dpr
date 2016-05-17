program TeeBI_Manual_Data;

uses
  Vcl.Forms,
  Unit_Manual_Data in 'Unit_Manual_Data.pas' {FormManual},
  Unit_Examples in 'Unit_Examples.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormManual, FormManual);
  Application.Run;
end.
