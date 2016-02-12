program TeeBI_DataSearch_Demo;

uses
  Vcl.Forms,
  Unit_DataSearch_Demo in 'Unit_DataSearch_Demo.pas' {FormSearchDemo};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSearchDemo, FormSearchDemo);
  Application.Run;
end.
