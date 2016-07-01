program BIVisual_Editor;

uses
  Vcl.Forms,
  Unit_Main in 'Unit_Main.pas' {Form34};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm34, Form34);
  Application.Run;
end.
