program TeeBI_AI_Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_Main in 'Unit_Main.pas' {Form_AI_Demo};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TForm_AI_Demo, Form_AI_Demo);
  Application.Run;
end.
