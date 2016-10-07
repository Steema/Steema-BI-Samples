program TeeBI_Wikipedia;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_Main in 'Unit_Main.pas' {FormWiki};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TFormWiki, FormWiki);
  Application.Run;
end.
