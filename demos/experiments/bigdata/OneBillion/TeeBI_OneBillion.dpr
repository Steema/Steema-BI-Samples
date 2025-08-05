program TeeBI_OneBillion;

uses
  Vcl.Forms,
  Create_BigData in 'Create_BigData.pas' {FormCreate},
  Main_Unit in 'Main_Unit.pas' {MainForm},
  Query_BigData in 'Query_BigData.pas' {FormQuery};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
