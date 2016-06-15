program TeeBI_SingleRecord;

uses
  Vcl.Forms,
  Unit_Main in 'Unit_Main.pas' {SingleRecordDemo},
  Unit_UsingProvider in 'Unit_UsingProvider.pas' {SingleRecordProviderDemo},
  Unit_Menu in 'Unit_Menu.pas' {SingleRecordMenu};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSingleRecordMenu, SingleRecordMenu);
  Application.Run;
end.
