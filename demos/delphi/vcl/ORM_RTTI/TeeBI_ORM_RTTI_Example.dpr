program TeeBI_ORM_RTTI_Example;

uses
  Vcl.Forms,
  Unit15 in 'Unit15.pas' {RTTIDemo},
  Space_Flights in 'Space_Flights.pas',
  Customers in 'Customers.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TRTTIDemo, RTTIDemo);
  Application.Run;
end.
