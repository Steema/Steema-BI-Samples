program TeeBI_FMX_Summary;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_Summary in 'Unit_Summary.pas' {Main_Form},
  Unit_Histogram_Text in 'Unit_Histogram_Text.pas' {FormHistogramText};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TMain_Form, Main_Form);
  Application.Run;
end.
