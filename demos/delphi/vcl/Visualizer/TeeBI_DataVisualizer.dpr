program TeeBI_DataVisualizer;

uses
  Vcl.Forms,
  Unit_BI_Visualizer_Test in 'Unit_BI_Visualizer_Test.pas' {FormViz};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormViz, FormViz);
  Application.Run;
end.
