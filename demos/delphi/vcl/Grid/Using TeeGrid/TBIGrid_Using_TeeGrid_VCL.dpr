program TBIGrid_Using_TeeGrid_VCL;

uses
  Vcl.Forms,
  Unit_Main in 'Unit_Main.pas' {FormTeeGrid};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormTeeGrid, FormTeeGrid);
  Application.Run;
end.
