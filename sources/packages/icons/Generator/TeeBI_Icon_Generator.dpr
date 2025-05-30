program TeeBI_Icon_Generator;

uses
  Vcl.Forms,
  Unit_Icon_Generator in 'Unit_Icon_Generator.pas' {FormIconGenerator};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormIconGenerator, FormIconGenerator);
  Application.Run;
end.
