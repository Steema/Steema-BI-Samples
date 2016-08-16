program TeeBI_RichView_Export;

uses
  Vcl.Forms,
  Unit35 in 'Unit35.pas' {FormRichView};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormRichView, FormRichView);
  Application.Run;
end.
