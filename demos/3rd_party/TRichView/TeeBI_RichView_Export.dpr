program TeeBI_RichView_Export;

uses
  Vcl.Forms,
  Unit35 in 'Unit35.pas' {FormRichView};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormRichView, FormRichView);
  Application.Run;
end.
