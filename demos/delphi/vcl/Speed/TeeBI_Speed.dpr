program TeeBI_Speed;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FormSpeed};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSpeed, FormSpeed);
  Application.Run;
end.
