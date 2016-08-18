program TeeBI_Speed_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {FormSpeed};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSpeed, FormSpeed);
  Application.Run;
end.
