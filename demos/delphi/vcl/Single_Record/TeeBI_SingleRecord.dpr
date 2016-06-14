program TeeBI_SingleRecord;

uses
  Vcl.Forms,
  Unit_Main in 'Unit_Main.pas' {SingleRecordDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSingleRecordDemo, SingleRecordDemo);
  Application.Run;
end.
