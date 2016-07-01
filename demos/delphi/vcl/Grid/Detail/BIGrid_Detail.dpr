program BIGrid_Detail;

uses
  Vcl.Forms,
  Unit_Grid_Detail in 'Unit_Grid_Detail.pas' {FormGridDetail};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormGridDetail, FormGridDetail);
  Application.Run;
end.
