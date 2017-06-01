program BITree_VCL_Example;

uses
  Vcl.Forms,
  Unit_BITree_Example in 'Unit_BITree_Example.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
