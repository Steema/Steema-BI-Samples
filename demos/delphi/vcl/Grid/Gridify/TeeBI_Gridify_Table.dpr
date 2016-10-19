program TeeBI_Gridify_Table;

uses
  Vcl.Forms,
  Unit_Main_Gridify in 'Unit_Main_Gridify.pas' {FromGridify},
  BI.Demos.RandomTable in 'BI.Demos.RandomTable.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFromGridify, FromGridify);
  Application.Run;
end.
