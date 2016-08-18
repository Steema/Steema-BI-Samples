program TeeBI_Gridify_Table;

uses
  Vcl.Forms,
  Unit_Main_Gridify in 'Unit_Main_Gridify.pas' {FromGridify},
  BI.Data.Gridify in '..\..\..\..\..\..\Sources\BI.Data.Gridify.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFromGridify, FromGridify);
  Application.Run;
end.
