program TeeBI_Data_Ranks;

uses
  Vcl.Forms,
  Main_Unit_Ranks in 'Main_Unit_Ranks.pas' {FormDataRank};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDataRank, FormDataRank);
  Application.Run;
end.
