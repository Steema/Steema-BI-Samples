program TeeBI_FMX_Summary;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_Summary in 'Unit_Summary.pas' {Form17},
  Unit_Histogram_Text in 'Unit_Histogram_Text.pas' {FormHistogramText};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm17, Form17);
  Application.Run;
end.
