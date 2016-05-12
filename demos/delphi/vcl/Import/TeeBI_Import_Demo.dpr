program TeeBI_Import_Demo;

uses
  Vcl.Forms,
  Automatic_File_Import in 'Automatic_File_Import.pas' {ImportDemoForm},
  FindSampleData in 'FindSampleData.pas',
  Import_File in 'Import_File.pas',
  ImportData in 'ImportData.pas' {ImportDataMain},
  Import_ByCode in 'Import_ByCode.pas' {ByCode},
  Import_FromComponents in 'Import_FromComponents.pas' {FromComponents},
  Import_Store in 'Import_Store.pas' {FromBIStore},
  Import_RTTI in 'Import_RTTI.pas' {FromRTTI},
  Customers in '..\ORM_RTTI\Customers.pas',
  Space_Flights in '..\ORM_RTTI\Space_Flights.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TImportDataMain, ImportDataMain);
  Application.Run;
end.
