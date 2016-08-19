program TeeBI_FDMemTable_Sqlite_Speed;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit35 in 'Unit35.pas' {LocalSQLvsBI};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLocalSQLvsBI, LocalSQLvsBI);
  Application.Run;
end.
