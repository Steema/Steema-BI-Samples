{*********************************************}
{  TeeBI Software Library                     }
{  Database connection tester                 }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DB.Fire.AllDrivers;

interface

{$IF CompilerVersion>25}
{$DEFINE HASFIREDAC}

  {$DEFINE HASFIREDAC_SQLite}
  {$DEFINE HASFIREDAC_IB}
  {$DEFINE HASFIREDAC_ADS}
  {$DEFINE HASFIREDAC_PosGre}

  {$IFNDEF ANDROID}
  {$DEFINE HASFIREDAC_MSSQL}
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  {$DEFINE HASFIREDAC_ODBC}
  {$DEFINE HASFIREDAC_MSAcc}
  {$ENDIF}

  {$IF CompilerVersion>27}
  {$DEFINE HASFIREDAC_ORACLE}

    {$IFNDEF ANDROID}
    {$DEFINE HASFIREDAC_MYSQL}
    {$ENDIF}

    {$IF CompilerVersion>=30}
      {$IFNDEF ANDROID}
      {$DEFINE HASFIREDAC_MONGODB}
      {$ENDIF}
    {$ENDIF}

  {$ELSE}

    {$IFDEF MSWINDOWS}
    {$DEFINE HASFIREDAC_ORACLE}
    {$DEFINE HASFIREDAC_MYSQL}
    {$ELSE}
    {$IFDEF MACOSX}
    {$DEFINE HASFIREDAC_ORACLE}
    {$DEFINE HASFIREDAC_MYSQL}
    {$ENDIF}
    {$ENDIF}

  {$ENDIF}

{$ENDIF}

uses
  System.Classes,
  BI.DataItem, BI.DB,

  {$IFDEF HASFIREDAC}
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys,

  {$IFDEF HASFIREDAC_ODBC} // Microsoft Windows ODBC
  FireDAC.Phys.ODBC,
  FireDAC.Phys.ODBCBase,
  {$IF CompilerVersion>27}
  FireDAC.Phys.ODBCDef,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF HASFIREDAC_MSSQL} // Microsoft SQL Server
  FireDAC.Phys.MSSQL,
  {$IF CompilerVersion>27}
  FireDAC.Phys.MSSQLDef,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF HASFIREDAC_MSAcc} // Microsoft Access
  FireDAC.Phys.MSAccMeta,
  FireDAC.Phys.MSAcc,
  {$IF CompilerVersion>27}
  FireDAC.Phys.MSAccDef,
  {$ENDIF}
  {$ENDIF}

  FireDAC.Comp.Client, FireDAC.Comp.DataSet,

  {$IFDEF HASFIREDAC_ORACLE} // Oracle
  FireDAC.Phys.OracleMeta,
  {$ENDIF}

  {$IFDEF HASFIREDAC_MYSQL} // MySQL
  FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLMeta,
  {$IF CompilerVersion>27}
  FireDAC.Phys.MySQLDef,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF HASFIREDAC_MONGODB} // MongoDB
  FireDAC.Phys.MongoDB,
  FireDAC.Phys.MongoDBMeta,
  {$IF CompilerVersion>27}
  FireDAC.Phys.MongoDBDef,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF HASFIREDAC_IB} // Embarcadero Interbase
  FireDAC.Phys.IB,
  FireDAC.Phys.IBBase,
  {$IF CompilerVersion>27}
  FireDAC.Phys.IBDef,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF HASFIREDAC_SQLite} // SQLite
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteVDataSet,

  {$IF CompilerVersion>27}
  FireDAC.Phys.SQLiteDef,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF HASFIREDAC_ADS} // ADS
  FireDAC.Phys.ADS,
  {$IF CompilerVersion>27}
  FireDAC.Phys.ADSMeta,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF HASFIREDAC_PosGre} // PostgreSQL
  FireDAC.Phys.PG,
  {$IF CompilerVersion>27}
  FireDAC.Phys.PGMeta,
  {$ENDIF}
  {$ENDIF}

      // Pending:
      (*
      InfxDriver
      Db2Driver
      DSDriver
      ADSDriver
      ASADriver
      *PgDriver
      *)

  FireDAC.Stan.ExprFuncs

  {$ELSE}
  Data.SqlExpr
  {$ENDIF};

type
  TBIDBFireDACTester=class(TBIDBTester)
  public
    class procedure Test(const Driver,Database,Server,Port,User,Password:String;
                        const Prompt:Boolean); override;
  end;

  {$IFDEF HASFIREDAC_SQLite} // SQLite
  TBILocalSQL=record
  public
    class procedure Add(const AOwner:TComponent; const AData:TDataItem; const ALocal:TFDLocalSQL); static;
    class function From(const ALocal:TFDLocalSQL):TDataItem; overload; static;
    class function From(const AOwner:TComponent; const AData:TDataItem):TFDLocalSQL; overload; static;
  end;
  {$ENDIF}

implementation

uses
  SysUtils, BI.UI;

{ TBIDBTester }

class procedure TBIDBFireDACTester.Test(const Driver, Database, Server, Port, User,
                          Password: String; const Prompt: Boolean);
var C : TFDConnection;
begin
  C:=TFDConnection.Create(nil);
  try
    C.Params.Add('DriverID='+Driver);
    C.Params.Add('Database='+Database);
    C.Params.Add('Server='+Server);
    C.Params.Add('Port='+Port);
    C.Params.Add('User_Name='+User);
    C.Params.Add('Password='+TCrypto.Decrypt(Password));

    C.LoginPrompt:=Prompt;

    // Just open the connection to validate it:
    C.Open;

    // Exceptions will be raised if connection cannot succeed
  finally
    C.Free;
  end;
end;

{$IFDEF HASFIREDAC_SQLite} // SQLite

{ TBILocalSQL }

class function TBILocalSQL.From(const ALocal: TFDLocalSQL): TDataItem;
var t : Integer;
    tmp : TDataItem;
begin
  result:=TDataItem.Create;
  result.Name:=ALocal.Name;

  if result.Name='' then
     result.Name:='LocalSQL';

  for t:=0 to ALocal.DataSets.Count-1 do
  begin
    tmp:=TBIDB.From(ALocal.DataSets[t].DataSet,ALocal.DataSets[t].{$IF CompilerVersion>26}FullName{$ELSE}Name{$ENDIF});

    result.Items.Add(tmp);
  end;
end;

class procedure TBILocalSQL.Add(const AOwner:TComponent; const AData:TDataItem;
                                const ALocal:TFDLocalSQL);

  function Exists(const S:String):Boolean;
  begin
    {$IF CompilerVersion>26}
    result:=ALocal.DataSets.FindDataSet('',S)<>nil;
    {$ELSE}
    result:=False;
    {$ENDIF}
  end;

  function UniqueName(const AName:String):String;
  var t : Integer;
  begin
    result:=AName;

    t:=0;

    while Exists(result) do
    begin
      Inc(t);
      result:=AName+'_'+IntToStr(t);
    end;
  end;

  procedure Add(const AData:TDataItem);
  var tmp : TFDLocalSQLDataSet;
  begin
    tmp:=ALocal.DataSets.Add;
    tmp.Name:=UniqueName(AData.Name);
    tmp.DataSet:=TBIDBExport.From(AOwner,AData);
  end;

  function ValidFields(const AItems:TDataItems):Integer;
  var tmp : TDataItem;
  begin
    result:=0;

    for tmp in AItems.AsArray do
        if tmp.Kind<>TDataKind.dkUnknown then
           Inc(result);
  end;

  function IsTable(const AData:TDataItem):Boolean;
  begin
    result:=AData.AsTable and (ValidFields(AData.Items)>0);
  end;

var tmp : TDataItem;
begin
  if AData<>nil then
     if IsTable(AData) then
        Add(AData)
     else
     if AData.Kind=TDataKind.dkUnknown then
        for tmp in AData.Items.AsArray do
            if IsTable(tmp) then
               Add(tmp);
end;

class function TBILocalSQL.From(const AOwner:TComponent; const AData: TDataItem): TFDLocalSQL;
begin
  result:=TFDLocalSQL.Create(AOwner);
  Add(AOwner,AData,result);
end;
{$ENDIF}

end.

