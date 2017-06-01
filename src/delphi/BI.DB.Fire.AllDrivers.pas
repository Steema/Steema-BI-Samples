{*********************************************}
{  TeeBI Software Library                     }
{  Database connection tester                 }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DB.Fire.AllDrivers;

interface

{$IF CompilerVersion>25}
{$DEFINE HASFIREDAC}

  {$DEFINE HASFIREDAC_SQLite}
  {$DEFINE HASFIREDAC_IB}
  {$DEFINE HASFIREDAC_ADS}

  {$IFNDEF ANDROID}
  {$DEFINE HASFIREDAC_MSSQL}
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  {$DEFINE HASFIREDAC_ODBC}
  {.$DEFINE HASFIREDAC_MSAcc}
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

      // Pending:
      (*
      InfxDriver
      Db2Driver
      DSDriver
      ADSDriver
      ASADriver
      PgDriver
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
