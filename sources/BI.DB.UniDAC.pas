{*********************************************}
{  TeeBI Software Library                     }
{  UniDAC database support                    }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DB.UniDAC;

interface

// Database Engine for DevArt UniDAC "Data Access Components"

{
  https://www.devart.com/unidac

  "MSAccess" and "MSClasses" units are required by this unit.

  Usage:

    uses
      BI.DB, BI.DB.UniDAC;

    TBIDB.Engine:=TDBUniDACEngine.Create;

}

uses
  System.Classes, System.Types, Data.DB, MSAccess, BI.DB, BI.DataItem,
  BI.Arrays, BI.Persist, BI.DataSource;

type
  TDBUniDACEngine=class(TBIDBEngine)
  public
    class function CloneConnection(const AConn:TCustomConnection): TCustomConnection; override;
    class function CreateConnection(const Definition:TDataDefinition):TCustomConnection; override;
    class function CreateDataSet(const AOwner:TComponent; const AData:TDataItem):TDataSet; override;
    class function CreateQuery(const AConnection:TCustomConnection; const SQL:String):TDataSet; override;
    class function DriverNames:TStringDynArray; override;
    class function DriverToName(const ADriver:String):String; override;
    class function GetConnectionName(const AConnection:TCustomConnection):String; override;
    class function GetDriver(const AIndex:Integer):String; override;
    class function GetKeyFieldNames(const AConnection:TCustomConnection; const ATable:String):TStrings; override;
    class function GetSchemas(const AConnection:TCustomConnection):TStrings;
    class function GetTable(const AConnection:TCustomConnection; const AName:String):TDataSet; override;
    class function GetTableNames(const AConnection:TCustomConnection):TStrings; override;
    class procedure GuessForeignKeys(const AName:String; const Table:TDataSet; const AData:TDataItem; const Source:TBISource); override;
    class function ImportFile(const Source:TBIDB; const AFileName:String):TDataArray; override;
    class function Tester:TBIDBTesterClass; override;
  end;

  {
  TBIUniDAC=class(TBIDB)
  private
    function Import(const Connection:TMSConnection; const AName:String):TDataItem; overload;
  public
    function Import(const Connection:TMSConnection):TDataItem; overload;
    class function Import(const Connection:TMSConnection; const MultiThread:Boolean):TDataItem; overload;
  end;
  }

implementation

{.$DEFINE TEEPARALLEL}

uses
  //ActiveX,
  System.SysUtils, System.Threading, System.Diagnostics,
  {$IFDEF TEEPARALLEL}
  TeeParallel,
  {$ENDIF}
  DB, MSClasses;

{ TDBUniDACEngine }

class function TDBUniDACEngine.CloneConnection(
  const AConn: TCustomConnection): TCustomConnection;
begin

end;

class function TDBUniDACEngine.CreateConnection(
  const Definition: TDataDefinition): TCustomConnection;
begin

end;

class function TDBUniDACEngine.CreateDataSet(const AOwner: TComponent;
  const AData: TDataItem): TDataSet;
begin

end;

class function TDBUniDACEngine.CreateQuery(const AConnection: TCustomConnection;
  const SQL: String): TDataSet;
begin

end;

class function TDBUniDACEngine.DriverNames: TStringDynArray;
begin

end;

class function TDBUniDACEngine.DriverToName(const ADriver: String): String;
begin

end;

class function TDBUniDACEngine.GetConnectionName(
  const AConnection: TCustomConnection): String;
begin

end;

class function TDBUniDACEngine.GetDriver(const AIndex: Integer): String;
begin

end;

class function TDBUniDACEngine.GetKeyFieldNames(
  const AConnection: TCustomConnection; const ATable: String): TStrings;
begin

end;

class function TDBUniDACEngine.GetSchemas(
  const AConnection: TCustomConnection): TStrings;
begin

end;

class function TDBUniDACEngine.GetTable(const AConnection: TCustomConnection;
  const AName: String): TDataSet;
begin

end;

class function TDBUniDACEngine.GetTableNames(const AConnection: TCustomConnection): TStrings;
begin

end;

class procedure TDBUniDACEngine.GuessForeignKeys(const AName: String;
  const Table: TDataSet; const AData: TDataItem; const Source: TBISource);
begin
  inherited;

end;

class function TDBUniDACEngine.ImportFile(const Source: TBIDB;
  const AFileName: String): TDataArray;
begin

end;

class function TDBUniDACEngine.Tester: TBIDBTesterClass;
begin
  result:=nil;
end;

end.
