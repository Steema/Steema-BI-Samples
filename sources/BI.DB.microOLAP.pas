{*********************************************}
{  TeeBI Software Library                     }
{  microOLAP MySQL DAC data import and export }
{  http://www.microolap.com                   }
{                                             }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DB.microOLAP;

interface

{
  Alternative Database Engine using microOLAP mySQL components

  Usage:

    uses
      BI.DB, BI.DB.microOLAP;

    TBIDB.Engine:=TDBmicroOLAPEngine.Create;

}

uses
  System.Classes, System.Types, Data.DB, BI.DataItem, BI.DB, BI.DataSource,
  BI.Persist;

type
  TDBmicroOLAPEngine=class(TBIDBEngine)
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
    class function Supports(const Extension:String):Boolean; override;
    class function Tester:TBIDBTesterClass; override;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, BI.UI,
  mySQLDbTables;

type
  TDataAccess=class(TDataItem);
  TBISourceAccess=class(TBISource);

class procedure TDBmicroOLAPEngine.GuessForeignKeys(const AName:String; const Table:TDataSet; const AData:TDataItem; const Source:TBISource);
begin

end;

(*
var Meta : TFDMetaInfoQuery;
    Fields : TFDMetaInfoQuery;

    LocalColumn,
    ForeignColumn,
    ForeignSchema,
    ForeignKey,
    ForeignTable : TStringField;

    tmpSchema,
    tmpLocal,
    tmpTable : String;

    tmpCol : TDataItem;
begin
  Meta:=TFDMetaInfoQuery.Create(nil);
  try
    Meta.MetaInfoKind:=TFDPhysMetaInfoKind.mkForeignKeys;
    Meta.ObjectName:=AName;
    Meta.Connection:=(Table as TFDRdbmsDataSet).Connection;

    try
      Meta.Open;

      ForeignSchema:=Meta.Fields.FindField('PKEY_SCHEMA_NAME') as TStringField;
      ForeignTable:=Meta.Fields.FindField('PKEY_TABLE_NAME') as TStringField;
      ForeignKey:=Meta.Fields.FindField('FKEY_NAME') as TStringField;

      Fields:=TFDMetaInfoQuery.Create(nil);
      try
        Fields.Connection:=(Table as TFDRdbmsDataSet).Connection;

        Meta.First;
        while not Meta.Eof do
        begin
          tmpTable:=ForeignTable.AsString;
          tmpSchema:=ForeignSchema.AsString;

          if tmpSchema<>'' then
             tmpSchema:=tmpSchema+'.';

          Fields.Close;

          Fields.BaseObjectName:=AName;
          Fields.ObjectName:=ForeignKey.AsString;
          Fields.MetaInfoKind:=TFDPhysMetaInfoKind.mkForeignKeyFields;
          Fields.Open;

          LocalColumn:=Fields.Fields.FindField('COLUMN_NAME') as TStringField;
          ForeignColumn:=Fields.Fields.FindField('PKEY_COLUMN_NAME') as TStringField;

          Fields.First;

          while not Fields.Eof do
          begin
            tmpLocal:=LocalColumn.AsString;

            tmpCol:=AData.Items.Find(tmpLocal);

            if tmpCol=nil then
              // ??
            else
              // "..|.." means database parent of tmpCol
              TDataAccess(tmpCol).IMasterOrigin:='..|..|'+tmpSchema+tmpTable+'|'+ForeignColumn.AsString;

            Fields.Next;
          end;

          Meta.Next;
        end;

        Meta.Close;
      finally
        Fields.Free;
      end;
    except
      on E:Exception do
      begin
        if not TBISourceAccess(Source).CallOnError(E.Message) then
           raise;
      end;
    end;
  finally
    Meta.Free;
  end;
end;
*)

class function TDBmicroOLAPEngine.GetKeyFieldNames(const AConnection:TCustomConnection; const ATable:String):TStrings;
begin
  result:=TStringList.Create;

//  if AConnection is TmySQLDatabase then
//     TmySQLDatabase(AConnection).GetKeyFieldNames('','',ATable,'',result);
end;

class function TDBmicroOLAPEngine.CloneConnection(const AConn:TCustomConnection): TCustomConnection;
begin
  result:=TmySQLDatabase(AConn.ClassType).Create(nil);
  result.Assign(AConn);
end;

function CanGetMetaData(const Connection:TCustomConnection):Boolean;
begin
  result:=Connection is TmySQLDatabase;
end;

class function TDBmicroOLAPEngine.GetSchemas(const AConnection: TCustomConnection): TStrings;
begin
  result:=TStringList.Create;

//  if CanGetMetaData(AConnection) then
//     TmySQLDatabase(AConnection).GetSchemaNames(TmySQLDatabase(AConnection).ConnectionName,'',result);
end;

class function TDBmicroOLAPEngine.GetTableNames(const AConnection:TCustomConnection):TStrings;
begin
  result:=TStringList.Create;

  if CanGetMetaData(AConnection) then
     TmySQLDatabase(AConnection).GetTableNames('',result,True);
end;

class function TDBmicroOLAPEngine.GetTable(const AConnection:TCustomConnection; const AName:String):TDataSet;
begin
  result:=CreateQuery(AConnection,'select * from '+AName);
end;

class function TDBmicroOLAPEngine.CreateQuery(const AConnection:TCustomConnection; const SQL:String):TDataSet;
var Query : TmySQLQuery;
begin
  Query:=TmySQLQuery.Create(AConnection.Owner);
  Query.Database:=AConnection as TmySQLDatabase;

  Query.SQL.Text:=SQL;
  Query.Open;

  result:=Query;
end;

class function TDBmicroOLAPEngine.DriverNames: TStringDynArray;
begin
  SetLength(result,1);
  result[0]:='MySQL';
end;

class function TDBmicroOLAPEngine.DriverToName(const ADriver:String):String;
begin
  if SameText(ADriver,'MySQL') then
     result:=ADriver
  else
     result:='';
end;

class function TDBmicroOLAPEngine.ImportFile(const Source:TBIDB; const AFileName:String):TDataArray;
begin
  result:=nil;
end;

class function TDBmicroOLAPEngine.GetConnectionName(const AConnection:TCustomConnection):String;
begin
  if AConnection is TmySQLDatabase then
     result:=TmySQLDatabase(AConnection).DatabaseName
  else
     result:=AConnection.Name;
end;

class function TDBmicroOLAPEngine.GetDriver(const AIndex: Integer): String;
begin
  result:='MySQL';
end;

class function TDBmicroOLAPEngine.CreateConnection(const Definition:TDataDefinition):TCustomConnection;
var C : TmySQLDatabase;
    tmpServer,
    tmpData : String;
begin
  C:=TmySQLDatabase.Create(nil);

  tmpServer:=Trim(Definition['DBServer']);

  C.Host:=tmpServer;

  tmpData:=Definition['DBDatabase'];

  if (tmpServer='') and TPath.IsRelativePath(tmpData) then
     tmpData:=TStore.FullPath(Definition.Store,tmpData);

  C.DatabaseName:=tmpData;
  C.UserName:=Definition['DBUser'];
  C.UserPassword:=TCrypto.Decrypt(Definition['DBPassword']);
  C.LoginPrompt:=Definition.AsBoolean('DBLogin');

  result:=C;
end;

class function TDBmicroOLAPEngine.CreateDataSet(const AOwner:TComponent; const AData:TDataItem):TDataSet;
begin
  result:=TmySQLTable.Create(AOwner);

  AddFields(result.FieldDefs,AData.Items.AsArray);

  TmySQLTable(result).CreateTable;
end;

class function TDBmicroOLAPEngine.Supports(const Extension:String):Boolean;
begin
  result:=False;
end;

class function TDBmicroOLAPEngine.Tester: TBIDBTesterClass;
begin
  result:=nil;
end;

initialization
  TBIDB.Engine:=TDBmicroOLAPEngine.Create;
finalization
  TBIDB.Engine:=nil;
end.
