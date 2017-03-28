{*********************************************}
{  TeeBI Software Library                     }
{  FireDAC Database data import and export    }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.DB.FireDAC;

interface

(*
  Just include this unit in the "uses" of any unit, to enable FireDAC as the
  "Engine" for all TeeBI database operations.

  Note: Better to not include BI.Data.DB.ZeosLib or any other unit that
        also provides a different "Engine".

  When using multiple engines (optionally),
  the current one must be manually selected like:

  uses BI.Data.DB;
    TBIDB.Engine:=TDBFireDACEngine.Create; <--- the desired engine class

*)


uses
  System.Classes, System.Types, Data.DB, BI.Data, BI.Data.DB, BI.DataSource,
  BI.Persist, FireDAC.Comp.Client;

type
  TDBFireDACEngine=class(TBIDBEngine)
  private
    class procedure SetQueryParameters(const Query:TFDQuery); static;
  public
    class function CloneConnection(const AConn:TCustomConnection): TCustomConnection; override;
    class function CreateConnection(const Definition:TDataDefinition):TCustomConnection; override;
    class function CreateDataSet(const AOwner:TComponent; const AData:TDataItem):TDataSet; override;
    class function CreateQuery(const AConnection:TCustomConnection; const SQL:String):TDataSet; override;
    class function DriverNames:TStringDynArray; override;
    class function DriverToName(const ADriver:String):String; override;
    class function FileFilter: TFileFilters; override;
    class function GetConnectionName(const AConnection:TCustomConnection):String; override;
    class function GetDriver(const AIndex:Integer):String; override;
    class function GetKeyFieldNames(const AConnection:TCustomConnection; const ATable:String):TStrings; override;
    class function GetSchemas(const AConnection:TCustomConnection):TStrings;
    class function GetTable(const AConnection:TCustomConnection; const AName:String):TDataSet; override;
    class function GetItemNames(const AConnection:TCustomConnection;
                                const IncludeSystem,IncludeViews:Boolean):TStrings; override;
    class procedure GuessForeignKeys(const AName:String; const Table:TDataSet; const AData:TDataItem; const Source:TBISource); override;
    class function ImportFile(const Source:TBIDB; const AFileName:String):TDataArray; override;
    class procedure StartParallel; override;
    class function Supports(const Extension:String):Boolean; override;
    class function Tester:TBIDBTesterClass; override;
  end;

implementation
