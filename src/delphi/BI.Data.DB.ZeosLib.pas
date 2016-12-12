{*********************************************}
{  TeeBI Software Library                     }
{  ZeosLib Database data import and export    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.DB.ZeosLib;

(*
  ZeosLib, database drivers for Delphi

  https://sourceforge.net/projects/zeoslib
  http://zeoslib.sourceforge.net/index.php


*)

interface

uses
  System.Classes, System.Types, Data.DB, BI.Data, BI.Data.DB, BI.DataSource,
  BI.Persist, ZClasses, ZDbcIntfs, ZCompatibility, ZDbcConnection,
  ZAbstractConnection;

type
  TDbcConnection=ZDbcConnection.TZAbstractConnection;
  TZAbstractConnectionClass=class of TDbcConnection;

  TBIZeosLib=class(TBIItemsSource)
  private
    class procedure Append(const AStrings:TStrings;
                           const AResultSet:IZResultSet;
                           const AName:String); static;

    class function ConnectionClass(const ADriver:String):TZAbstractConnectionClass; static;
    class function CreateConnection(const Definition:TDataDefinition):TDbcConnection; static;

    class function CreateQuery(const AConnection:TZAbstractConnection; const SQL:String):TDataSet; static;

    class function GetConnectionName(const AConnection:TDbcConnection):String; static;

    class function GetKeyFieldNames(const AConnection:TDbcConnection;
                                    const ATable:String):TStrings; static;

    class function GetSchemas(const AConnection: TDbcConnection): TStrings; static;

    class function GetItemNames(const AConnection:TDbcConnection;
                                const IncludeSystem,IncludeViews:Boolean):TStrings;

    class function GetTable(const AConnection:TZAbstractConnection; const AName:String):TDataSet; static;

    class function QueryTable(const AConnection:TDbcConnection; const ATable:String):IZResultSet; static;
  public
    class function DriverNames: TStringDynArray; static;
    class function FileFilter: TFileFilters; static;

    class function Import(const AConnection:TDbcConnection;
                          const IncludeViews:Boolean=False):TDataItem; overload; static;

    class function Import(const AFileName,AProtocol:String;
                          const IncludeViews:Boolean=False):TDataItem; overload; static;
    class function Import(const AResultSet:IZResultSet):TDataItem; overload; static;

    class function Supports(const Extension:String):Boolean; static;
  end;

implementation
