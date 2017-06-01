{*********************************************}
{  TeeBI Software Library                     }
{  Using TeeChart "Functions" as BI providers }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.ChartFunctions;
{.$DEFINE FMX}

interface

uses
  System.Classes,

  BI.DataItem,

  {$IFDEF FMX}
  FMXTee.Constants, FMXTee.Engine,
  {$ELSE}
  VCLTee.TeeConst, VCLTee.TeEngine,
  {$ENDIF}

  {$IFDEF FPC}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ELSE}

  {$IF TeeMsg_TeeChartPalette='TeeChart'}
  {$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
  {$ENDIF}
  {$ENDIF}


  BI.Algorithm;

type
  TDataFunction=class(TDataProviderNeeds)
  private
    FFunction : TTeeFunction;
    FFunctionClass : TTeeFunctionClass;

    procedure AddNeeds(const AFunctionClass:TTeeFunctionClass);
    function CalcClass:TTeeFunctionClass;
    function GetPeriod: Double;
    function GetTeeFunction: TTeeFunction;
    procedure SetPeriod(const Value: Double);
  protected
    function GetChildOwner: TComponent; override;
    Procedure GetChildren(Proc:TGetChildProc; Root:TComponent); override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure Loaded; override;
  public
    Constructor CreateFunction(const AOwner:TComponent; const AFunctionClass:TTeeFunctionClass); overload;
    Constructor CreateFunction(const AOwner:TComponent; const AFunction:TTeeFunction); overload;

    function FunctionTitle:String;
    function IsFinancial:Boolean;

    property FunctionClass:TTeeFunctionClass read CalcClass;
    property TeeFunction:TTeeFunction read GetTeeFunction;
  published
    property Period:Double read GetPeriod write SetPeriod stored False;
  end;

implementation
