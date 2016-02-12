{*********************************************}
{  TeeBI Software Library                     }
{  Plugin for opaR native Delphi R language   }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Plugins.R.opaR;

// Note: When using this unit, TeeBI automatically will plug with
//       the "opaR" library to pass/get data to R scripts.

{
  https://github.com/SigmaSciences/opaR

  opaR (object pascal for R) is a port of R.NET 1.6.5 to Embarcadero Delphi,
  allowing you to integrate the popular R statistical language into your Delphi apps.
}

// "R language" installer for Windows (32bit and 64bit) can be downloaded from:

// https://cran.r-project.org/bin/windows/base

interface

uses
  System.Classes, BI.Arrays, BI.Data, BI.Plugins.R,
  opaR.Interfaces, opaR.Engine, opaR.Devices.NullCharacterDevice, opaR.Utils;

type
  TopaR=class(TBIREngine)
  private
    function CreateArray<T>(const Index:TInt64Array; const Value:TArray<T>):TArray<T>;
  protected
    function Finish:Boolean; override;
  public
    Constructor Create;

    procedure AddVariable(const AName:String; const Index:TInt64Array;
                          const ADatas:TDataArray; const UseMissing:Boolean=True); override;

    procedure GetVariable(const AName:String; const AData:TDataItem); override;
    procedure LoadPackage(const APackage:String); override;
    procedure ParseOutput(const ADest:TDataItem); override;
    procedure ParseRawMap(const AMap,ADest:TDataItem); override;
    procedure Statement(const AStatement:String); override;
    function Version:String; override;
  end;

implementation
