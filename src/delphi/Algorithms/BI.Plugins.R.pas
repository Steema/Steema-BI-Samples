{*********************************************}
{  TeeBI Software Library                     }
{  Plugin for R language                      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Plugins.R;

interface

// Note:

// "R language" installer for Windows (32bit and 64bit) can be downloaded from:
// https://cran.r-project.org/bin/windows/base

// When compiling this unit in x64 bits, the R x64 bits version is used.

uses
  System.Classes, BI.Arrays, BI.Data, BI.Algorithm, BI.Algorithm.Model,
  System.SysUtils;

type
  TBIREngine=class(TBIPlugin)
  private
    class var
      FEngine : TBIREngine;

    class procedure SetEngine(const Value:TBIREngine); static;
  protected
    procedure AddPackage(const AOutput:TStrings; const APackage:String);
    function Finish:Boolean; virtual;
    procedure Start; virtual;
  public
    class var
      Output : TStrings;

    procedure AddVariable(const AName:String; const Index:TNativeIntArray;
                          const AData:TDataArray; const UseMissing:Boolean=True); overload; virtual; abstract;

    procedure AddVariable(const AName:String; const Index:TNativeIntArray;
                          const AData:TDataItem; const UseMissing:Boolean=True); overload;

    procedure GetVariable(const AName:String; const AData:TDataItem); virtual; abstract;
    procedure LoadPackage(const APackage:String); virtual; abstract;
    procedure ParseOutput(const ADest:TDataItem); virtual; abstract;
    procedure ParseRawMap(const AMap,ADest:TDataItem); virtual; abstract;
    procedure Statement(const AStatement:String); virtual; abstract;
    function Version:String; virtual; abstract;

    class property Engine:TBIREngine read FEngine write SetEngine;
  end;

  TRBaseAlgorithm=class(TBaseAlgorithm)
  protected
    procedure BuildScript; virtual; abstract;
    function R:TBIREngine;
  public
    procedure Calculate; override;
  end;

  TRSupervisedModel=class(TSupervisedModel)
  protected
    procedure BuildScript; virtual; abstract;
    function R:TBIREngine;
  public
    procedure Calculate; override;
    procedure Plot; virtual;
  end;

implementation
