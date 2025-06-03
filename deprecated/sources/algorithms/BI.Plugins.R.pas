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
  System.Classes, BI.Arrays, BI.DataItem, BI.Algorithm, BI.Algorithm.Model,
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
    function R:TBIREngine; inline;
  public
    procedure Calculate; override;
    procedure Plot; virtual;
  end;

  TRSupervisedModel=class(TSupervisedModel)
  protected
    procedure BuildScript; virtual; abstract;
    function R:TBIREngine; inline;
  public
    procedure Calculate; override;
    procedure Plot; virtual;
  end;

implementation

{ TRModel }

function TRBaseAlgorithm.R: TBIREngine;
begin
  result:=TBIREngine.Engine;
end;

procedure TRBaseAlgorithm.Calculate;

  procedure DoError;
  begin
    raise EBIException.Create('Error: R plugin not configured. Use BI.Plugins.R.opaR or BI.Plugins.R.Command units');
  end;

begin
  if R=nil then
     DoError;

  inherited;

  R.Start;
  BuildScript;
  R.Finish;
end;

procedure TRBaseAlgorithm.Plot;
begin
  R.Statement('plot(out)');
end;

{ TRSupervisedModel }

function TRSupervisedModel.R: TBIREngine;
begin
  result:=TBIREngine.Engine;
end;

procedure TRSupervisedModel.Calculate;

  procedure DoError;
  begin
    raise EBIException.Create('Error: R plugin not configured. Use BI.Plugins.R.opaR or BI.Plugins.R.Command units');
  end;

begin
  if R=nil then
     DoError;

  inherited;

  R.Start;
  BuildScript;
  R.Finish;
end;

procedure TRSupervisedModel.Plot;
begin
  R.Statement('plot(out)');
end;

{ TBIREngine }

procedure TBIREngine.AddPackage(const AOutput: TStrings;
  const APackage: String);
begin
  AOutput.Add('if(!is.element('''+APackage+''', installed.packages()[,1]))');
  AOutput.Add('{');
  AOutput.Add('  options(repos = c(CRAN = "http://cran.uk.r-project.org/"))');
  AOutput.Add('  install.packages('''+APackage+''', dependencies = TRUE)');
  AOutput.Add('}');

  AOutput.Add('library('+APackage+')');
end;

procedure TBIREngine.AddVariable(const AName: String; const Index: TNativeIntArray;
  const AData: TDataItem; const UseMissing: Boolean);
var tmp : TDataArray;
begin
  SetLength(tmp,1);
  tmp[0]:=AData;

  AddVariable(AName,Index,tmp,UseMissing);
end;

function TBIREngine.Finish:Boolean;
begin
  result:=False;
end;

class procedure TBIREngine.SetEngine(const Value: TBIREngine);
begin
  FEngine.Free;
  FEngine:=Value;
end;

procedure TBIREngine.Start;
begin // Nothing
end;

initialization
finalization
  TBIREngine.Output:=nil;
  TBIREngine.Engine:=nil;
end.
