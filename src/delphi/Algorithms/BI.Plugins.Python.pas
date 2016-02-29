{*********************************************}
{  TeeBI Software Library                     }
{  Plugin for Python language                 }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Plugins.Python;

interface

uses
  System.Classes, BI.Plugins.Python.Engine, BI.Algorithm.Model, BI.Arrays,
  BI.Data;

type
  TBIPython=class(TBIPlugin)
  private
    class var FEngine : TPythonEngine;
    class var FOutput : TStrings;

    class var PyNDArray_Type : PPyObject;

    class function GetEngine:TPythonEngine; static;
  public
    class property Engine:TPythonEngine read GetEngine;
    class property Output:TStrings read FOutput write FOutput;

    class function DatasRowToVector(const AIndex:TInteger; const ADatas:TDataArray;
                      const UseMissing:Boolean=True):String; static;

    class function Evaluate(const AText:String):PPyObject; static;
    class function EvaluateAsString(const AText:String):String; static;

    class function Execute(const AScript:TStrings):Boolean; overload; static;
    class function Execute(const AText:String):Boolean; overload; static;

    class function FromData(const AName:String; const ADatas:TDataArray; const Index:TNativeIntArray=nil;
                      const UseMissing:Boolean=True):String; overload; static;

    class function FromData(const AName:String; const AData:TDataItem; const Index:TNativeIntArray=nil;
                      const UseMissing:Boolean=True):String; overload; static;

    // Pending:
    // class function FromData(...) : Int64;  <--- return pointer address

    class function GetData(const AName:String):TDataItem; static;
  end;

  // Base class for Scikit-learn algorithms
  TScikitSupervisedModel=class(TSupervisedModel)
  protected
    function BuildScript:TStrings; virtual; abstract;
    function Fit:Boolean;
  end;

implementation
