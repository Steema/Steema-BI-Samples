{*********************************************}
{  TeeBI Software Library                     }
{  Regression / Correlation                   }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm.Regression;

interface

uses
  BI.Data, BI.Arrays, BI.Plugins.R, BI.Algorithm.Model, System.Classes;

type
  TRegression=class
  end;

  TLinearRegression=class(TRegression)
  public
    Coefficient,
    M,B : TFloat;

    SumX,
    SumX2,

    SumY,
    SumY2,

    SumXY : TFloat;

    Count : TInteger;

    procedure Calculate(const X,Y:TDataItem);
    function MeanSquaredError(const X,Y:TDataItem):TFloat;
  end;

  TRegressionMatrix=class
  public
    class function Calculate(const Data:TDataItem):TSquareGrid;
  end;

  TBISupportVector=class(TRSupervisedModel)
  protected
    procedure BuildScript; override;
  public
    procedure Calculate; override;

    // property Cost
    // property Gamma
    // property Kernel
  end;

implementation
