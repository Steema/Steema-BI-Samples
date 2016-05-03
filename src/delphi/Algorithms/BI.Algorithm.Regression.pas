{*********************************************}
{  TeeBI Software Library                     }
{  Regression / Correlation                   }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm.Regression;

interface

uses
  System.Classes,
  BI.Data, BI.Arrays, BI.Plugins.R, BI.Algorithm;

type
  TRegression=class(TBaseAlgorithm)
  end;

  TLinearRegression=class(TRegression)
  private
    function GetX: TDataItem; inline;
    function GetY: TDataItem; inline;
    procedure SetX(const Value: TDataItem); inline;
    procedure SetY(const Value: TDataItem); inline;
  public
    // Output
    Coefficient,
    M,B : TFloat;

    SumX,
    SumX2,

    SumY,
    SumY2,

    SumXY : TFloat;

    Count : TInteger;

    Constructor Create(AOwner:TComponent); override;

    procedure Calculate; override;
    function MeanSquaredError(const X,Y:TDataItem):TFloat;

    // Input data
    property X:TDataItem read GetX write SetX;
    property Y:TDataItem read GetY write SetY;
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
