{*********************************************}
{  TeeBI Software Library                     }
{  PCA Dimensionality Reduction algorithm     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm.DimReduction;

interface

uses
  BI.Algorithm.Model, BI.Plugins.R,
  System.Classes, BI.Arrays;

type
  TBIPCA=class(TRSupervisedModel)
  protected
    procedure BuildScript; override;
  end;

implementation
