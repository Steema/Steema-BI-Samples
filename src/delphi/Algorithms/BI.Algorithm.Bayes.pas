{*********************************************}
{  TeeBI Software Library                     }
{  Bayesian algorithms                        }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Algorithm.Bayes;

interface

uses
  BI.Plugins.R;

type
  TBINaiveBayes=class(TRSupervisedModel)
  protected
    procedure BuildScript; override;
  public
    // property Laplace
  end;

implementation
